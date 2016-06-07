#[macro_use]
extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate num_cpus;
extern crate cargo;
extern crate syntex_syntax;

use std::env;
use std::process;
use std::thread;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::path::{Path, PathBuf};

use cargo::util::Config;
use cargo::core::{Target, TargetKind};

mod errors;
mod loader;
mod parser;
mod gen;

use errors::Result;

#[derive(Debug)]
struct AppConf {
    jobs: usize,
    dirs: Vec<PathBuf>,
}

impl AppConf {
    fn load_targets(&self, cargo_conf: &Config) -> Result<Vec<(PathBuf, Target)>> {
        let mut targets = Vec::new();

        for dir in &self.dirs {
            let (package, packages) = try!(loader::load_crate(&dir, cargo_conf));

            targets.extend(package.targets()
                .iter()
                .map(|target| (PathBuf::from(package.root()), target.clone())));

            for dep in package.dependencies() {
                debug!("found dependency; name={}, version={}, locked={}",
                       dep.name(),
                       dep.version_req(),
                       dep.specified_req().unwrap_or("N/A"));
            }

            for package_id in packages.package_ids() {
                let package = try!(packages.get(package_id));

                info!("resolved package; {}, root={}",
                      package,
                      package.root().to_str().unwrap());

                for target in package.targets() {
                    if target.is_lib() {
                        targets.push((PathBuf::from(package.root()), target.clone()))
                    }
                }
            }
        }

        Ok(targets)
    }

    fn start_parsers(&self,
                     targets: Arc<Mutex<Vec<(PathBuf, Target)>>>)
                     -> mpsc::Receiver<parser::SourceFile> {
        let (tx, rx) = mpsc::channel();

        for i in 0..self.jobs {
            let targets = targets.clone();
            let tx = tx.clone();

            thread::Builder::new()
                .name(format!("parser-{}", i))
                .spawn(move || {
                    loop {
                        if let Some((ref base_dir, ref target)) = match targets.lock() {
                                Ok(guard) => guard,
                                Err(poisoned) => poisoned.into_inner(),
                            }
                            .pop() {
                            debug!("parsing target; name={}, kind={}, src_path={}",
                                   target.name(),
                                   match *target.kind() {
                                       TargetKind::Lib(_) => "lib",
                                       TargetKind::CustomBuild => "build",
                                       TargetKind::Bench => "bench",
                                       TargetKind::Test => "test",
                                       TargetKind::Example => "example",
                                       TargetKind::Bin => "bin",
                                   },
                                   target.src_path().to_str().unwrap());

                            match parser::extract_symbols(base_dir, target) {
                                Ok(source_files) => {
                                    for source_file in source_files {
                                        tx.send(source_file).unwrap();
                                    }
                                }
                                Err(err) => {
                                    warn!("parse target failed, {}", err);
                                }
                            }
                        } else {
                            break;
                        }
                    }

                    drop(tx);
                })
                .expect("fail to start parser");
        }

        drop(tx);

        rx
    }
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage: {} [options] DIR [..DIR]", program);

    println!("{}", opts.usage(&brief));
}

fn parse_args(program: &str, args: &[String]) -> Result<AppConf> {
    let mut opts = getopts::Options::new();

    opts.optflag("h", "help", "Print this help menu");

    let matches = match opts.parse(args) {
        Ok(m) => m,
        Err(err) => {
            println!("Error: {}\n", err);

            print_usage(program, opts);

            process::exit(-1);
        }
    };

    if matches.opt_present("h") {
        print_usage(&program, opts);

        process::exit(0);
    }

    Ok(AppConf {
        jobs: num_cpus::get(),

        dirs: if matches.free.is_empty() {
            vec![env::current_dir().unwrap()]
        } else {
            matches.free
                .iter()
                .map(|s| {
                    let p = PathBuf::from(s);

                    if p.is_absolute() {
                        p
                    } else {
                        let mut cwd = env::current_dir().unwrap();

                        cwd.push(p);

                        cwd.canonicalize().unwrap()
                    }
                })
                .collect()
        },
    })
}

fn main() {
    env_logger::init().unwrap();

    let args = env::args().collect::<Vec<String>>();
    let program = Path::new(&args[0]).file_name().unwrap().to_str().unwrap();

    let app_conf = parse_args(program, &args[1..]).expect("fail to parse arguments");

    debug!("parsed options: {:?}", app_conf);

    let cargo_conf = Config::default().expect("fail to initial cargo");

    let targets = Arc::new(Mutex::new(app_conf.load_targets(&cargo_conf)
        .expect("fail to load targets")));

    let rx = app_conf.start_parsers(targets);

    let mut files: Vec<parser::SourceFile> = rx.iter().collect();

    files.sort();

    for file in files {
        debug!("received symbols of file {} in {} lines",
               file.path.to_str().unwrap(),
               file.lines.len());
    }
}
