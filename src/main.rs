#[macro_use]
extern crate log;
extern crate env_logger;
extern crate getopts;
extern crate num_cpus;
extern crate cargo;

use std::env;
use std::cmp;
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

#[derive(PartialEq, Eq, PartialOrd)]
#[repr(u8)]
enum Symbol {
    /// non-symbol text
    Nop,
    /// function definition
    FuncDef = b'$',
    /// function call
    FuncCall = b'`',
    /// function end
    FuncEnd = b'}',
    /// #define
    Define = b'#',
    /// #define end
    DefineEnd = b')',
    /// #include
    Include = b'~',
    /// direct assignment, increment, or decrement
    Assignment = b'=',
    /// enum/struct/union definition end
    DeclEnd = b';',
    /// class definition
    ClassDef = b'c',
    /// enum definition
    EnumDef = b'e',
    /// other global definition
    GlobalDef = b'g',
    /// function/block local definition
    LocalDef = b'l',
    /// global enum/struct/union member definition
    GlobalDecl = b'm',
    /// function parameter definition
    FuncParam = b'p',
    /// struct definition
    StructDef = b's',
    /// typedef definition
    Typedef = b't',
    /// union definition
    UnionDef = b'u',
}

#[derive(PartialEq, Eq, PartialOrd)]
struct SourceLine {
    line_num: usize,
    symbols: Vec<Symbol>,
}

impl cmp::Ord for SourceLine {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.line_num.cmp(&other.line_num)
    }
}

#[derive(PartialEq, Eq, PartialOrd)]
struct SourceFile {
    path: PathBuf,
    lines: Vec<SourceLine>,
}

impl cmp::Ord for SourceFile {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.path.cmp(&other.path)
    }
}

#[derive(Debug)]
struct AppConf {
    jobs: usize,
    dirs: Vec<PathBuf>,
}

impl AppConf {
    fn load_targets(&self, cargo_conf: &Config) -> Vec<Target> {
        let mut targets = Vec::new();

        for dir in &self.dirs {
            let mut p = PathBuf::from(dir);

            p.push("Cargo.toml");

            let (package, packages) = loader::load(&p, cargo_conf).unwrap();

            targets.append(&mut Vec::from(package.targets()));

            for dep in package.dependencies() {
                debug!("found dependency; name={}, version={}, locked={}",
                       dep.name(),
                       dep.version_req(),
                       dep.specified_req().unwrap_or("N/A"));
            }

            for pkg in packages.package_ids() {
                let pkg = packages.get(pkg).unwrap();

                info!("resolved package; {}, root={}",
                      pkg,
                      pkg.root().to_str().unwrap());

                for target in pkg.targets() {
                    if target.is_lib() {
                        targets.push(target.clone())
                    }
                }
            }
        }

        targets
    }

    fn start_parsers(&self, targets: Arc<Mutex<Vec<Target>>>) -> mpsc::Receiver<SourceFile> {
        let (tx, rx) = mpsc::channel();

        for i in 0..self.jobs {
            let targets = targets.clone();
            let tx = tx.clone();

            thread::Builder::new()
                .name(format!("parser-{}", i))
                .spawn(move || {
                    loop {
                        if let Some(target) = targets.lock().unwrap().pop() {
                            debug!("loading target; name={}, kind={}, src_path={}",
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

                            let mut lines = Vec::new();

                            lines.sort();

                            tx.send(SourceFile {
                                    path: PathBuf::from(target.src_path()),
                                    lines: lines,
                                })
                                .unwrap()
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

    let targets = Arc::new(Mutex::new(app_conf.load_targets(&cargo_conf)));

    let rx = app_conf.start_parsers(targets);

    let mut files: Vec<SourceFile> = rx.iter().collect();

    files.sort();

    for file in files {
        debug!("received symbols of file {} in {} lines",
               file.path.to_str().unwrap(),
               file.lines.len());
    }
}
