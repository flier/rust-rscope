#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate lazy_static;
extern crate getopts;
extern crate num_cpus;
extern crate memmap;
#[macro_use]
extern crate nom;
extern crate string_cache;
extern crate cargo;
extern crate syntex_syntax;
extern crate rustbox;
extern crate rustyline;

use std::env;
use std::process;
use std::thread;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::time::Instant;
use std::ops::DerefMut;
use std::path::{Path, PathBuf};

use cargo::util::Config;
use memmap::{Mmap, Protection};

#[macro_use]
mod errors;
mod loader;
mod symbol;
mod parser;
mod digraph;
mod crossref;
mod invlib;
mod gen;
mod themes;
mod ui;

use errors::Result;

const APP_VERSION: &'static str = "1.0";

#[derive(Debug)]
struct AppConf {
    build_only: bool,
    caseless: bool,
    skip_compress: bool,
    skip_update: bool,
    line_mode: bool,
    short_match: bool,

    jobs: usize,
    dirs: Vec<PathBuf>,
}

impl AppConf {
    fn start_parsers(&self,
                     targets: Arc<Mutex<Vec<loader::Task>>>)
                     -> mpsc::Receiver<parser::SourceFile> {
        let (tx, rx) = mpsc::channel();

        for i in 0..self.jobs {
            let targets = targets.clone();
            let tx = tx.clone();

            thread::Builder::new()
                .name(format!("parser-{}", i))
                .spawn(move || {
                    loop {
                        if let Some(ref task) = {
                            match targets.lock() {
                                    Ok(guard) => guard,
                                    Err(poisoned) => poisoned.into_inner(),
                                }
                                .pop()
                        } {
                            debug!("parsing {:?}", task);

                            let now = Instant::now();

                            match parser::extract_symbols(&task.src_path()) {
                                Ok(source_files) => {
                                    info!("parsed `{}` target with {} source files in {:.2}ms",
                                          task.target.name(),
                                          source_files.len(),
                                          now.elapsed().as_secs() as f64 * 1000.0 +
                                          now.elapsed().subsec_nanos() as f64 / 1000.0 / 1000.0);

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

    opts.optflag("b", "build", "Build the cross-reference only.");
    opts.optflag("C", "caseless", "Ignore letter case when searching.");
    opts.optflag("c",
                 "no-compress",
                 "Use only ASCII characters in the cross-ref file. (don't compress)");
    opts.optflag("d", "skip-update", "Do not update the cross-reference.");
    opts.optflag("l", "line-mode", "Line-oriented interface.");
    opts.optflag("T",
                 "short-match",
                 "Use only the first eight characters to match against C symbols.");
    opts.optflag("V", "version", "Print the version number.");

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

    if matches.opt_present("V") {
        println!("{}: version {}", program, APP_VERSION);

        process::exit(0);
    }

    Ok(AppConf {
        build_only: matches.opt_present("b"),
        caseless: matches.opt_present("C"),
        skip_compress: matches.opt_present("c"),
        skip_update: matches.opt_present("d"),
        line_mode: matches.opt_present("l"),
        short_match: matches.opt_present("T"),

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

    if let Ok(cur_dir) = env::current_dir() {
        let mut p = cur_dir;

        p.push("cscope.out");

        if p.exists() && p.is_file() {
            let mm = Mmap::open_path(p.as_path(), Protection::Read).unwrap();

            match crossref::parse(unsafe { mm.as_slice() }) {
                nom::IResult::Done(_, cf) => {
                    info!("Load {} source files from {}",
                          cf.files.len(),
                          p.to_str().unwrap());
                }
                nom::IResult::Error(ref err) => warn!("Fail to parse crossref file, {}", err),
                nom::IResult::Incomplete(needed) => {
                    warn!("Incomplete crossref file, need more{} data",
                          if let nom::Needed::Size(size) = needed {
                              format!(" ({} bytes)", size)
                          } else {
                              String::new()
                          });
                }
            }
        }
    }

    if !app_conf.skip_update {
        let cargo_conf = Config::default().expect("fail to initial cargo");

        let targets = Arc::new(Mutex::new(app_conf.dirs
            .iter()
            .map(|ref dir| {
                loader::load_crate(dir, &cargo_conf)
                    .expect(&format!("fail to load crate from {}", dir.to_str().unwrap()))
            })
            .flat_map(|(ref pkg, ref deps)| {
                loader::find_targets(pkg, deps)
                    .expect(&format!("fail to find targets from {}", pkg))
            })
            .collect()));

        let rx = app_conf.start_parsers(targets);

        let mut files: Vec<parser::SourceFile> = rx.iter().collect();

        files.sort();

        for file in files {
            debug!("received symbols of file {} in {} lines",
                   file.path.to_str().unwrap(),
                   file.lines.len());
        }
    }

    if !app_conf.build_only {
        match if app_conf.line_mode {
            ui::LineUI::new()
        } else {
            ui::TermUI::new()
        } {
            Ok(mut ui) => {
                if let Err(err) = ui.run() {
                    warn!("run UI fail")
                }

                ui.close();
            }

            Err(err) => {
                println!("ERROR: {}", err);
            }
        }
    }
}
