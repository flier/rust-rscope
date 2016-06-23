use std::env;
use std::path::PathBuf;

use rustbox::{self, Key, Event, RustBox};

use rustyline::Editor;

use errors::Result;
use term::{Drawable, Canvas, Layout, Align, Fill, Widget, button, input, pannel, space, align_to,
           fill_up};

const HISTORY_FILE: &'static str = ".rscope_history";

pub trait UI {
    fn close(&mut self) -> Result<()>;

    fn run(&mut self) -> Result<()>;
}

pub struct TermUI<'a> {
    rb: RustBox,
    widgets: Widget<'a>,
}

impl<'a> TermUI<'a> {
    pub fn new() -> Result<Box<UI>> {
        let search = input(Some("search"));

        let toolbar = pannel(Some("toolbar"),
                             vec![button("Symbol", Key::F(1)),
                                  space(1),
                                  button("Global", Key::F(2)),
                                  space(1),
                                  button("Func Called", Key::F(3)),
                                  space(1),
                                  button("Func Calling", Key::F(4)),
                                  space(1),
                                  button("Text", Key::F(5)),
                                  space(1),
                                  button("File", Key::F(6)),
                                  space(1),
                                  button("Include", Key::F(7)),
                                  space(1),
                                  button("Assign To", Key::F(8)),
                                  space(1),
                                  button("Quit", Key::F(9))]);

        Ok(Box::new(TermUI {
            rb: try!(RustBox::init(rustbox::InitOptions {
                input_mode: rustbox::InputMode::Esc,
                buffer_stderr: true,
            })),
            widgets: align_to(pannel(None,
                                     vec![fill_up(search, Fill::Width),
                                          align_to(toolbar, Align::Bottom)]),
                              Align::Bottom),
        }))
    }

    fn init(&mut self) {
        self.rb.clear();

        self.widgets.layout(self.rb.rect()).draw(&self.rb);

        self.rb.present();
    }

    fn draw(&self) {
        self.rb.present();
    }
}

impl<'a> UI for TermUI<'a> {
    fn close(&mut self) -> Result<()> {
        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        info!("term UI started");

        self.init();

        loop {
            self.draw();

            match try!(self.rb.poll_event(false)) {
                Event::KeyEvent(key) => {
                    match key {
                        Key::F(9) | Key::Ctrl('c') => {
                            break;
                        }
                        _ => {
                            debug!("drop {:?} key", key);
                        }
                    }
                }
                Event::ResizeEvent(w, h) => {
                    debug!("resize to {{{}, {}}}", w, h);

                    self.init();
                }
                evt @ _ => {
                    debug!("drop {:?} event", evt);
                }
            }
        }

        Ok(())
    }
}

pub struct LineUI<'a> {
    rl: Editor<'a>,
}

impl<'a> LineUI<'a> {
    pub fn new() -> Result<Box<UI>> {
        let mut rl = Editor::new();

        try!(load_history(&mut rl));

        Ok(Box::new(LineUI { rl: rl }))
    }
}

impl<'a> UI for LineUI<'a> {
    fn close(&mut self) -> Result<()> {
        if let Err(err) = save_history(&self.rl) {
            warn!("fail to save history, {}", err)
        }

        Ok(())
    }

    fn run(&mut self) -> Result<()> {
        info!("line UI started");

        Ok(())
    }
}

fn find_history() -> Option<PathBuf> {
    if let Ok(dir) = env::current_dir() {
        let mut p = dir;

        p.push(HISTORY_FILE);

        if p.exists() && p.is_file() {
            return Some(p);
        }
    }

    if let Some(dir) = env::home_dir() {
        let mut p = dir;

        p.push(HISTORY_FILE);

        if p.exists() && p.is_file() {
            return Some(p);
        }
    }

    None
}

fn load_history<'a>(rl: &mut Editor<'a>) -> Result<Option<PathBuf>> {
    if let Some(p) = find_history() {
        info!("loading history from {}", p.to_str().unwrap());

        try!(rl.load_history(&p));

        Ok(Some(p))
    } else {
        Ok(None)
    }
}

fn save_history<'a>(rl: &Editor<'a>) -> Result<Option<PathBuf>> {
    if let Ok(cur_dir) = env::current_dir() {
        let mut p = cur_dir;

        p.push(HISTORY_FILE);

        info!("saving history to {}", p.to_str().unwrap());

        try!(rl.save_history(&p));

        Ok(Some(p))
    } else {
        Ok(None)
    }
}
