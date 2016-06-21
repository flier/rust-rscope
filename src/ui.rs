use std::env;
use std::fmt;
use std::convert::From;
use std::ops::{Add, AddAssign, Sub, SubAssign, Neg, Deref, DerefMut};
use std::path::PathBuf;

use rustbox::{self, Key, Event, RustBox};

use rustyline::Editor;
use rustyline::completion::Completer;

use string_cache::Atom;

use errors::Result;
use themes::{self, Theme, Themed, themed};

const HISTORY_FILE: &'static str = ".rscope_history";

pub trait UI {
    fn close(&mut self) -> Result<()>;

    fn run(&mut self) -> Result<()>;
}

pub trait Drawable {
    fn draw(&self, rb: &RustBox) -> Option<Size>;
}

pub trait HasContent {
    fn size(&self) -> Size;
}

pub trait AutoSizeable {
    fn resize(&self, rb: &RustBox);
}

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
struct Pos {
    x: isize,
    y: isize,
}

fn position(x: isize, y: isize) -> Pos {
    Pos { x: x, y: y }
}

trait HasPosition {
    fn position(&self) -> &Option<Pos>;
}

impl HasPosition for Option<Pos> {
    fn position(&self) -> &Option<Pos> {
        self
    }
}

impl Add<Distance> for Pos {
    type Output = Self;

    fn add(self, rhs: Distance) -> Self::Output {
        Pos {
            x: self.x + rhs.cols,
            y: self.y + rhs.lines,
        }
    }
}

impl AddAssign<Distance> for Pos {
    fn add_assign(&mut self, rhs: Distance) {
        self.x += rhs.cols;
        self.y += rhs.lines;
    }
}

impl Sub<Distance> for Pos {
    type Output = Self;

    fn sub(self, rhs: Distance) -> Self::Output {
        Pos {
            x: self.x - rhs.cols,
            y: self.y - rhs.lines,
        }
    }
}

impl SubAssign<Distance> for Pos {
    fn sub_assign(&mut self, rhs: Distance) {
        self.x -= rhs.cols;
        self.y -= rhs.lines;
    }
}

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
struct Size {
    w: usize,
    h: usize,
}

fn size(w: usize, h: usize) -> Size {
    Size { w: w, h: h }
}

impl Add<Distance> for Size {
    type Output = Self;

    fn add(self, rhs: Distance) -> Self::Output {
        Size {
            w: (self.w as isize + rhs.cols) as usize,
            h: (self.h as isize + rhs.lines) as usize,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
struct Distance {
    cols: isize,
    lines: isize,
}

fn width(w: usize) -> Distance {
    Distance {
        cols: w as isize,
        lines: 0,
    }
}

fn height(h: usize) -> Distance {
    Distance {
        cols: 0,
        lines: h as isize,
    }
}

impl Neg for Distance {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Distance {
            cols: -self.cols,
            lines: -self.lines,
        }
    }
}

pub trait DrawExt {
    fn draw(&self, pos: &Pos, theme: &Theme, s: &str) -> Size;
}

impl DrawExt for RustBox {
    fn draw(&self, pos: &Pos, theme: &Theme, s: &str) -> Size {
        self.print(pos.x as usize,
                   pos.y as usize,
                   theme.style,
                   theme.fg,
                   theme.bg,
                   s);

        size(s.len(), 1)
    }
}

type Label = Atom;
type ThemedLabel<'a> = Themed<'a, Label>;

trait HasLabel<'a> {
    fn label(&self) -> &ThemedLabel<'a>;
}

#[derive(Debug)]
struct Labeled<'a, T>(T, ThemedLabel<'a>);

fn labeled<'a, T>(value: T, label: ThemedLabel<'a>) -> Labeled<'a, T> {
    Labeled(value, label)
}

impl<'a, T> Deref for Labeled<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> DerefMut for Labeled<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a, T> HasLabel<'a> for Labeled<'a, T> {
    fn label(&self) -> &ThemedLabel<'a> {
        &self.1
    }
}

type ThemedKey<'a> = Themed<'a, Key>;

trait HasShortcut<'a> {
    fn key(&self) -> &ThemedKey<'a>;
}

#[derive(Debug)]
struct Shortcut<'a, T>(T, ThemedKey<'a>);

fn shortcuted<'a, T>(value: T, key: ThemedKey<'a>) -> Shortcut<'a, T> {
    Shortcut(value, key)
}

impl<'a, T> Deref for Shortcut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> DerefMut for Shortcut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a, T> HasShortcut<'a> for Shortcut<'a, T> {
    fn key(&self) -> &ThemedKey<'a> {
        &self.1
    }
}

#[derive(Debug)]
enum Widget<'a> {
    Button(Shortcut<'a, Labeled<'a, Option<Pos>>>),

    Pannel(Labeled<'a, Option<Pos>>, Vec<Box<Widget<'a>>>),
}

fn button<'a>(label: &str, shortcut: Key) -> Widget<'a> {
    Widget::Button(shortcuted(labeled(None, themed(Atom::from(label), &themes::BUTTON.label)),
                              themed(shortcut, &themes::BUTTON.shortcut)))
}

impl<'a> Drawable for Widget<'a> {
    fn draw(&self, rb: &RustBox) -> Option<Size> {
        match *self {
            Widget::Button(ref button) => {
                if let &Some(pos) = button.position() {
                    let key_name = key_name(&button.key());

                    let size = rb.draw(&pos, &button.key().theme(), &key_name);

                    let pos = pos + width(size.w);

                    let label = format!(" {} ", button.label().to_string());

                    Some(rb.draw(&pos, &button.label().theme(), &label) + width(size.w))
                } else {
                    None
                }
            }
            Widget::Pannel(ref pannel, ref children) => None,
        }
    }
}

fn key_name(key: &Key) -> Atom {
    match *key {
        Key::Tab => Atom::from("Tab"),
        Key::Enter => Atom::from("Enter"),
        Key::Esc => Atom::from("Esc"),
        Key::Backspace => Atom::from("BkSp"),
        Key::Right => Atom::from("Right"),
        Key::Left => Atom::from("Left"),
        Key::Up => Atom::from("Up"),
        Key::Down => Atom::from("Down"),
        Key::Delete => Atom::from("Del"),
        Key::Insert => Atom::from("Ins"),

        Key::Home => Atom::from("Home"),
        Key::End => Atom::from("End"),
        Key::PageUp => Atom::from("PgUp"),
        Key::PageDown => Atom::from("PgDn"),

        Key::Char(c) => Atom::from(format!("{}", c)),
        Key::Ctrl(c) => Atom::from(format!("^{}", c)),
        Key::F(n) => Atom::from(format!("F{}", n)),
        Key::Unknown(u) => Atom::from(format!("{}", u)),
    }
}

pub struct TermUI<'a> {
    rb: RustBox,
    buttons: Vec<Widget<'a>>,
}

impl<'a> TermUI<'a> {
    pub fn new() -> Result<Box<UI>> {
        Ok(Box::new(TermUI {
            rb: try!(RustBox::init(Default::default())),
            buttons: Vec::new(),
        }))
    }

    fn init(&mut self) {
        self.rb.clear();

        self.buttons
            .append(&mut vec![button("Help", Key::F(1)), button("Quit", Key::F(10))]);

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
                rustbox::Event::KeyEvent(key) => {
                    match key {
                        Key::Char('q') => {
                            break;
                        }
                        _ => {
                            debug!("drop {:?} key", key);
                        }
                    }
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
