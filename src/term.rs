use std::convert::From;
use std::ops::{Add, AddAssign, Sub, SubAssign, Neg, Deref, DerefMut};

use rustbox::{Key, RustBox};

use string_cache::Atom;

use themes::{self, Theme, HasTheme, Themed, themed};

trait HasContent {
    fn size(&self) -> Size;
}

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
pub struct Pos {
    x: isize,
    y: isize,
}

pub fn position(x: isize, y: isize) -> Pos {
    Pos { x: x, y: y }
}

trait HasPosition {
    fn position(&self) -> &Pos;
}

impl HasPosition for Pos {
    fn position(&self) -> &Pos {
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

impl Sub<Pos> for Pos {
    type Output = Distance;

    fn sub(self, rhs: Pos) -> Self::Output {
        Distance {
            cols: self.x - rhs.x,
            lines: self.y - rhs.y,
        }
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
pub struct Size {
    w: usize,
    h: usize,
}

impl Size {
    pub fn width(&self) -> usize {
        self.w
    }

    pub fn height(&self) -> usize {
        self.h
    }
}

pub fn size(w: usize, h: usize) -> Size {
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

impl Add<Size> for Size {
    type Output = Self;

    fn add(self, rhs: Size) -> Self::Output {
        Size {
            w: self.w + rhs.w,
            h: self.h + rhs.h,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
pub struct Distance {
    cols: isize,
    lines: isize,
}

fn distance(cols: isize, lines: isize) -> Distance {
    Distance {
        cols: cols,
        lines: lines,
    }
}

fn columns(cols: usize) -> Distance {
    Distance {
        cols: cols as isize,
        lines: 0,
    }
}

fn lines(lines: usize) -> Distance {
    Distance {
        cols: 0,
        lines: lines as isize,
    }
}

impl Distance {
    pub fn as_size(&self) -> Size {
        Size {
            w: self.cols as usize,
            h: self.lines as usize,
        }
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

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
pub struct Container(Pos, Size);

impl Container {
    pub fn width(&self) -> usize {
        self.1.w
    }

    pub fn height(&self) -> usize {
        self.1.h
    }
}

impl Deref for Container {
    type Target = Pos;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Container {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

type Label = Atom;
type ThemedLabel<'a> = Themed<'a, Label>;

trait HasLabel<'a> {
    fn label(&self) -> &ThemedLabel<'a>;
}

impl<'a> HasLabel<'a> for ThemedLabel<'a> {
    fn label(&self) -> &ThemedLabel<'a> {
        self
    }
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
    fn shortcut(&self) -> &ThemedKey<'a>;
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
    fn shortcut(&self) -> &ThemedKey<'a> {
        &self.1
    }
}

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
pub enum Align {
    Center,
    Top,
    Bottom,
    Left,
    Right,
    LeftTop,
    LeftBottom,
    RightTop,
    RightBottom,
}

trait Alignment {
    fn align_to(&self, container: &Container, align: Align) -> Pos;
}

pub trait Canvas {
    fn container(&self) -> Container;

    fn draw(&self, pos: &Pos, theme: &Theme, s: &str) -> Size;
}

impl Canvas for RustBox {
    fn container(&self) -> Container {
        Container(position(0, 0), size(self.width(), self.height()))
    }

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

pub trait Drawable<C: Canvas> {
    fn draw(&self, canvas: &C) -> Size;

    fn draw_to(&self, canvas: &C, pos: &Pos) -> Size;
}

#[derive(Debug)]
pub enum Widget<'a> {
    Button(Shortcut<'a, ThemedLabel<'a>>),

    Pannel(Vec<Widget<'a>>),

    Space(usize),

    AlignTo(Box<Widget<'a>>, Align),
}

pub fn button<'a>(label: &str, shortcut: Key) -> Widget<'a> {
    Widget::Button(shortcuted(themed(Atom::from(label), &themes::BUTTON.label),
                              themed(shortcut, &themes::BUTTON.shortcut)))
}

pub fn space<'a>(n: usize) -> Widget<'a> {
    Widget::Space(n)
}

pub fn align_to<'a>(w: Widget<'a>, align: Align) -> Widget<'a> {
    Widget::AlignTo(Box::new(w), align)
}

impl<'a> HasContent for Widget<'a> {
    fn size(&self) -> Size {
        match *self {
            Widget::Button(ref button) => {
                size(button.shortcut().as_atom().len() + button.label().len() + 2,
                     1)
            }
            Widget::Pannel(ref children) => {
                size(children.iter().fold(0, |width, ref child| width + child.size().width()),
                     children.iter().map(|ref child| child.size().height()).max().unwrap())
            }
            Widget::Space(n) => size(n, 1),
            Widget::AlignTo(ref w, _) => w.size(),
        }
    }
}

impl<'a> Alignment for Widget<'a> {
    fn align_to(&self, container: &Container, align: Align) -> Pos {
        let size = self.size();

        let distance = match align {
            Align::Center => {
                distance((container.width() - size.w) as isize / 2,
                         (container.height() - size.h) as isize / 2)
            }
            Align::Top => columns((container.width() - size.w) / 2),
            Align::Bottom => {
                distance((container.width() - size.w) as isize / 2,
                         (container.height() - size.h) as isize)
            }
            Align::Left => lines((container.height() - size.h) / 2),
            Align::Right => {
                distance((container.width() - size.w) as isize,
                         (container.height() - size.h) as isize / 2)
            }
            Align::LeftTop => distance(0, 0),
            Align::LeftBottom => lines(container.height() - size.h),
            Align::RightTop => columns(container.width() - size.w),
            Align::RightBottom => {
                distance((container.width() - size.w) as isize,
                         (container.height() - size.h) as isize)
            }
        };

        debug!("widget {:?} align to {:?} of {:?} with {:?}",
               size,
               align,
               container,
               distance);

        *container.position() + distance
    }
}

impl<'a, C: Canvas> Drawable<C> for Widget<'a> {
    fn draw(&self, canvas: &C) -> Size {
        self.draw_to(canvas, &canvas.container())
    }

    fn draw_to(&self, canvas: &C, pos: &Pos) -> Size {
        match *self {
            Widget::Button(ref button) => {
                let size = canvas.draw(&pos,
                                       &button.shortcut().theme(),
                                       &button.shortcut().as_atom());

                let pos = *pos + columns(size.w);

                let label = format!(" {} ", button.label().to_string());

                canvas.draw(&pos, &button.label().theme(), &label) + columns(size.w)
            }
            Widget::Pannel(ref children) => {
                let mut p = *pos;

                for child in children {
                    p += columns(child.draw_to(canvas, &p).w);
                }

                (p - *pos).as_size()
            }
            Widget::Space(n) => size(n, 1),
            Widget::AlignTo(ref w, align) => {
                w.draw_to(canvas, &w.align_to(&canvas.container(), align))
            }
        }
    }
}

trait AsAtom {
    fn as_atom(&self) -> Atom;
}

impl AsAtom for Key {
    fn as_atom(&self) -> Atom {
        match *self {
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
}
