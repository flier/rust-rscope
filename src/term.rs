use std::fmt;
use std::cmp;
use std::convert::From;
use std::iter::FromIterator;
use std::ops::{Add, AddAssign, Sub, SubAssign, Neg, Shl, Shr, BitAnd, Deref, DerefMut};

use rustbox::{Key, RustBox};

use string_cache::Atom;

use themes::{self, Theme, HasTheme, Themed, themed};

#[derive(PartialEq, Eq, PartialOrd, Debug, Clone, Copy)]
pub struct Pos {
    x: isize,
    y: isize,
}

pub fn position(x: isize, y: isize) -> Pos {
    Pos { x: x, y: y }
}

trait HasPosition {
    fn position(&self) -> Pos;
}

impl HasPosition for Pos {
    fn position(&self) -> Pos {
        *self
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

    pub fn as_distance(&self) -> Distance {
        distance(self.w as isize, self.h as isize)
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
pub struct Rect(Pos, Size);

impl Rect {
    pub fn width(&self) -> usize {
        self.1.w
    }

    pub fn height(&self) -> usize {
        self.1.h
    }
}

impl Rect {
    pub fn size(&self) -> Size {
        self.1
    }

    pub fn move_by(&self, distance: Distance) -> Rect {
        Rect(self.0 + distance, self.1)
    }
}

impl Deref for Rect {
    type Target = Pos;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Rect {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Shl<Distance> for Rect {
    type Output = Rect;

    fn shl(self, rhs: Distance) -> Self::Output {
        Rect(self.0 - rhs, self.1)
    }
}

impl Shr<Distance> for Rect {
    type Output = Rect;

    fn shr(self, rhs: Distance) -> Self::Output {
        Rect(self.0 + rhs, self.1)
    }
}

impl BitAnd<Rect> for Rect {
    type Output = Rect;

    fn bitand(self, rhs: Rect) -> Self::Output {
        Rect(self.0,
             size(if self.0.x > rhs.0.x {
                      self.1.w.saturating_sub((self.0.x - rhs.0.x) as usize)
                  } else {
                      rhs.1.w.saturating_sub((rhs.0.x - self.0.x) as usize)
                  },
                  if self.0.y > rhs.0.y {
                      self.1.h.saturating_sub((self.0.y - rhs.0.y) as usize)
                  } else {
                      rhs.1.h.saturating_sub((rhs.0.y - self.0.y) as usize)
                  }))
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

#[derive(Clone,  Debug)]
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

#[derive(Clone,  Debug)]
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

trait AutoSize {
    fn preferred_size(&self, size: &Size) -> Size;
}

trait Named {
    fn name(&self) -> Atom;
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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
    fn align_to(&self, rect: &Rect, align: Align) -> Pos;
}

impl<T: AutoSize + fmt::Display> Alignment for T {
    fn align_to(&self, rect: &Rect, align: Align) -> Pos {
        let size = self.preferred_size(&rect.size());

        let distance = match align {
            Align::Center => {
                distance((rect.width() - size.w) as isize / 2,
                         (rect.height() - size.h) as isize / 2)
            }
            Align::Top => columns((rect.width() - size.w) / 2),
            Align::Bottom => {
                distance((rect.width() - size.w) as isize / 2,
                         (rect.height() - size.h) as isize)
            }
            Align::Left => lines((rect.height() - size.h) / 2),
            Align::Right => {
                distance((rect.width() - size.w) as isize,
                         (rect.height() - size.h) as isize / 2)
            }
            Align::LeftTop => distance(0, 0),
            Align::LeftBottom => lines(rect.height() - size.h),
            Align::RightTop => columns(rect.width() - size.w),
            Align::RightBottom => {
                distance((rect.width() - size.w) as isize,
                         (rect.height() - size.h) as isize)
            }
        };

        let pos = rect.position() + distance;

        debug!("widget {} {:?} align to {:?} of {:?} with {:?} to {:?}",
               self,
               size,
               align,
               rect,
               distance,
               pos);

        pos
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Fill {
    Width,
    Height,
    Content,
}

trait FillUp {
    fn fill_up(&self, size: &Size, fill: Fill) -> Size;
}

impl<T: AutoSize + fmt::Display> FillUp for T {
    fn fill_up(&self, parent_size: &Size, fill: Fill) -> Size {
        let fillup_size = match fill {
            Fill::Width => {
                size(parent_size.width(),
                     self.preferred_size(&parent_size).height())
            }
            Fill::Height => {
                size(self.preferred_size(&parent_size).width(),
                     parent_size.height())
            }
            Fill::Content => *parent_size,
        };

        debug!("widget {} fill {:?} up to {:?} base on {:?}",
               self,
               fill,
               fillup_size,
               parent_size);

        fillup_size
    }
}

pub trait Canvas {
    fn rect(&self) -> Rect;

    fn draw(&self, pos: &Pos, theme: &Theme, s: &str) -> Size;
}

impl Canvas for RustBox {
    fn rect(&self) -> Rect {
        Rect(position(0, 0), size(self.width(), self.height()))
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

    fn draw_to(&self, canvas: &C, rect: Rect) -> Size;
}

#[derive(Clone, Debug)]
pub enum Widget<'a> {
    Button(Shortcut<'a, ThemedLabel<'a>>),

    Input(Option<ThemedLabel<'a>>),

    Pannel(Option<ThemedLabel<'a>>, Vec<Widget<'a>>),

    Space(usize),

    AlignTo(Align, Box<Widget<'a>>),

    FillUp(Fill, Box<Widget<'a>>),

    Pinned(Rect, Box<Widget<'a>>),
}

pub fn button<'a>(label: &str, shortcut: Key) -> Widget<'a> {
    Widget::Button(shortcuted(themed(Atom::from(label), &themes::BUTTON.label),
                              themed(shortcut, &themes::BUTTON.shortcut)))
}

pub fn input<'a>(placeholder: Option<&str>) -> Widget<'a> {
    Widget::Input(placeholder.map(|placeholder| {
                       themed(Atom::from(placeholder), &themes::INPUT.placeholder)
                   }))
}

pub fn pannel<'a, I>(label: Option<&str>, iter: I) -> Widget<'a>
    where I: IntoIterator<Item = Widget<'a>>
{
    Widget::Pannel(label.map(|label| themed(Atom::from(label), &themes::PANNEL.label)),
                   Vec::from_iter(iter))
}

pub fn space<'a>(n: usize) -> Widget<'a> {
    Widget::Space(n)
}

pub fn align_to<'a>(w: Widget<'a>, align: Align) -> Widget<'a> {
    Widget::AlignTo(align, Box::new(w))
}

pub fn fill_up<'a>(w: Widget<'a>, fill: Fill) -> Widget<'a> {
    Widget::FillUp(fill, Box::new(w))
}

impl<'a> fmt::Display for Widget<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Widget::Button(ref label) => write!(f, "Button({})", label.to_string()),
            Widget::Input(ref prompt) => {
                write!(f,
                       "Input({})",
                       if let &Some(ref prompt) = prompt {
                           prompt.to_string()
                       } else {
                           String::new()
                       })
            }
            Widget::Pannel(ref label, _) => {
                write!(f,
                       "Pannel({})",
                       if let &Some(ref label) = label {
                           label.to_string()
                       } else {
                           String::new()
                       })
            }
            Widget::Space(n) => write!(f, "Space({})", n),
            Widget::AlignTo(align, _) => write!(f, "AlignTo({:?})", align),
            Widget::FillUp(fill, _) => write!(f, "FillUp({:?})", fill),
            Widget::Pinned(ref pinned, ref w) => write!(f, "Pinned({:?}, {})", pinned, w),
        }
    }
}

impl<'a> AutoSize for Widget<'a> {
    fn preferred_size(&self, parent_size: &Size) -> Size {
        let preferred_size = match *self {
            Widget::Button(ref button) => {
                size(button.shortcut().as_atom().len() + button.label().len() + 2,
                     1)
            }

            Widget::Input(ref prompt) => {
                match *prompt {
                    Some(ref prompt) => size(prompt.len(), 1),
                    None => size(0, 1),
                }
            }

            Widget::Pannel(_, ref children) => {
                let mut x = 0;
                let mut y = 0;
                let mut max_width = 0;
                let mut cur_height = 0;

                for child in children {
                    let child_size = child.preferred_size(parent_size);

                    if x + child_size.width() > parent_size.width() {
                        x = 0;
                        y += cur_height;
                    }

                    x += child_size.width();
                    max_width = cmp::max(max_width, x);
                    cur_height = cmp::max(cur_height, child_size.height());
                }

                y += cur_height;

                size(max_width, y)
            }

            Widget::Space(n) => size(n, 1),

            Widget::AlignTo(_, ref w) => w.preferred_size(parent_size),

            Widget::FillUp(fill, ref w) => w.fill_up(parent_size, fill),

            Widget::Pinned(ref pinned, _) => pinned.size(),
        };

        debug!("widget {} preferred {:?} within parent {:?}",
               self,
               preferred_size,
               parent_size);

        preferred_size
    }
}

pub trait Layout {
    fn layout(&self, rect: Rect) -> Self;
}

impl<'a> Layout for Widget<'a> {
    fn layout(&self, rect: Rect) -> Self {
        match *self {
            Widget::Button(_) |
            Widget::Input(_) => {
                let pinned = Rect(rect.position(), self.preferred_size(&rect.size()));

                debug!("widget {} pinned to {:?}", self, pinned);

                Widget::Pinned(pinned, Box::new(self.clone()))
            }

            Widget::Space(_) |
            Widget::Pinned(_, _) => self.clone(),

            Widget::Pannel(ref label, ref children) => {
                let mut widgets = Vec::new();
                let mut x = 0;
                let mut y = 0;
                let mut cur_height: usize = 0;
                let parent_size = rect.size();

                for child in children {
                    let child_size = child.preferred_size(&parent_size);

                    if x + child_size.width() > parent_size.width() {
                        x = 0;
                        y += cur_height;
                    }

                    let pinned = (rect >> distance(x as isize, y as isize)) & rect;

                    widgets.push(Widget::Pinned(pinned, Box::new(child.layout(pinned))));

                    x += child_size.width();
                    cur_height = cmp::max(cur_height, child_size.height());
                }

                y += cur_height;

                Widget::Pinned(Rect(rect.position(), size(x, y)),
                               Box::new(Widget::Pannel(label.clone(), widgets)))
            }

            Widget::AlignTo(align, ref w) => {
                let pos = w.align_to(&rect, align);
                let r = (rect >> (pos - rect.position())) & rect;
                let size = w.preferred_size(&r.size());
                let pinned = Rect(pos, size);

                debug!("widget {} pinned to {:?} after align {:?} within {:?}",
                       w,
                       pinned,
                       align,
                       rect);

                let child = w.layout(pinned);

                Widget::Pinned(pinned,
                               if let Widget::Pinned(_, ref w) = child {
                                   w.clone()
                               } else {
                                   Box::new(child)
                               })
            }
            Widget::FillUp(fill, ref w) => {
                let pinned = Rect(rect.position(), w.fill_up(&rect.size(), fill));

                debug!("widget {} draw to {:?} after fill {:?} within {:?}",
                       w,
                       pinned,
                       fill,
                       rect);

                let child = w.layout(pinned);

                Widget::Pinned(pinned,
                               if let Widget::Pinned(_, ref w) = child {
                                   w.clone()
                               } else {
                                   Box::new(child)
                               })
            }
        }
    }
}

impl<'a, C: Canvas> Drawable<C> for Widget<'a> {
    fn draw(&self, canvas: &C) -> Size {
        self.draw_to(canvas, canvas.rect())
    }

    fn draw_to(&self, canvas: &C, rect: Rect) -> Size {
        match *self {
            Widget::Button(ref button) => {
                let size = canvas.draw(&rect.position(),
                                       &button.shortcut().theme(),
                                       &button.shortcut().as_atom());

                let pos = rect.position() + columns(size.w);

                let label = format!(" {} ", button.label().to_string());

                debug!("widget {} draw to {:?}", self, pos);

                canvas.draw(&pos, &button.label().theme(), &label) + columns(size.w)
            }

            Widget::Input(ref prompt) => {
                if let &Some(ref prompt) = prompt {
                    debug!("widget {} draw to {:?}", self, rect.position());

                    let mut underline = Vec::new();

                    underline.resize(rect.width(), b' ');

                    let size = canvas.draw(&rect.position(),
                                           &prompt.theme(),
                                           &String::from_utf8(underline).unwrap());

                    canvas.draw(&rect.position(), &prompt.theme(), &prompt);

                    size
                } else {
                    size(0, 0)
                }
            }

            Widget::Pannel(_, ref children) => {
                debug!("widget {} draw to {:?}", self, rect.position());

                let mut x = 0;
                let mut y = 0;
                let mut cur_height: usize = 0;
                let parent_size = rect.size();

                for child in children {
                    let child_size = child.preferred_size(&parent_size);

                    if x + child_size.width() > parent_size.width() {
                        x = 0;
                        y += cur_height;
                    }

                    let r = (rect >> distance(x as isize, y as isize)) & rect;

                    child.draw_to(canvas, r);

                    x += child_size.width();
                    cur_height = cmp::max(cur_height, child_size.height());
                }

                y += cur_height;

                size(x, y)
            }

            Widget::Space(n) => size(n, 1),

            Widget::AlignTo(_, ref w) |
            Widget::FillUp(_, ref w) => {
                if let Widget::Pinned(pinned, w) = w.layout(rect) {
                    w.draw_to(canvas, pinned)
                } else {
                    size(0, 0)
                }
            }

            Widget::Pinned(pinned, ref w) => w.draw_to(canvas, pinned),
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
