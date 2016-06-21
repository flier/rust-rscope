use std::ops::{Deref, DerefMut};

use rustbox::{self, Color, Style};

#[derive(Debug, Clone)]
pub struct Theme {
    pub style: Style,
    pub fg: Color,
    pub bg: Color,
}

#[derive(Debug, Clone)]
pub struct Themed<'a, T>(T, &'a Theme);

impl<'a, T> Deref for Themed<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> DerefMut for Themed<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a, T> Themed<'a, T> {
    pub fn theme(&self) -> &'a Theme {
        self.1
    }
}

pub fn themed<'a, T>(value: T, theme: &'a Theme) -> Themed<'a, T> {
    Themed(value, theme)
}

pub struct Button {
    pub shortcut: Theme,
    pub label: Theme,
}

pub static BUTTON: Button = Button {
    shortcut: Theme {
        style: rustbox::RB_REVERSE,
        fg: Color::White,
        bg: Color::Black,
    },

    label: Theme {
        style: rustbox::RB_REVERSE,
        fg: Color::White,
        bg: Color::Black,
    },
};
