use std::ops::{Deref, DerefMut};

use rustbox::{self, Color, Style};

#[derive(Clone, Debug)]
pub struct Theme {
    pub style: Style,
    pub fg: Color,
    pub bg: Color,
}

pub trait HasTheme {
    fn theme(&self) -> &Theme;
}

#[derive(Clone, Debug)]
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

impl<'a, T> HasTheme for Themed<'a, T> {
    fn theme(&self) -> &Theme {
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
        style: rustbox::RB_NORMAL,
        fg: Color::White,
        bg: Color::Black,
    },
};

pub struct Pannel {
    pub label: Theme,
}

pub static PANNEL: Pannel = Pannel {
    label: Theme {
        style: rustbox::RB_NORMAL,
        fg: Color::White,
        bg: Color::Default,
    },
};

pub struct Input {
    pub placeholder: Theme,
    pub text: Theme,
}

pub static INPUT: Input = Input {
    placeholder: Theme {
        style: rustbox::RB_UNDERLINE,
        fg: Color::Cyan,
        bg: Color::Default,
    },
    text: Theme {
        style: rustbox::RB_NORMAL,
        fg: Color::Yellow,
        bg: Color::Default,
    },
};
