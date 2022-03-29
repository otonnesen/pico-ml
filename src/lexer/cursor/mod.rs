mod tokenize;

use std::str::Chars;
pub use tokenize::Token;

#[derive(Debug)]
pub struct Cursor<'a> {
    _initial_len: usize,
    chars: Chars<'a>,
    prev: char,
}

const EOF: char = '\0';

impl<'a> Cursor<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Self {
            _initial_len: input.len(),
            chars: input.chars(),
            prev: EOF,
        }
    }

    #[must_use]
    fn nth_char(&self, n: usize) -> char {
        self.chars().nth(n).unwrap_or(EOF)
    }

    #[must_use]
    fn first(&self) -> char {
        self.nth_char(0)
    }

    #[must_use]
    fn prev(&self) -> char {
        self.prev
    }

    fn chars(&self) -> Chars {
        self.chars.clone()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.prev = c;
        Some(c)
    }
}
