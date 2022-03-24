use Token::*;

use super::Cursor;

#[derive(Debug)]
pub enum Token {
    Add,
    And,
    Arrow,
    Assign,
    EmptyList,
    Decimal(f32),
    Div,
    Cons,
    Else,
    Eq,
    FAdd,
    FDiv,
    FMul,
    FSub,
    Fun,
    Ge,
    Gt,
    Ident(String),
    If,
    In,
    Integer(i32),
    Le,
    Let,
    Lparen,
    Lt,
    Match,
    MatchOr,
    Mul,
    Neq,
    Or,
    Rec,
    Rparen,
    Sub,
    Then,
    Unknown,
    Whitespace,
    With,
}

impl Cursor<'_> {
    pub fn advance_token(&mut self) -> Option<Token> {
        Some(match self.bump()? {
            c if is_digit(c) => self.int_or_decimal(),
            c if is_id_start(c) => self.ident(),
            c if is_whitespace(c) => self.eat_whitespace(),
            '(' => Lparen,
            ')' => Rparen,
            '+' => match self.first() {
                '.' => {
                    self.bump();
                    FAdd
                }
                _ => Add,
            },
            '-' => match self.first() {
                '.' => {
                    self.bump();
                    FSub
                }
                '>' => {
                    self.bump();
                    Arrow
                }
                _ => Sub,
            },
            '*' => match self.first() {
                '.' => {
                    self.bump();
                    FMul
                }
                _ => Mul,
            },
            '/' => match self.first() {
                '.' => {
                    self.bump();
                    FDiv
                }
                _ => Div,
            },
            '<' => match self.first() {
                '=' => {
                    self.bump();
                    Le
                }
                '>' => {
                    self.bump();
                    Unknown
                }
                _ => Lt,
            },
            '>' => match self.first() {
                '=' => {
                    self.bump();
                    Ge
                }
                _ => Gt,
            },
            '=' => match self.first() {
                '=' => {
                    self.bump();
                    Eq
                }
                _ => Assign,
            },
            '!' => match self.first() {
                '=' => {
                    self.bump();
                    Neq
                }
                _ => Unknown,
            },
            '&' => match self.first() {
                '&' => {
                    self.bump();
                    And
                }
                _ => Unknown,
            },
            '|' => match self.first() {
                '|' => {
                    self.bump();
                    Or
                }
                _ => MatchOr,
            },
            ':' => match self.first() {
                ':' => {
                    self.bump();
                    Cons
                }
                _ => Unknown,
            },
            '[' => match self.first() {
                ']' => {
                    self.bump();
                    EmptyList
                }
                _ => Unknown,
            },
            _ => Unknown,
        })
    }

    fn int_or_decimal(&mut self) -> Token {
        let mut n = String::from(self.prev());
        let mut decimal = false;
        loop {
            match self.first() {
                c if is_digit(c) => {
                    self.bump();
                    n.push(c);
                }
                '.' => {
                    decimal = true;
                    self.bump();
                    n.push('.');
                }
                _ => {
                    if decimal {
                        return Decimal(n.parse().unwrap());
                    } else {
                        return Integer(n.parse().unwrap());
                    }
                }
            }
        }
    }

    fn ident(&mut self) -> Token {
        let mut id = vec![self.prev()];
        loop {
            if !is_id_cont(self.first()) {
                if is_id_end(self.first()) {
                    id.push(self.bump().unwrap());
                }
                break;
            }
            id.push(self.bump().unwrap());
        }
        match id.into_iter().collect::<String>().as_str() {
            "let" => Let,
            "rec" => Rec,
            "in" => In,
            "if" => If,
            "then" => Then,
            "else" => Else,
            "fun" => Fun,
            "match" => Match,
            "with" => With,
            i => Ident(i.to_string()),
        }
    }

    fn eat_whitespace(&mut self) -> Token {
        loop {
            if !is_whitespace(self.first()) {
                break;
            }
            self.bump();
        }
        Whitespace
    }
}

fn is_id_start(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}

fn is_id_cont(c: char) -> bool {
    is_id_start(c) || ('0'..='9').contains(&c) || c == '\''
}

fn is_id_end(c: char) -> bool {
    is_id_cont(c) || c == '?' || c == '!'
}

fn is_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_whitespace(c: char) -> bool {
    matches!(c, '\u{0009}' | '\u{000A}' | '\u{000B}' | '\u{0020}')
}
