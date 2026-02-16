use crate::lisp::arena::{Arena, Id, Node};

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    LParen,
    RParen,
    Quote,
    Number(i64),
    String(String),
    Symbol(String),
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            src: input.as_bytes(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            while matches!(self.peek(), Some(b' ' | b'\t' | b'\r' | b'\n')) {
                self.pos += 1;
            }
            if self.peek() == Some(b';') {
                while let Some(c) = self.bump() {
                    if c == b'\n' {
                        break;
                    }
                }
                continue;
            }
            break;
        }
    }

    fn next_token(&mut self) -> Result<Option<Tok>, String> {
        self.skip_ws_and_comments();
        let Some(c) = self.bump() else {
            return Ok(None);
        };
        match c {
            b'(' => Ok(Some(Tok::LParen)),
            b')' => Ok(Some(Tok::RParen)),
            b'\'' => Ok(Some(Tok::Quote)),
            b'"' => self.read_string().map(Some),
            b'|' => self.read_bar_symbol().map(Some),
            b'#' => {
                // Common Lisp function designator shorthand: #'foo => foo
                if self.peek() == Some(b'\'') {
                    self.pos += 1;
                    let Some(first) = self.bump() else {
                        return Err("expected symbol after #'".into());
                    };
                    if first == b'|' {
                        return self.read_bar_symbol().map(Some);
                    }
                    if !is_symbol_char(first) {
                        return Err("expected symbol after #'".into());
                    }
                    Ok(Some(self.read_symbol_or_number(first)))
                } else if is_symbol_char(c) {
                    Ok(Some(self.read_symbol_or_number(c)))
                } else {
                    Err(format!("unexpected character: '{}'", c as char))
                }
            }
            _ => {
                if is_symbol_char(c) {
                    Ok(Some(self.read_symbol_or_number(c)))
                } else {
                    Err(format!("unexpected character: '{}'", c as char))
                }
            }
        }
    }

    fn read_string(&mut self) -> Result<Tok, String> {
        let mut out = String::new();
        while let Some(c) = self.bump() {
            match c {
                b'"' => return Ok(Tok::String(out)),
                b'\\' => {
                    let Some(n) = self.bump() else {
                        return Err("unterminated escape in string".into());
                    };
                    match n {
                        b'"' => out.push('"'),
                        b'\\' => out.push('\\'),
                        b'n' => out.push('\n'),
                        b't' => out.push('\t'),
                        other => out.push(other as char),
                    }
                }
                other => out.push(other as char),
            }
        }
        Err("unterminated string literal".into())
    }

    fn read_bar_symbol(&mut self) -> Result<Tok, String> {
        let mut out = String::new();
        while let Some(c) = self.bump() {
            match c {
                b'|' => return Ok(Tok::Symbol(out)),
                b'\\' => {
                    let Some(n) = self.bump() else {
                        return Err("unterminated escape in symbol".into());
                    };
                    out.push(n as char);
                }
                other => out.push(other as char),
            }
        }
        Err("unterminated |...| symbol".into())
    }

    fn read_symbol_or_number(&mut self, first: u8) -> Tok {
        let mut s = String::new();
        s.push(first as char);
        while let Some(c) = self.peek() {
            if is_symbol_char(c) {
                self.pos += 1;
                s.push(c as char);
            } else {
                break;
            }
        }
        if let Ok(n) = s.parse::<i64>() {
            Tok::Number(n)
        } else {
            Tok::Symbol(s)
        }
    }
}

fn is_symbol_char(c: u8) -> bool {
    c.is_ascii_alphanumeric()
        || matches!(
            c,
            b'_' | b'+' | b'-' | b'*' | b'/' | b'=' | b'?' | b'!' | b'<' | b'>' | b':' | b'.' | b'#' | b'|'
        )
}

pub fn parse_program(arena: &mut Arena, input: &str) -> Result<Vec<Id>, String> {
    let mut lx = Lexer::new(input);
    let mut toks = Vec::new();
    while let Some(t) = lx.next_token()? {
        toks.push(t);
    }

    let mut p = Parser {
        arena,
        toks,
        pos: 0,
    };
    let mut out = Vec::new();
    while !p.eof() {
        out.push(p.parse_expr()?);
    }
    Ok(out)
}

struct Parser<'a> {
    arena: &'a mut Arena,
    toks: Vec<Tok>,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn eof(&self) -> bool {
        self.pos >= self.toks.len()
    }

    fn peek(&self) -> Option<&Tok> {
        self.toks.get(self.pos)
    }

    fn bump(&mut self) -> Option<Tok> {
        let t = self.peek()?.clone();
        self.pos += 1;
        Some(t)
    }

    fn parse_expr(&mut self) -> Result<Id, String> {
        match self.bump() {
            Some(Tok::LParen) => self.parse_list(),
            Some(Tok::Quote) => {
                let q = self.parse_expr()?;
                let quote_sym = self.arena.sym("quote");
                Ok(self.arena.list(vec![quote_sym, q]))
            }
            Some(Tok::Number(n)) => Ok(self.arena.alloc(Node::Int(n))),
            Some(Tok::String(s)) => Ok(self.arena.alloc(Node::Str(s))),
            Some(Tok::Symbol(s)) => match s.as_str() {
                "nil" => Ok(self.arena.alloc(Node::Nil)),
                "t" => Ok(self.arena.alloc(Node::Bool(true))),
                _ => Ok(self.arena.alloc(Node::Sym(s))),
            },
            Some(Tok::RParen) => Err("unexpected ')'".into()),
            None => Err("unexpected end of input".into()),
        }
    }

    fn parse_list(&mut self) -> Result<Id, String> {
        let mut xs = Vec::new();
        loop {
            match self.peek() {
                Some(Tok::RParen) => {
                    self.pos += 1;
                    return Ok(self.arena.alloc(Node::List(xs)));
                }
                None => return Err("unterminated list".into()),
                _ => xs.push(self.parse_expr()?),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_quote_and_list() {
        let mut a = Arena::new();
        let out = parse_program(&mut a, "'(a 1 \"x\")").expect("parse should succeed");
        assert_eq!(out.len(), 1);
        assert_eq!(a.fmt(out[0]), "(quote (a 1 \"x\"))");
    }

    #[test]
    fn parse_multiple_forms() {
        let mut a = Arena::new();
        let out = parse_program(&mut a, "(+ 1 2) ; cmt\n(define x 9)").expect("parse should succeed");
        assert_eq!(out.len(), 2);
        assert_eq!(a.fmt(out[0]), "(+ 1 2)");
        assert_eq!(a.fmt(out[1]), "(define x 9)");
    }

    #[test]
    fn parse_bar_escaped_symbol() {
        let mut a = Arena::new();
        let out = parse_program(&mut a, "'|[]|").expect("parse should succeed");
        assert_eq!(out.len(), 1);
        assert_eq!(a.fmt(out[0]), "(quote [])");
    }
}
