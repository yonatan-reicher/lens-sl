//! This module parses an input ARM program into our in-code representation.
//! Code taken from `arm-parser.rkt` at `https://github.com/mangpo/greenthumb`, the original Lens
//! implementation.
//!
//! Public surface:
//!   - `parse<W: Word>(src: &str) -> Result<Vec<arm::Inst<W>>, ParseError>`
//!   - `liveness_from_file(path: &str) -> Result<HashMap<String,Vec<usize>>, ...>`
//!   - `info_from_file(path: &str) -> Result<Vec<LiveValue>, ...>`

use std::collections::HashMap;
use std::fmt;
use std::fs;

use super::{ArgType, CondCode, OpCode, ShiftCode};
use crate::arm;
use crate::word::prelude::*;

// ─────────────────────────────────────────────────────────────────────────────
// Data types
// ─────────────────────────────────────────────────────────────────────────────

/// Direct analogue of the Racket `inst` struct.
/// `op`   — always three slots: [opcode, cond_type, shift_op]  (empty = absent)
/// `args` — operand strings
#[derive(Debug, Clone, PartialEq)]
pub struct Inst {
    pub op: Vec<String>,
    pub args: Vec<String>,
}

impl Inst {
    fn real(opcode: &str, cond: &str, shift: &str, args: Vec<String>) -> Self {
        Inst {
            op: vec![opcode.to_owned(), cond.to_owned(), shift.to_owned()],
            args,
        }
    }
    /// Hole instruction — `(inst #f #f)` in the original.
    fn hole() -> Self {
        Inst {
            op: vec![],
            args: vec![],
        }
    }
    pub fn is_hole(&self) -> bool {
        self.op.is_empty()
    }
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_hole() {
            write!(f, "HOLE")
        } else {
            write!(
                f,
                "{}{}{} {:?}",
                self.op[0], self.op[1], self.op[2], self.args
            )
        }
    }
}

/// Value from `info_from_file`: either a register number or a named register.
#[derive(Debug, Clone, PartialEq)]
pub enum LiveValue {
    Num(i64),
    Name(String),
}

// ─────────────────────────────────────────────────────────────────────────────
// Error type
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "parse error at line {} col {}: {}",
            self.line, self.col, self.message
        )
    }
}

impl std::error::Error for ParseError {}

// ─────────────────────────────────────────────────────────────────────────────
// Parser
// ─────────────────────────────────────────────────────────────────────────────
// Grammar (from the Racket source):
//
//   arg       ::= REG | HASH NUM | NUM
//   arg-pair  ::= WORD arg | '[' REG ',' arg ']'
//   args      ::= arg | arg-pair | arg ',' args | arg-pair ',' args
//   instruction ::= WORD args | WORD _WORD | NOP | '?'
//   inst-list ::= ε | instruction inst-list
//   code      ::= inst-list

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn advance(&mut self) -> &Token {
        let t = &self.tokens[self.pos];
        if self.pos + 1 < self.tokens.len() {
            self.pos += 1;
        }
        t
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        let t = self.peek().clone();
        if &t.kind == kind {
            self.advance();
            Ok(t)
        } else {
            Err(ParseError {
                message: format!("expected {:?}, got {:?} ('{}') ", kind, t.kind, t.value),
                line: t.line,
                col: t.col,
            })
        }
    }

    // arg ::= REG | HASH NUM | NUM
    fn parse_arg(&mut self) -> Result<String, ParseError> {
        match self.peek().kind {
            TokenKind::Reg => {
                let t = self.advance().clone();
                Ok(t.value)
            }
            TokenKind::Hash => {
                self.advance(); // consume '#'
                let n = self.expect(&TokenKind::Num)?;
                Ok(n.value)
            }
            TokenKind::Num => {
                let t = self.advance().clone();
                Ok(t.value)
            }
            _ => {
                let t = self.peek();
                Err(ParseError {
                    message: format!(
                        "expected arg (REG / # NUM / NUM), got {:?} ('{}')",
                        t.kind, t.value
                    ),
                    line: t.line,
                    col: t.col,
                })
            }
        }
    }

    // arg-pair ::= WORD arg | '[' REG ',' arg ']'
    // Returns (Option<String>, String) where the first is the optional WORD prefix
    // or base-register-from-brackets, and the second is the arg value.
    fn try_parse_arg_pair(&mut self) -> Result<Option<Vec<String>>, ParseError> {
        match self.peek().kind {
            TokenKind::Word => {
                // Could be `WORD arg` (arg-pair) or just `WORD args` (instruction).
                // The grammar is ambiguous here: `WORD` can be either the instruction
                // opcode or the first element of arg-pair inside an arg list.
                // In context this is only called from inside `parse_args` where we
                // already consumed the opcode word, so a WORD here is always the
                // arg-pair modifier (e.g. shift modifier like "lsl").
                let w = self.advance().clone();
                if w.value == "rrx" {
                    return Ok(Some(vec![w.value]));
                }
                let a = self.parse_arg()?;
                Ok(Some(vec![w.value, a]))
            }
            TokenKind::Lsqbr => {
                self.advance(); // '['
                let reg = self.expect(&TokenKind::Reg)?;
                self.expect(&TokenKind::Comma)?;
                let a = self.parse_arg()?;
                self.expect(&TokenKind::Rsqbr)?;
                Ok(Some(vec![reg.value, a]))
            }
            _ => Ok(None),
        }
    }

    // args ::= (arg | arg-pair) (',' (arg | arg-pair))*
    fn parse_args(&mut self) -> Result<Vec<String>, ParseError> {
        let mut result = Vec::new();

        // First element: try arg-pair, then arg
        if let Some(pair) = self.try_parse_arg_pair()? {
            result.extend(pair);
        } else {
            result.push(self.parse_arg()?);
        }

        while self.peek().kind == TokenKind::Comma {
            self.advance(); // consume ','
            if let Some(pair) = self.try_parse_arg_pair()? {
                result.extend(pair);
            } else {
                result.push(self.parse_arg()?);
            }
        }
        Ok(result)
    }

    // instruction ::= WORD args | WORD _WORD | NOP | '?'
    fn parse_instruction(&mut self) -> Result<Option<Inst>, ParseError> {
        match self.peek().kind {
            TokenKind::Eof => Ok(None),
            // Labels and .text directives don't produce instructions; skip them.
            TokenKind::Label | TokenKind::Block | TokenKind::Text => {
                self.advance();
                Ok(Some(Inst::hole())) // filtered out by caller
                // Actually we want to skip, not emit a hole — see parse_code.
            }
            TokenKind::Hole => {
                self.advance();
                Ok(Some(Inst::hole()))
            }
            TokenKind::Nop => {
                self.advance();
                Ok(Some(create_inst("nop", vec![])?))
            }
            TokenKind::Word => {
                let op_tok = self.advance().clone();
                match self.peek().kind {
                    TokenKind::UWord => {
                        let uw = self.advance().clone();
                        Ok(Some(create_special_inst(&op_tok.value, &uw.value)?))
                    }
                    TokenKind::Eof
                    | TokenKind::Word   // next instruction
                    | TokenKind::Nop
                    | TokenKind::Hole
                    | TokenKind::Label
                    | TokenKind::Block
                    | TokenKind::Text => {
                        // zero-arg instruction
                        Ok(Some(create_inst(&op_tok.value, vec![])?))
                    }
                    _ => {
                        let args = self.parse_args()?;
                        Ok(Some(create_inst(&op_tok.value, args)?))
                    }
                }
            }
            _ => {
                let t = self.peek();
                Err(ParseError {
                    message: format!(
                        "unexpected token {:?} ('{}') at start of instruction",
                        t.kind, t.value
                    ),
                    line: t.line,
                    col: t.col,
                })
            }
        }
    }

    // code ::= inst-list EOF
    fn parse_code(&mut self) -> Result<Vec<Inst>, ParseError> {
        let mut insts = Vec::new();
        loop {
            match self.peek().kind {
                TokenKind::Eof => break,
                // Skip non-instruction tokens at top level
                TokenKind::Label | TokenKind::Block | TokenKind::Text => {
                    self.advance();
                }
                _ => {
                    if let Some(inst) = self.parse_instruction()? {
                        // Don't emit synthetic holes from label/block skips
                        insts.push(inst);
                    }
                }
            }
        }
        Ok(insts)
    }
}

// ─────────────────────────────────────────────────────────────────────────────

// Lexer
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    /// Named / alias register: r0–r15, fp, ip, lr, sl
    Reg,
    /// Plain identifier (starts with a letter, continues with letters/digits)
    Word,
    /// Underscore-prefixed identifier
    UWord,
    /// "identifier:" label
    Label,
    /// "; BB<n>_<n>:" block comment — token value is the full lexeme
    Block,
    /// Signed decimal number
    Num,
    Nop,
    Text, // .text directive
    Comma,
    Dquote,
    Hole, // ?
    Hash,
    Lsqbr,
    Rsqbr,
    Eof,
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    value: String,
    line: usize,
    col: usize,
}

impl Token {
    fn new(kind: TokenKind, value: impl Into<String>, line: usize, col: usize) -> Self {
        Token {
            kind,
            value: value.into(),
            line,
            col,
        }
    }
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Lexer {
            src: src.as_bytes(),
            pos: 0,
            line: 1,
            col: 0,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<u8> {
        let ch = self.src.get(self.pos).copied()?;
        self.pos += 1;
        if ch == b'\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == b' ' || ch == b'\t' || ch == b'\r' || ch == b'\n' {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Skip a line comment that is NOT a block comment.
    /// A block comment looks like `; BB<n>_<n>:` — those must be tokenised.
    fn try_skip_line_comment(&mut self) -> bool {
        if self.peek() != Some(b';') {
            return false;
        }
        // Look ahead to decide if this is a block comment.
        let rest = &self.src[self.pos..];
        if is_block_comment_start(rest) {
            return false;
        }
        // Consume through end of line.
        while let Some(ch) = self.advance() {
            if ch == b'\n' {
                break;
            }
        }
        true
    }

    fn read_digits(&mut self) -> String {
        let mut s = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                s.push(ch as char);
                self.advance();
            } else {
                break;
            }
        }
        s
    }

    fn tokenise_all(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut tokens = Vec::new();
        loop {
            self.skip_whitespace();
            while self.try_skip_line_comment() {
                self.skip_whitespace();
            }
            let line = self.line;
            let col = self.col;
            match self.peek() {
                None => {
                    tokens.push(Token::new(TokenKind::Eof, "", line, col));
                    break;
                }
                Some(b',') => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Comma, ",", line, col));
                }
                Some(b'"') => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Dquote, "\"", line, col));
                }
                Some(b'?') => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Hole, "?", line, col));
                }
                Some(b'#') => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Hash, "#", line, col));
                }
                Some(b'[') => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Lsqbr, "[", line, col));
                }
                Some(b']') => {
                    self.advance();
                    tokens.push(Token::new(TokenKind::Rsqbr, "]", line, col));
                }
                Some(b';') => {
                    // Must be a block comment here (line comments already consumed above).
                    let tok = self.lex_block_comment(line, col)?;
                    tokens.push(tok);
                }
                Some(b'-') | Some(b'0'..=b'9') => {
                    let tok = self.lex_number(line, col)?;
                    tokens.push(tok);
                }
                Some(b'.') => {
                    // Directives like .text
                    self.advance(); // consume '.'
                    let word = self.lex_bare_identifier();
                    if word == "text" {
                        tokens.push(Token::new(TokenKind::Text, ".text", line, col));
                    }
                    // other directives are silently ignored (not needed by the grammar)
                }
                Some(b'_') => {
                    let tok = self.lex_u_identifier(line, col);
                    tokens.push(tok);
                }
                Some(ch) if ch.is_ascii_alphabetic() => {
                    let tok = self.lex_identifier_or_keyword(line, col);
                    tokens.push(tok);
                }
                Some(ch) => {
                    return Err(ParseError {
                        message: format!("unexpected character '{}'", ch as char),
                        line,
                        col,
                    });
                }
            }
        }
        Ok(tokens)
    }

    /// Lex a signed decimal number (possibly with a decimal fraction, though
    /// the grammar only uses integer values in practice).
    fn lex_number(&mut self, line: usize, col: usize) -> Result<Token, ParseError> {
        let mut s = String::new();
        if self.peek() == Some(b'-') {
            s.push('-');
            self.advance();
        }
        let digits = self.read_digits();
        if digits.is_empty() {
            return Err(ParseError {
                message: "expected digits after '-'".into(),
                line,
                col,
            });
        }
        s.push_str(&digits);
        if self.peek() == Some(b'.') {
            s.push('.');
            self.advance();
            s.push_str(&self.read_digits());
        }
        Ok(Token::new(TokenKind::Num, s, line, col))
    }

    /// Lex a block comment of the form `; BB<n>_<n>:`.
    fn lex_block_comment(&mut self, line: usize, col: usize) -> Result<Token, ParseError> {
        let mut s = String::new();
        // Consume through end of line (block comments are single-line in the original).
        while let Some(ch) = self.peek() {
            if ch == b'\n' {
                break;
            }
            s.push(ch as char);
            self.advance();
        }
        Ok(Token::new(TokenKind::Block, s, line, col))
    }

    /// Lex `_identifier` (underscore followed by letters/digits/underscore).
    fn lex_u_identifier(&mut self, line: usize, col: usize) -> Token {
        let mut s = String::from("_");
        self.advance(); // consume '_'
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                s.push(ch as char);
                self.advance();
            } else {
                break;
            }
        }
        Token::new(TokenKind::UWord, s, line, col)
    }

    /// Lex a bare word (no leading underscore, already known to start with a letter).
    fn lex_bare_identifier(&mut self) -> String {
        let mut s = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() {
                s.push(ch as char);
                self.advance();
            } else {
                break;
            }
        }
        s
    }

    /// Lex an identifier and classify it as Reg / Nop / Word / Label.
    fn lex_identifier_or_keyword(&mut self, line: usize, col: usize) -> Token {
        let word = self.lex_bare_identifier();

        // Check for trailing ':' → label
        if self.peek() == Some(b':') {
            self.advance();
            return Token::new(TokenKind::Label, format!("{}:", word), line, col);
        }

        // Keywords / register names
        if word == "nop" {
            return Token::new(TokenKind::Nop, word, line, col);
        }

        if is_register(&word) {
            return Token::new(TokenKind::Reg, word, line, col);
        }

        Token::new(TokenKind::Word, word, line, col)
    }
}

fn is_register(s: &str) -> bool {
    matches!(s, "fp" | "ip" | "lr" | "sl") || (s.starts_with('r') && s[1..].parse::<u32>().is_ok())
}

/// Returns true if the bytes starting at `src` look like `; BB<n>_<n>:`.
fn is_block_comment_start(src: &[u8]) -> bool {
    // "; BB" followed by digits, '_', digits, ':'
    if src.len() < 7 {
        return false;
    }
    if &src[..4] != b"; BB" {
        return false;
    }
    let mut i = 4;
    while i < src.len() && src[i].is_ascii_digit() {
        i += 1;
    }
    if i == 4 {
        return false;
    }
    if i >= src.len() || src[i] != b'_' {
        return false;
    }
    i += 1;
    let start = i;
    while i < src.len() && src[i].is_ascii_digit() {
        i += 1;
    }
    if i == start {
        return false;
    }
    i < src.len() && src[i] == b':'
}

// ─────────────────────────────────────────────────────────────────────────────
// Instruction construction  (create-inst / create-special-inst / rename)
// ─────────────────────────────────────────────────────────────────────────────

const COND_SUFFIXES: &[&str] = &["eq", "ne", "ls", "hi", "cc", "cs", "lt", "ge"];
const SHIFT_OPS: &[&str] = &["asr", "asl", "lsr", "lsl", "ror"];

fn create_special_inst(op1: &str, op2: &str) -> Result<Inst, ParseError> {
    match op2 {
        "__aeabi_idiv" => Ok(Inst::real(
            "sdiv",
            "",
            "",
            vec!["r0".into(), "r0".into(), "r1".into()],
        )),
        "__aeabi_uidiv" => Ok(Inst::real(
            "udiv",
            "",
            "",
            vec!["r0".into(), "r0".into(), "r1".into()],
        )),
        _ => Err(ParseError {
            message: format!("undefined special instruction: {} {}", op1, op2),
            line: 0,
            col: 0,
        }),
    }
}

fn create_inst(op: &str, mut args: Vec<String>) -> Result<Inst, ParseError> {
    // Normalise asl → lsl at the opcode level
    let mut op = if op == "asl" {
        "lsl".to_owned()
    } else {
        op.to_owned()
    };

    let args_len = args.len();

    // Special no-amount shift form: `..., rrx`
    if args_len >= 3 && args.last().is_some_and(|a| a == "rrx") {
        args.pop();
        let mut inst = create_inst(&op, args)?;
        inst.op[2] = "rrx".to_owned();
        return Ok(inst);
    }

    // Shift-op folding: if the second-to-last arg is a shift operator,
    // fold it into the op vector's third slot (as in the Racket original).
    if args_len >= 4 {
        let candidate = args[args_len - 2].clone();
        let norm = if candidate == "asl" {
            "lsl"
        } else {
            &candidate
        };
        if SHIFT_OPS.contains(&norm) {
            let last = args.pop().unwrap();
            let _shift_op = args.pop().unwrap();
            let shift_str = if candidate == "asl" {
                "lsl".to_owned()
            } else {
                candidate
            };
            // Recurse on the shorter arg list, then overwrite op[2]
            let mut inst = create_inst(&op, args)?;
            // inner call already produced a 3-slot op vec; overwrite shift slot
            inst.op[2] = shift_str;
            // restore the shift amount as the last arg
            inst.args.push(last);
            return Ok(inst);
        }
    }

    // Condition-code stripping
    let op_len = op.len();
    let (cond_type, bare_op) = if op_len > 3 {
        let suffix = &op[op_len - 2..];
        if COND_SUFFIXES.contains(&suffix) && op != "smmls" {
            (suffix.to_owned(), op[..op_len - 2].to_owned())
        } else {
            (String::new(), op.clone())
        }
    } else {
        (String::new(), op.clone())
    };

    op = bare_op;

    // ldr / str: treat fp (r99 in original) and scale offset by /4
    if (op == "ldr" || op == "str") && args_len >= 3 {
        let offset = &args[2];
        // Only scale if the offset does NOT start with 'r' (i.e. it is a literal)
        if !offset.starts_with('r')
            && let Ok(n) = offset.parse::<i64>()
        {
            args[2] = (n / 4).to_string();
        }
    }

    let renamed: Vec<String> = args.into_iter().map(rename).collect();

    Ok(Inst::real(&op, &cond_type, "", renamed))
}

fn rename(x: String) -> String {
    match x.as_str() {
        "sb" => "r9".into(),
        "sl" => "r10".into(),
        "fp" => "r11".into(),
        "ip" => "r12".into(),
        "sp" => "r13".into(),
        "lr" => "r14".into(),
        "pc" => "r15".into(),
        _ => x,
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Public API
// ─────────────────────────────────────────────────────────────────────────────

/// Parse a string of ARM assembly into a vector of instructions.
fn parse_raw(src: &str) -> Result<Vec<Inst>, ParseError> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenise_all()?;
    let mut parser = Parser::new(tokens);
    parser.parse_code()
}

#[derive(Clone, Copy)]
enum ParsedArg {
    Reg(u8),
    Imm(i64),
}

fn parse_cond_code(cond: &str) -> Result<CondCode, ParseError> {
    match cond {
        "" => Ok(CondCode::Al),
        "eq" => Ok(CondCode::Eq),
        "ne" => Ok(CondCode::Ne),
        "cs" => Ok(CondCode::Cs),
        "cc" => Ok(CondCode::Cc),
        "mi" => Ok(CondCode::Mi),
        "pl" => Ok(CondCode::Pl),
        "vs" => Ok(CondCode::Vs),
        "vc" => Ok(CondCode::Vc),
        "hi" => Ok(CondCode::Hi),
        "ls" => Ok(CondCode::Ls),
        "ge" => Ok(CondCode::Ge),
        "lt" => Ok(CondCode::Lt),
        "gt" => Ok(CondCode::Gt),
        "le" => Ok(CondCode::Le),
        _ => Err(ParseError {
            message: format!("unsupported condition code suffix '{cond}'"),
            line: 0,
            col: 0,
        }),
    }
}

fn parse_shift_amount<WShift: Clone + From<usize> + Into<usize>>(
    amount: i64,
    op: &str,
) -> Result<WShift, ParseError> {
    let amount_orig = amount;
    if amount < 0 {
        return Err(ParseError {
            message: format!("shift amount is '{amount_orig}, but shift amount cannot be negative"),
            line: 0,
            col: 0,
        });
    }
    let amount = WShift::from(amount as usize);
    if amount.clone().into() != amount_orig as usize {
        return Err(ParseError {
            message: format!("invalid shift amount '{amount_orig}' for {op}"),
            line: 0,
            col: 0,
        });
    }
    Ok(amount)
}

fn parse_shift_code<WShift: Clone + From<usize> + Into<usize>>(
    shift: &str,
    parsed_args: &mut Vec<ParsedArg>,
) -> Result<ShiftCode<WShift>, ParseError> {
    let shift = if shift == "asl" { "lsl" } else { shift };
    if shift.is_empty() {
        return Ok(ShiftCode::None);
    }
    if shift == "rrx" {
        return Ok(ShiftCode::Rrx);
    }
    let Some(last) = parsed_args.pop() else {
        return Err(ParseError {
            message: format!("missing shift amount for '{shift}'"),
            line: 0,
            col: 0,
        });
    };
    let ParsedArg::Imm(amount) = last else {
        return Err(ParseError {
            message: format!("shift amount must be immediate for '{shift}'"),
            line: 0,
            col: 0,
        });
    };
    let amount = parse_shift_amount(amount, shift)?;
    match shift {
        "asr" => Ok(ShiftCode::Asr(amount)),
        "lsl" => Ok(ShiftCode::Lsl(amount)),
        "lsr" => Ok(ShiftCode::Lsr(amount)),
        "ror" => Ok(ShiftCode::Ror(amount)),
        _ => Err(ParseError {
            message: format!("unsupported shift op '{shift}'"),
            line: 0,
            col: 0,
        }),
    }
}

fn parse_reg(s: &str) -> Option<u8> {
    let s = match s {
        "sb" => "r9",
        "sl" => "r10",
        "fp" => "r11",
        "ip" => "r12",
        "sp" => "r13",
        "lr" => "r14",
        "pc" => "r15",
        _ => s,
    };
    if let Some(num) = s.strip_prefix('r') {
        let reg = num.parse::<u8>().ok()?;
        if reg < 16 {
            return Some(reg);
        }
    }
    None
}

fn parse_old_arg(s: &str) -> Option<ParsedArg> {
    parse_reg(s)
        .map(ParsedArg::Reg)
        .or_else(|| s.parse::<i64>().ok().map(ParsedArg::Imm))
}

fn opcode_matches_args(op_code: OpCode, args: &[ParsedArg]) -> bool {
    let mut arg_i = 0usize;
    for arg_type in op_code.arg_types() {
        match arg_type {
            ArgType::Unused => {}
            ArgType::Reg(_) => {
                if !matches!(args.get(arg_i), Some(ParsedArg::Reg(_))) {
                    return false;
                }
                arg_i += 1;
            }
            ArgType::Imm => {
                if !matches!(args.get(arg_i), Some(ParsedArg::Imm(_))) {
                    return false;
                }
                arg_i += 1;
            }
        }
    }
    arg_i == args.len()
}

fn map_opcode(op: &str, args: &[ParsedArg]) -> Result<OpCode, ParseError> {
    let candidates: Vec<OpCode> = OpCode::ALL
        .iter()
        .copied()
        .filter(|candidate| candidate.as_str() == op)
        .collect();

    if candidates.is_empty() {
        return Err(ParseError {
            message: format!("unsupported opcode '{op}'"),
            line: 0,
            col: 0,
        });
    }

    let matches: Vec<OpCode> = candidates
        .into_iter()
        .filter(|candidate| opcode_matches_args(*candidate, args))
        .collect();

    match matches.as_slice() {
        [op_code] => Ok(*op_code),
        [] => Err(ParseError {
            message: format!("unsupported operands for opcode '{op}'"),
            line: 0,
            col: 0,
        }),
        _ => Err(ParseError {
            message: format!("ambiguous opcode '{op}' for provided operands"),
            line: 0,
            col: 0,
        }),
    }
}

fn translate_inst<W: Word, WShift: Word>(inst: &Inst) -> Result<arm::Inst<W, WShift>, ParseError> {
    if inst.is_hole() {
        return Err(ParseError {
            message: "cannot translate hole instruction".into(),
            line: 0,
            col: 0,
        });
    }
    if inst.op.len() != 3 {
        return Err(ParseError {
            message: "invalid old Inst.op shape".into(),
            line: 0,
            col: 0,
        });
    }
    let cond_code = parse_cond_code(&inst.op[1])?;
    let mut parsed_args: Vec<ParsedArg> = inst
        .args
        .iter()
        .map(|a| {
            parse_old_arg(a).ok_or_else(|| ParseError {
                message: format!("invalid operand '{a}'"),
                line: 0,
                col: 0,
            })
        })
        .collect::<Result<_, _>>()?;
    let shift_code = parse_shift_code(&inst.op[2], &mut parsed_args)?;
    let op_code = map_opcode(&inst.op[0], &parsed_args)?;
    let arg_types = op_code.arg_types();
    let mut args = [W::from(0usize); 3];

    for (i, arg_type) in arg_types.iter().enumerate() {
        match (arg_type, parsed_args.get(i).copied()) {
            (ArgType::Unused, _) => {}
            (ArgType::Reg(_), Some(ParsedArg::Reg(r))) => args[i] = (r as usize).into(),
            (ArgType::Imm, Some(ParsedArg::Imm(n))) => args[i] = (n as usize).into(),
            (ArgType::Reg(_), Some(ParsedArg::Imm(_))) => {
                return Err(ParseError {
                    message: format!("expected register for argument {}", i + 1),
                    line: 0,
                    col: 0,
                });
            }
            (ArgType::Imm, Some(ParsedArg::Reg(_))) => {
                return Err(ParseError {
                    message: format!("expected immediate for argument {}", i + 1),
                    line: 0,
                    col: 0,
                });
            }
            (_, None) => {
                return Err(ParseError {
                    message: "too few operands".into(),
                    line: 0,
                    col: 0,
                });
            }
        }
    }

    let used_arg_count = arg_types
        .iter()
        .filter(|k| !matches!(k, ArgType::Unused))
        .count();
    if parsed_args.len() > used_arg_count {
        return Err(ParseError {
            message: "too many operands".into(),
            line: 0,
            col: 0,
        });
    }

    Ok(arm::Inst {
        op_code,
        cond_code,
        shift: shift_code,
        args,
    })
}

/// Parse to the old stringy Inst format, then translate each instruction to typed arm::Inst.
pub fn parse<W: Word, WShift: Word>(src: &str) -> Result<Vec<arm::Inst<W, WShift>>, ParseError> {
    parse_raw(src)?
        .iter()
        .map(translate_inst::<W, WShift>)
        .collect()
}

/// Corresponds to `liveness-from-file` in the original.
///
/// Each line has the form `<key>:<n1>,<n2>,...`; the result maps the key
/// string to a list of register indices (parsed as usize).
pub fn liveness_from_file(
    path: &str,
) -> Result<HashMap<String, Vec<usize>>, Box<dyn std::error::Error>> {
    let content = fs::read_to_string(path)?;
    let mut map = HashMap::new();
    for line in content.lines() {
        if let Some(colon_pos) = line.find(':') {
            let key = &line[..colon_pos];
            let rest = &line[colon_pos + 1..];
            let regs: Vec<usize> = rest
                .split(',')
                .filter_map(|s| s.trim().parse::<usize>().ok())
                .collect();
            map.insert(key.to_owned(), regs);
        }
    }
    Ok(map)
}

/// Corresponds to `info-from-file` in the original.
///
/// Reads the first line of the file and parses it as a comma-separated list
/// of live-out values: each element is either an integer or a named register.
pub fn info_from_file(path: &str) -> Result<Vec<LiveValue>, Box<dyn std::error::Error>> {
    let content = fs::read_to_string(path)?;
    let first = content.lines().next().unwrap_or("");
    let values = first
        .split(',')
        .map(|s| {
            let s = s.trim();
            if let Ok(n) = s.parse::<i64>() {
                LiveValue::Num(n)
            } else {
                LiveValue::Name(s.to_owned())
            }
        })
        .collect();
    Ok(values)
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{CondCode, OpCode, ShiftCode, Word4};

    fn op(inst: &Inst) -> (&str, &str, &str) {
        (&inst.op[0], &inst.op[1], &inst.op[2])
    }

    #[test]
    fn test_simple_mov() {
        let insts = parse_raw("mov r0, r1").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(op(&insts[0]), ("mov", "", ""));
        assert_eq!(insts[0].args, vec!["r0", "r1"]);
    }

    #[test]
    fn test_nop() {
        let insts = parse_raw("nop").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(op(&insts[0]), ("nop", "", ""));
        assert!(insts[0].args.is_empty());
    }

    #[test]
    fn test_hole() {
        let insts = parse_raw("?").unwrap();
        assert_eq!(insts.len(), 1);
        assert!(insts[0].is_hole());
    }

    #[test]
    fn test_cond_suffix() {
        let insts = parse_raw("moveq r0, r1").unwrap();
        assert_eq!(op(&insts[0]), ("mov", "eq", ""));
    }

    #[test]
    fn test_shift_fold() {
        // add r0, r1, r2, lsl #2  →  op = ["add", "", "lsl"], args = ["r0","r1","r2","2"]
        let insts = parse_raw("add r0, r1, r2, lsl #2").unwrap();
        assert_eq!(op(&insts[0]), ("add", "", "lsl"));
        assert_eq!(insts[0].args, vec!["r0", "r1", "r2", "2"]);
    }

    #[test]
    fn test_ldr_offset_scaling() {
        // ldr r0, [r1, #8]  → offset 8/4 = 2
        let insts = parse_raw("ldr r0, [r1, #8]").unwrap();
        assert_eq!(op(&insts[0]), ("ldr", "", ""));
        assert_eq!(insts[0].args, vec!["r0", "r1", "2"]);
    }

    #[test]
    fn test_rename_fp() {
        let insts = parse_raw("mov r0, fp").unwrap();
        assert_eq!(insts[0].args, vec!["r0", "r11"]);
    }

    #[test]
    fn test_special_inst_sdiv() {
        let insts = parse_raw("bl __aeabi_idiv").unwrap();
        assert_eq!(op(&insts[0]), ("sdiv", "", ""));
        assert_eq!(insts[0].args, vec!["r0", "r0", "r1"]);
    }

    #[test]
    fn test_label_and_block_comment_skipped() {
        let src = "main:\n; BB0_1:\nmov r0, r1\n";
        let insts = parse_raw(src).unwrap();
        // Only the mov should survive; labels/block-comments produce no Inst
        assert_eq!(insts.len(), 1);
        assert_eq!(op(&insts[0]), ("mov", "", ""));
    }

    #[test]
    fn test_asl_normalised_to_lsl() {
        let insts = parse_raw("asl r0, r1, #1").unwrap();
        assert_eq!(op(&insts[0]), ("lsl", "", ""));
    }

    #[test]
    fn test_multiple_instructions() {
        let src = "mov r0, #0\nadd r1, r0, r2\nstr r1, [r3, #4]\n";
        let insts = parse_raw(src).unwrap();
        assert_eq!(insts.len(), 3);
    }

    #[test]
    fn test_parse_simple_mov() {
        let insts = parse::<Word4, Word2>("mov r0, r1").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Mov);
        assert_eq!(insts[0].cond_code, CondCode::Al);
        assert_eq!(insts[0].shift, ShiftCode::None);
        assert_eq!(usize::from(insts[0].args[0]), 0);
        assert_eq!(usize::from(insts[0].args[1]), 1);
    }

    #[test]
    fn test_parse_preserves_old_hole_parse_but_rejects_translation() {
        assert!(parse_raw("?").unwrap()[0].is_hole());
        let err = parse::<Word4, Word2>("?").unwrap_err();
        assert!(err.message.contains("hole"));
    }

    #[test]
    fn test_parse_accepts_shift_folded_inst() {
        let insts = parse::<Word4, Word2>("add r0, r1, r2, lsl #2").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Add);
        assert_eq!(insts[0].shift, ShiftCode::Lsl(2.into()));
    }

    #[test]
    fn test_parse_rejects_invalid_shift_amount() {
        let err = parse::<Word4, Word3>("add r0, r1, r2, lsl #17").unwrap_err();
        dbg!(&err);
        assert!(err.message.contains("invalid shift amount"));
    }

    #[test]
    fn test_parse_mov_with_shift() {
        let insts = parse::<Word4, Word2>("mov r0, r1, rrx").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Mov);
        assert_eq!(insts[0].shift, ShiftCode::Rrx);
    }

    #[test]
    fn test_parse_add_with_rrx_shift() {
        let raw = parse_raw("add r0, r1, r2, rrx").unwrap();
        assert_eq!(op(&raw[0]), ("add", "", "rrx"));
        assert_eq!(raw[0].args, vec!["r0", "r1", "r2"]);

        let insts = parse::<Word4, Word2>("add r0, r1, r2, rrx").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Add);
        assert_eq!(insts[0].shift, ShiftCode::Rrx);
    }

    #[test]
    fn test_parse_movt() {
        let insts = parse::<Word4, Word2>("movt r0, #1").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Movt);
        assert_eq!(insts[0].cond_code, CondCode::Al);
    }

    #[test]
    fn test_parse_movt_with_condition() {
        let insts = parse::<Word4, Word2>("movteq r0, #1").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Movt);
        assert_eq!(insts[0].cond_code, CondCode::Eq);
    }

    #[test]
    fn test_parse_movw() {
        let insts = parse::<Word4, Word2>("movw r0, #1").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Movw);
        assert_eq!(insts[0].cond_code, CondCode::Al);
    }

    #[test]
    fn test_parse_movw_with_condition() {
        let insts = parse::<Word4, Word2>("movweq r0, #1").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Movw);
        assert_eq!(insts[0].cond_code, CondCode::Eq);
    }

    #[test]
    fn test_parse_rsb() {
        let insts = parse::<Word4, Word2>("rsb r0, r1, r2").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Rsb);
    }

    #[test]
    fn test_parse_rsbi() {
        let insts = parse::<Word4, Word2>("rsb r0, r1, #3").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::RsbI);
    }

    #[test]
    fn test_parse_bic() {
        let insts = parse::<Word4, Word2>("bic r0, r1, r2").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(insts[0].op_code, OpCode::Bic);
    }

    #[test]
    fn test_parse_tst_and_tsti_from_opcode_table() {
        let reg = parse::<Word4, Word2>("tst r0, r1").unwrap();
        assert_eq!(reg.len(), 1);
        assert_eq!(reg[0].op_code, OpCode::Tst);

        let imm = parse::<Word4, Word2>("tst r0, #1").unwrap();
        assert_eq!(imm.len(), 1);
        assert_eq!(imm[0].op_code, OpCode::TstI);
    }

    #[test]
    fn test_parse_rejects_rrx_with_amount() {
        let err = parse::<Word4, Word2>("mov r0, r1, rrx, #1").unwrap_err();
        assert!(
            err.message.contains("invalid operand") || err.message.contains("too many operands")
        );
    }

    #[test]
    fn test_parse_rejects_mov_rrx_without_source_reg() {
        let err = parse::<Word4, Word2>("mov r0, rrx").unwrap_err();
        assert!(
            err.message.contains("invalid operand")
                || err.message.contains("unsupported mov operands")
        );
    }

    #[test]
    fn test_parse_rejects_negative_shift_amount() {
        let err = parse::<Word4, Word2>("add r0, r1, r2, lsl #-1").unwrap_err();
        dbg!(&err);
        assert!(err.message.contains("negative"));
    }

    #[test]
    fn test_parse_rejects_register_shift_amount() {
        let err = parse::<Word4, Word2>("add r0, r1, r2, lsl r3").unwrap_err();
        assert!(err.message.contains("shift amount must be immediate"));
    }

    #[test]
    fn test_parse_folded_asl_becomes_lsl_shift_code() {
        let insts = parse::<Word4, Word2>("add r0, r1, r2, asl #2").unwrap();
        assert_eq!(insts[0].shift, ShiftCode::Lsl(2.into()));
    }

    #[test]
    fn test_parse_conditional_with_shift() {
        let insts = parse::<Word4, Word2>("addeq r0, r1, r2, lsr #3").unwrap();
        assert_eq!(insts[0].op_code, OpCode::Add);
        assert_eq!(insts[0].cond_code, CondCode::Eq);
        assert_eq!(insts[0].shift, ShiftCode::Lsr(3.into()));
    }
}
