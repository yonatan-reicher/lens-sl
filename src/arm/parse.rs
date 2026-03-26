/// This function parses an input ARM program into our in-code representation.
// Code taken from `arm-parser.rkt` at `https://github.com/mangpo/greenthumb`, the original Lens
// implementation.

/// ARM assembly parser — translated from Racket (parser-tools/lex + yacc) to
/// hand-rolled Rust (no external dependencies).
///
/// Mirrors the original's data model exactly:
///   - `Inst { op: Vec<String>, args: Vec<String> }`
///     where op is always `[opcode, cond_suffix, shift_op]` (empty string when
///     not present) and args is the operand list.
///   - A hole instruction is represented with all fields empty (op=[], args=[]).
///
/// Public surface:
///   - `parse(src: &str) -> Result<Vec<Inst>, ParseError>`
///   - `liveness_from_file(path: &str) -> Result<HashMap<String,Vec<usize>>, ...>`
///   - `info_from_file(path: &str) -> Result<Vec<LiveValue>, ...>`
use std::collections::HashMap;
use std::fmt;
use std::fs;

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
            TokenKind::Eof => return Ok(None),
            // Labels and .text directives don't produce instructions; skip them.
            TokenKind::Label | TokenKind::Block | TokenKind::Text => {
                self.advance();
                return Ok(Some(Inst::hole())); // filtered out by caller
                // Actually we want to skip, not emit a hole — see parse_code.
            }
            TokenKind::Hole => {
                self.advance();
                return Ok(Some(Inst::hole()));
            }
            TokenKind::Nop => {
                self.advance();
                return Ok(Some(create_inst("nop", vec![])?));
            }
            TokenKind::Word => {
                let op_tok = self.advance().clone();
                match self.peek().kind {
                    TokenKind::UWord => {
                        let uw = self.advance().clone();
                        return Ok(Some(create_special_inst(&op_tok.value, &uw.value)?));
                    }
                    TokenKind::Eof
                    | TokenKind::Word   // next instruction
                    | TokenKind::Nop
                    | TokenKind::Hole
                    | TokenKind::Label
                    | TokenKind::Block
                    | TokenKind::Text => {
                        // zero-arg instruction
                        return Ok(Some(create_inst(&op_tok.value, vec![])?));
                    }
                    _ => {
                        let args = self.parse_args()?;
                        return Ok(Some(create_inst(&op_tok.value, args)?));
                    }
                }
            }
            _ => {
                let t = self.peek();
                return Err(ParseError {
                    message: format!(
                        "unexpected token {:?} ('{}') at start of instruction",
                        t.kind, t.value
                    ),
                    line: t.line,
                    col: t.col,
                });
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

    fn peek2(&self) -> Option<u8> {
        self.src.get(self.pos + 1).copied()
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
                None => { tokens.push(Token::new(TokenKind::Eof, "", line, col)); break; }
                Some(b',')  => { self.advance(); tokens.push(Token::new(TokenKind::Comma,  ",",  line, col)); }
                Some(b'"')  => { self.advance(); tokens.push(Token::new(TokenKind::Dquote, "\"", line, col)); }
                Some(b'?')  => { self.advance(); tokens.push(Token::new(TokenKind::Hole,   "?",  line, col)); }
                Some(b'#')  => { self.advance(); tokens.push(Token::new(TokenKind::Hash,   "#",  line, col)); }
                Some(b'[')  => { self.advance(); tokens.push(Token::new(TokenKind::Lsqbr,  "[",  line, col)); }
                Some(b']')  => { self.advance(); tokens.push(Token::new(TokenKind::Rsqbr,  "]",  line, col)); }
                Some(b';')  => {
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
    if op == "ldr" || op == "str" {
        if args_len >= 3 {
            let offset = &args[2];
            // Only scale if the offset does NOT start with 'r' (i.e. it is a literal)
            if !offset.starts_with('r') {
                if let Ok(n) = offset.parse::<i64>() {
                    args[2] = (n / 4).to_string();
                }
            }
        }
    }

    let renamed: Vec<String> = args.into_iter().map(|a| rename(a)).collect();

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
pub fn parse(src: &str) -> Result<Vec<Inst>, ParseError> {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenise_all()?;
    let mut parser = Parser::new(tokens);
    parser.parse_code()
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

    fn op(inst: &Inst) -> (&str, &str, &str) {
        (&inst.op[0], &inst.op[1], &inst.op[2])
    }

    #[test]
    fn test_simple_mov() {
        let insts = parse("mov r0, r1").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(op(&insts[0]), ("mov", "", ""));
        assert_eq!(insts[0].args, vec!["r0", "r1"]);
    }

    #[test]
    fn test_nop() {
        let insts = parse("nop").unwrap();
        assert_eq!(insts.len(), 1);
        assert_eq!(op(&insts[0]), ("nop", "", ""));
        assert!(insts[0].args.is_empty());
    }

    #[test]
    fn test_hole() {
        let insts = parse("?").unwrap();
        assert_eq!(insts.len(), 1);
        assert!(insts[0].is_hole());
    }

    #[test]
    fn test_cond_suffix() {
        let insts = parse("moveq r0, r1").unwrap();
        assert_eq!(op(&insts[0]), ("mov", "eq", ""));
    }

    #[test]
    fn test_shift_fold() {
        // add r0, r1, r2, lsl #2  →  op = ["add", "", "lsl"], args = ["r0","r1","r2","2"]
        let insts = parse("add r0, r1, r2, lsl #2").unwrap();
        assert_eq!(op(&insts[0]), ("add", "", "lsl"));
        assert_eq!(insts[0].args, vec!["r0", "r1", "r2", "2"]);
    }

    #[test]
    fn test_ldr_offset_scaling() {
        // ldr r0, [r1, #8]  → offset 8/4 = 2
        let insts = parse("ldr r0, [r1, #8]").unwrap();
        assert_eq!(op(&insts[0]), ("ldr", "", ""));
        assert_eq!(insts[0].args, vec!["r0", "r1", "2"]);
    }

    #[test]
    fn test_rename_fp() {
        let insts = parse("mov r0, fp").unwrap();
        assert_eq!(insts[0].args, vec!["r0", "r11"]);
    }

    #[test]
    fn test_special_inst_sdiv() {
        let insts = parse("bl __aeabi_idiv").unwrap();
        assert_eq!(op(&insts[0]), ("sdiv", "", ""));
        assert_eq!(insts[0].args, vec!["r0", "r0", "r1"]);
    }

    #[test]
    fn test_label_and_block_comment_skipped() {
        let src = "main:\n; BB0_1:\nmov r0, r1\n";
        let insts = parse(src).unwrap();
        // Only the mov should survive; labels/block-comments produce no Inst
        assert_eq!(insts.len(), 1);
        assert_eq!(op(&insts[0]), ("mov", "", ""));
    }

    #[test]
    fn test_asl_normalised_to_lsl() {
        let insts = parse("asl r0, r1, #1").unwrap();
        assert_eq!(op(&insts[0]), ("lsl", "", ""));
    }

    #[test]
    fn test_multiple_instructions() {
        let src = "mov r0, #0\nadd r1, r0, r2\nstr r1, [r3, #4]\n";
        let insts = parse(src).unwrap();
        assert_eq!(insts.len(), 3);
    }
}
