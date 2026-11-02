import std;

enum Kind {
    Ident,
    Integer,
    Float,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    Eof,
}

// start and len index into the source. ival is set for Integer, fval for Float
struct Token {
    kind: Kind,
    start: i64,
    len: i64,
    ival: i64,
    fval: f64,
}

fn is_digit(c: u8) -> bool { return c >= '0' && c <= '9'; }

fn is_alpha(c: u8) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

fn kind_name(k: Kind) -> u8[:] {
    match k {
        Ident => "Ident",
        Integer => "Integer",
        Float => "Float",
        Plus => "Plus",
        Minus => "Minus",
        Star => "Star",
        Slash => "Slash",
        LParen => "LParen",
        RParen => "RParen",
        Eof => "Eof",
    }
}

// Single char tokens. Extend here for new operators
fn single_kind(c: u8) -> Kind, bool {
    if c == '+' { return Kind.Plus, true; }
    if c == '-' { return Kind.Minus, true; }
    if c == '*' { return Kind.Star, true; }
    if c == '/' { return Kind.Slash, true; }
    if c == '(' { return Kind.LParen, true; }
    if c == ')' { return Kind.RParen, true; }
    return Kind.Eof, false;
}

// Scans src into tokens, ending with Eof. On a bad character returns an
// error message with line, column and a caret under the offender
fn lex(src: u8[:]) -> Token[>..], u8[] {
    var tokens: Token[>..] = [];
    var i = 0;
    while i < src.len {
        let c = src[i];

        // whitespace
        if c == ' ' || c == '\t' || c == '\n' || c == '\r' { i++; continue; }

        // number: digits, optionally followed by '.' and more digits
        if is_digit(c) {
            let start = i;
            while i < src.len && is_digit(src[i]) { i++; }
            var is_float = false;
            if i + 1 < src.len && src[i] == '.' && is_digit(src[i + 1]) {
                is_float = true;
                i++;
                while i < src.len && is_digit(src[i]) { i++; }
            }
            let text = src[start..i];
            if is_float {
                let f, _ = parse_flt(text);
                tokens.push(Token { kind: Kind.Float, start: start, len: i - start, ival: 0, fval: f });
            } else {
                let n, ok = parse_int(text);
                guard ok else { return tokens, str("integer literal too large '", text, "'"); }
                tokens.push(Token { kind: Kind.Integer, start: start, len: i - start, ival: n, fval: 0.0 });
            }
            continue;
        }

        // identifier
        if is_alpha(c) {
            let start = i;
            while i < src.len && (is_alpha(src[i]) || is_digit(src[i])) { i++; }
            tokens.push(Token { kind: Kind.Ident, start: start, len: i - start, ival: 0, fval: 0.0 });
            continue;
        }

        // operators and parens
        let k, ok = single_kind(c);
        guard ok else { return tokens, error_at(src, i, str("unexpected character '", src[i..i + 1], "'")); }
        tokens.push(Token { kind: k, start: i, len: 1, ival: 0, fval: 0.0 });
        i++;
    }
    tokens.push(Token { kind: Kind.Eof, start: src.len, len: 0, ival: 0, fval: 0.0 });
    return tokens, "";
}

// "line 1, column 11: <what>" plus the source line and a caret under byte offset at.
// Shared by the lexer and the parser
fn error_at(src: u8[:], at: i64, what: u8[:]) -> u8[] {
    var line = 1;
    var line_start = 0;
    var j = 0;
    while j < at {
        if src[j] == '\n' { line++; line_start = j + 1; }
        j++;
    }
    var line_end = line_start;
    while line_end < src.len && src[line_end] != '\n' { line_end++; }

    var msg: u8[>..] = [];

    format(msg, "line ", line, ", column ", at - line_start + 1, ": ", what, "\n",
        src[line_start..line_end], "\n");

    var col = 0;
    while col < at - line_start { msg.push(' '); col++; }
    msg.push('^');
    return msg;
}

fn dump(src: u8[:]) {
    let tokens, err = lex(src);
    print("source: ", src);
    for t in tokens {
        print("  ", t.start, "\t", kind_name(t.kind), "\t'", src[t.start..t.start + t.len], "'");
    }
    if err.len > 0 { print(err); }
}
