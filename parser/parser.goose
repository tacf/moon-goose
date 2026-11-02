import std;
import lexer.lexer;

enum BinKind { Add, Sub, Mul, Div }

// Children are indices into the node array
enum Node {
    IntLit   { v: i64 },
    FloatLit { v: f64 },
    NilLit,
    TrueLit,
    FalseLit,
    Neg      { operand: i64 },
    Bin      { kind: BinKind, left: i64, right: i64 },
    Print    { expr: i64 },
}

// Recursive descent. Grammar:
//   program := ("print" "(" expr ")")* Eof
//   expr    := term (("+" | "-") term)*
//   term    := unary (("*" | "/") unary)*
//   unary   := "-" unary | primary
//   primary := Integer | Float | true | false | nil | "(" expr ")"
// Returns the node array and the root node of each statement.
// Any error returns from parse with a message
fn parse(src: u8[:]) -> Node[>..], i64[>..], u8[] {
    let tokens, err = lex(src);
    guard err.len == 0 else { return [], [], err; }
    var nodes: Node[>..] = [];
    var roots: i64[>..] = [];
    var pos = 0;

    fn peek(k: Kind) -> bool { return tokens[pos].kind == k; }

    fn peek() -> Kind { return tokens[pos].kind; }

    fn text(t: Token) -> u8[:] { return src[t.start..t.start + t.len]; }

    fn add(n: Node) -> i64 {
        nodes.push(n);
        return nodes.len - 1;
    }

    fn expect(k: Kind, what: u8[:]) {
        guard peek(k) else {
            let t = tokens[pos];
            return [], [], error_at(src, t.start, str("expected ", what)) from parse;
        }
        pos++;
    }

    recursive fn parse_expr() -> i64 {
        return parse_expr_precedence(0);
    }

    recursive fn parse_expr_precedence(min_prec: i64) -> i64 {
        var left = parse_unary();

        loop {
            var kind: BinKind;
            var prec: i64;

            match peek() {
                Plus =>  { kind = BinKind.Add; },
                Minus => { kind = BinKind.Sub; },
                Star =>  { kind = BinKind.Mul; },
                Slash => { kind = BinKind.Div; },
                _ => { break; }
            }
            
            if (kind == BinKind.Div || kind == BinKind.Mul) { prec = 2; } else { prec = 1; }

            if prec < min_prec {
                break;
            }

            pos++;

            let right = parse_expr_precedence(prec + 1);

            left = add(Node.Bin {
                kind: kind,
                left: left,
                right: right
            });
        }

        return left;
    }

    recursive fn parse_unary() -> i64 {
        if peek(Kind.Minus) {
            pos++;
            let operand = parse_unary();
            return add(Node.Neg { operand: operand });
        }
        return parse_primary();
    }

    recursive fn parse_primary() -> i64 {
        let t = tokens[pos];
        match t.kind {
            Integer => { pos++; return add(Node.IntLit { v: t.ival }); },
            Float => { pos++; return add(Node.FloatLit { v: t.fval }); },
            Ident => {
                pos++;
                let word = text(t);
                if word == "true" { return add(Node.TrueLit); }
                if word == "false" { return add(Node.FalseLit); }
                if word == "nil" { return add(Node.NilLit); }
                return [], [], error_at(src, t.start, str("unknown name '", word, "'")) from parse;
            },
            LParen => {
                pos++;
                let e = parse_expr();
                expect(Kind.RParen, "')'");
                return e;
            },
            _ => {
                return [], [], error_at(src, t.start, str("unexpected token ", kind_name(t.kind))) from parse;
            }
        }
    }

    while !peek(Kind.Eof) {
        let t = tokens[pos];
        guard peek(Kind.Ident) && text(t) == "print" else {
            return [], [], error_at(src, t.start, "expected 'print'") from parse;
        }
        pos++;
        expect(Kind.LParen, "'('");
        let e = parse_expr();
        expect(Kind.RParen, "')'");
        roots.push(add(Node.Print { expr: e }));
    }
    return nodes, roots, "";
}

fn bin_name(k: BinKind) -> u8[:] {
    match k {
        Add => "Add",
        Sub => "Sub",
        Mul => "Mul",
        Div => "Div",
    }
}

// Appends the subtree at node i to out, one node per line, indented by depth
recursive fn dump_node(out: u8[>..]&, nodes: Node[>..]&, i: i64, depth: i64) {
    var d = 0;
    while d < depth { format(out, "  "); d++; }
    match nodes[i] {
        IntLit n => { format(out, "Integer(", n.v, ")\n"); },
        FloatLit n => { format(out, "Float(", n.v, ")\n"); },
        NilLit => { format(out, "Nil\n"); },
        TrueLit => { format(out, "True\n"); },
        FalseLit => { format(out, "False\n"); },
        Neg n => { format(out, "Neg\n"); dump_node(out, nodes, n.operand, depth + 1); },
        Bin n => {
            format(out, bin_name(n.kind), "\n");
            dump_node(out, nodes, n.left, depth + 1);
            dump_node(out, nodes, n.right, depth + 1);
        },
        Print n => { format(out, "Print\n"); dump_node(out, nodes, n.expr, depth + 1); },
    }
}

fn dump_ast(nodes: Node[>..]&, roots: i64[>..]&) {
    var out: u8[>..] = [];
    for r in roots { dump_node(out, nodes, r, 0); }
    print(out);
}
