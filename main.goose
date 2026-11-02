
enum Type {
    Nil,
    Boolean     { value: bool },
    Integer     { value: i64 },
    Float       { value: f64 },
    StringPtr   { value: i64 },
    TablePtr    { value: i64 }
}

enum Op {
    PUSH { value: Type }, 
    ADD, 
    MUL,
    DIV, 
    SUB, 
    PUSH_NIL,
    PUSH_TRUE,
    PUSH_FALSE,
    AND,
    OR,
    NOT,
    PRINT, 
    HALT
}

enum ArithOp { Add, Sub, Mul, Div }

enum BoolOp { And, Or, Not }


type InstructionSet = Op..[>..];
type VMStack = Type[>..<];

// TODO: For efficiency this may be bad, possibly check pop() error
// instead
fn ensure_stack_size(vmStack: Type[>..<]&, size: i64){
    if vmStack.len < size {
        abort("Error: vmStack underflow");
    }
}


// Numeric value as f64, aborts on non-numeric
fn as_float(v: Type) -> f64 {
    match v {
        Integer i => { return i.value; },
        Float f => { return f.value; },
        _ => { abort("Error: ADD op over non-numerical value"); }
    }
    return 0.0;
}


fn as_int(v: Type) -> i64 {
    match v {
        Integer i => { return i.value; },
        _ => { abort("Error: expected Integer"); }
    }
    return 0;
}


// Lua truthiness, only Nil and false are falsy
fn is_truthy(v: Type) -> bool {
    match v {
        Nil => { return false; },
        Boolean b => { return b.value; },
        _ => { return true; }
    }
    return true;
}


// Lua semantics. And/Or pop b then a and push one of the operands. Not pops one and pushes a Boolean
fn bool_op(vm_stack: Type[>..<]&, kind: BoolOp) {
    match kind {
        And => {
            ensure_stack_size(vm_stack, 2);
            let b = vm_stack.pop();
            let a = vm_stack.pop();
            // a and b: a if a is falsy, else b
            if is_truthy(a) { vm_stack.push(b); } else { vm_stack.push(a); }
        },
        Or => {
            ensure_stack_size(vm_stack, 2);
            let b = vm_stack.pop();
            let a = vm_stack.pop();
            // a or b: a if a is truthy, else b
            if is_truthy(a) { vm_stack.push(a); } else { vm_stack.push(b); }
        },
        Not => {
            ensure_stack_size(vm_stack, 1);
            let a = vm_stack.pop();
            vm_stack.push(Type.Boolean { value: !is_truthy(a) });
        }
    }
}

// Pops b then a, pushes a <kind> b. Float if either side is Float
fn arith_op(vm_stack: Type[>..<]&, kind: ArithOp) {
    ensure_stack_size(vm_stack, 2);
    let b = vm_stack.pop();
    let a = vm_stack.pop();
    var is_out_float = false;

    match a {
        Float => { is_out_float = true; },
        Integer => {},
        _ => abort("Error: arithmetic op over non-numerical value")
    }
    match b {
        Float => { is_out_float = true; },
        Integer => {},
        _ => abort("Error: arithmetic op over non-numerical value")
    }

    if is_out_float {
        let x = as_float(a);
        let y = as_float(b);
        var r = 0.0;
        match kind {
            Add => { r = x + y; },
            Sub => { r = x - y; },
            Mul => { r = x * y; },
            Div => {
                if y == 0.0 { abort("Error: DIV by 0 (zero)"); }
                r = x / y;
            }
        }
        vm_stack.push(Type.Float { value: r });
    } else {
        let x = as_int(a);
        let y = as_int(b);
        var r = 0;
        match kind {
            Add => { r = x + y; },
            Sub => { r = x - y; },
            Mul => { r = x * y; },
            Div => {
                if y == 0 { abort("Error: DIV by 0 (zero)"); }
                r = x / y;
            }
        }
        vm_stack.push(Type.Integer { value: r });
    }
}


fn load_code() -> InstructionSet {
    var code: Op..[>..] = [];

    code.push(Op.PUSH { value: Type.Integer { value: 10 }});
    code.push(Op.PUSH { value: Type.Integer { value: 20 }});
    code.push(Op.ADD);
    code.push(Op.PUSH { value: Type.Integer { value: 30 }});
    code.push(Op.ADD);
    code.push(Op.PUSH { value: Type.Integer { value: 2 }});
    code.push(Op.MUL);
    code.push(Op.PUSH { value: Type.Integer { value: 2 }});
    code.push(Op.DIV);
    code.push(Op.PRINT);
    code.push(Op.HALT);

    return code;
}

fn main() {
    
    var code = load_code();

    var vmStack: VMStack = [];

    for instruction in code {
        match instruction {
            PUSH &op => vmStack.push(op.value),
            PUSH_NIL => vmStack.push(Type.Nil),
            PUSH_TRUE => vmStack.push(Type.Boolean { value: true}),
            PUSH_FALSE => vmStack.push(Type.Boolean { value: false}),
            ADD => arith_op(vmStack, ArithOp.Add),
            MUL => arith_op(vmStack, ArithOp.Mul),
            DIV => arith_op(vmStack, ArithOp.Div),
            SUB => arith_op(vmStack, ArithOp.Sub),
            AND => bool_op(vmStack, BoolOp.And),
            OR => bool_op(vmStack, BoolOp.Or),
            NOT => bool_op(vmStack, BoolOp.Not),
            PRINT => {
                ensure_stack_size(vmStack, 1);
                print(vmStack.pop());
            },
            HALT => exit(0)
        }
    }
    print("Error: VM exited without a HALT operation")
}
