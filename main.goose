
enum Op {
    PUSH { value: i64 }, 
    ADD, 
    MUL,
    DIV, 
    SUB, 
    PRINT, 
    HALT
}

struct Instruction {
    kind: Op,
    value: i64,
}

// TODO: For efficiency this may be bad, possibly check pop() error
// instead
fn ensureStackSize(vmStack: i64[>..<]&, size: i64){
    if vmStack.len < size {
        abort("Error: vmStack underflow");
    }
}

fn main() {
    var code: Op..[>..] = [];

    code.push(Op.PUSH { value: 10});
    code.push(Op.PUSH {value: 20 });
    code.push(Op.ADD);
    code.push(Op.PUSH { value: 30});
    code.push(Op.ADD);
    code.push(Op.PUSH {value: 2});
    code.push(Op.MUL);
    code.push(Op.PUSH {value: 2});
    code.push(Op.DIV);
    code.push(Op.PRINT);
    code.push(Op.HALT);

    var vmStack: i64[>..<] = [];

    for instruction in code {
        match instruction {
            PUSH &op => vmStack.push(op.value),
            ADD => {
                ensureStackSize(vmStack, 2);
                let b = vmStack.pop();
                let a = vmStack.pop();

                vmStack.push(a + b);
            },
            MUL => {
                ensureStackSize(vmStack, 2);
                let b = vmStack.pop();
                let a = vmStack.pop();

                vmStack.push(a * b);
            },
            DIV =>  {
                ensureStackSize(vmStack, 2);
                let b = vmStack.pop();
                if b == 0 { abort("Error: DIV by 0 (zero)")};
                let a = vmStack.pop();

                vmStack.push(a / b);
            },
            SUB => {
                ensureStackSize(vmStack, 2);
                let b = vmStack.pop();
                let a = vmStack.pop();

                vmStack.push(a - b);
            },
            PRINT => {
                ensureStackSize(vmStack, 1);
                print(vmStack.pop());
            },
            HALT => exit(0)
        }
    }
    print("Error: VM exited without a HALT operation")
}
