type instr =
  | Cst(int)
  | Add
  | Mul
type instrs = list<instr>
type operand = int
type stack = list<operand>

let rec eval = (instrs, stack) => {
  switch (instrs, stack) {
  | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
  | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
  | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
  | (list{}, list{a, ..._}) => a
  | _ => assert false
  }
}