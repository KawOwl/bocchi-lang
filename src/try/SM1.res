type instr =
  | Cst(int)
  | Add
  | Mul
  | Var(int)
  | Pop
  | Swap
type instrs = list<instr>
type operand = int
type stack = list<operand>

let rec eval = (instrs, stack) => {
  // Js.log3("SM1", instrs, stack)
  switch (instrs, stack) {
  | (list{Cst(i), ...rest}, _) => eval(rest, list{i, ...stack})
  | (list{Add, ...rest}, list{a, b, ...stk}) => eval(rest, list{a + b, ...stk})
  | (list{Mul, ...rest}, list{a, b, ...stk}) => eval(rest, list{a * b, ...stk})
  | (list{Var(i), ...rest}, stk) => eval(
      rest,
      list{Belt.Option.getExn(Belt.List.get(stk, i)), ...stk},
    )

  | (list{Pop, ...rest}, list{_, ...stk}) => eval(rest, stk)
  | (list{Swap, ...rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
  | (list{}, list{top, ..._}) => top
  | _ => assert false
  }
}
