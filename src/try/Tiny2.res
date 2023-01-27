module Nameless = {
  type rec expr =
    | Cst(int)
    | Var(int)
    | Let(expr, expr)
    | Add(expr, expr)
    | Mul(expr, expr)

  type rec env = list<int>

  let rec cts1 = (expr): SM1.instrs => {
    switch expr {
    | Cst(n) => list{Cst(n)}
    | Var(n) => list{}
    | Let(e1, e2) => Belt.List.concatMany([cts1(e1), list{Var(0)}, cts1(e2), list{Swap, Pop}])
    | Add(e1, e2) => Belt.List.concatMany([cts1(e1), cts1(e2), list{Add}])
		| Mul(e1, e2) => Belt.List.concatMany([cts1(e1), cts1(e2), list{Mul}])
    }
  }

  let compileToStackMachine1 = cts1
}