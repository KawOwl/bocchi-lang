module Nameless = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  type env = list<int>

  let rec eval = (expr, env) => {
    switch expr {
    | Cst(i) => i
    | Add(a, b) => eval(a, env) + eval(b, env)
    | Mul(a, b) => eval(a, env) * eval(b, env)
    | Var(n) => List.nth(env, n)
    | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
    }
  }

  let rec cts1 = (expr): SM1.instrs => {
    switch expr {
    | Cst(i) => list{Cst(i)}
    | Add(e1, e2) => Belt.List.concatMany([cts1(e1), cts1(e2), list{Add}])
		| Mul(e1, e2) => Belt.List.concatMany([cts1(e1), cts1(e2), list{Mul}])
    | Var(n) => list{Var(n)}
    | Let(e1, e2) => Belt.List.concatMany([cts1(e1), list{Var(0)}, cts1(e2), list{Swap, Pop}])
    }
  }

  let compileToStackMachine1 = cts1
}