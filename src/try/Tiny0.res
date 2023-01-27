type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)

let rec eval1 = expr => {
  switch expr {
  | Cst(n) => n
  | Add(e1, e2) => eval1(e1) + eval1(e2)
  | Mul(e1, e2) => eval1(e1) * eval1(e2)
  }
}

let rec cts0 = (expr: expr): SM0.instrs => {
  switch expr {
  | Cst(n) => list{Cst(n)}
  | Add(e1, e2) => Belt.List.concatMany([cts0(e1), cts0(e2), list{Add}])
  | Mul(e1, e2) => Belt.List.concatMany([cts0(e1), cts0(e2), list{Mul}])
  }
}

let compileToStackMachine0 = cts0
