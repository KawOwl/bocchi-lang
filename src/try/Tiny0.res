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

let rec compile = (expr: expr): SM0.instrs => {
  switch expr {
  | Cst(n) => list{Cst(n)}
  | Add(e1, e2) => Belt.List.concatMany([compile(e1), compile(e2), list{Add}])
  | Mul(e1, e2) => Belt.List.concatMany([compile(e1), compile(e2), list{Mul}])
  }
}
