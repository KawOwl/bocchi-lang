module Nameable = {
  type rec expr =
    | Cst(int)
    | Var(string)
    | Let(string, expr, expr)
    | Add(expr, expr)
    | Mul(expr, expr)

  type env = list<(string, int)>

  let rec eval = (expr, env) => {
    switch expr {
    | Cst(i) => i
    | Add(a, b) => eval(a, env) + eval(b, env)
    | Mul(a, b) => eval(a, env) * eval(b, env)
    | Var(id) => List.assoc(id, env)
    | Let(id, e1, e2) => eval(e2, list{(id, eval(e1, env)), ...env})
    }
  }

  type cenv = list<string>

  let rec ctnl = (expr, cenv): Tiny2.Nameless.expr => {
    switch expr {
    | Cst(i) => Tiny2.Nameless.Cst(i)
    | Add(e1, e2) => Tiny2.Nameless.Add(ctnl(e1, cenv), ctnl(e2, cenv))
    | Mul(e1, e2) => Tiny2.Nameless.Mul(ctnl(e1, cenv), ctnl(e2, cenv))
    | Var(id) => Tiny2.Nameless.Var(Belt.Option.getExn(Util.findIndexInList(cenv, id)))
    | Let(id, e1, e2) => Tiny2.Nameless.Let(ctnl(e1, cenv), ctnl(e2, list{id, ...cenv}))
    }
  }

  let compileToNameless = ctnl
}
