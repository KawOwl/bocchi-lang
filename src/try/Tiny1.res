module Nameable = {
  type rec expr =
    | Cst(int)
    | Var(string)
    | Let(string, expr, expr)
    | Add(expr, expr)
    | Mul(expr, expr)

  type rec env = list<(string, int)>

	let rec ctnl = (expr): Tiny2.Nameless.expr => {
		switch expr {
		| Cst(n) => Tiny2.Nameless.Cst(n)
		| Var(x) => Tiny2.Nameless.Var(0)
		| Let(x, e1, e2) => Tiny2.Nameless.Let(ctnl(e1), ctnl(e2))
		| Add(e1, e2) => Tiny2.Nameless.Add(ctnl(e1), ctnl(e2))
		| Mul(e1, e2) => Tiny2.Nameless.Mul(ctnl(e1), ctnl(e2))
		}
	}

	let compileToNameless = ctnl
}

