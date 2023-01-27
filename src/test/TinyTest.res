let testExpr: Tiny0.expr = Add(Add(Cst(1), Cst(2)), Mul(Cst(3), Cst(4)))
let testInstrs = Tiny0.compileToStackMachine0(testExpr)
let iarray = Array.of_list(testInstrs)
Js.log(Tiny0.eval1(testExpr))
Js.log(iarray);
let result = SM0.eval(testInstrs, list{})
Js.log(result)

Js.log(SM1.eval(list{Var(1)}, list{1, 2, 3}))
Js.log(SM1.eval(list{Swap}, list{1, 2}))
Js.log(SM1.eval(list{Pop}, list{1, 2, 3}))