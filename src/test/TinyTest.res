let testExpr: Tiny0.expr = Add(Add(Cst(1), Cst(2)), Mul(Cst(3), Cst(4)))
let testInstrs = Tiny0.compileToStackMachine0(testExpr)
let iarray = Array.of_list(testInstrs)
Js.log(Tiny0.eval(testExpr))
Js.log(iarray);
let result = SM0.eval(testInstrs, list{})
Js.log(result)

Js.log(SM1.eval(list{Var(1)}, list{1, 2, 3}))
Js.log(SM1.eval(list{Swap}, list{1, 2}))
Js.log(SM1.eval(list{Pop}, list{1, 2, 3}))

let tiny1Testinst: Tiny1.Nameable.expr = Let("x", Add(Cst(2), Cst(3)), Add(Var("x"), Var("x")))
let t1itot2 = Tiny1.Nameable.compileToNameless(tiny1Testinst, list{})
let t2itoSM1 = Tiny2.Nameless.compileToStackMachine1(t1itot2)
Js.log(Tiny1.Nameable.eval(tiny1Testinst, list{}))
Js.log(Tiny2.Nameless.eval(t1itot2, list{}))
Js.log(SM1.eval(t2itoSM1, list{}))