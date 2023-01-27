let testExpr: Tiny0.expr = Add(Add(Cst(1), Cst(2)), Mul(Cst(3), Cst(4)))
let testInstrs = Tiny0.compile(testExpr)
let iarray = Array.of_list(testInstrs)
Js.log(Tiny0.eval1(testExpr))
Js.log(iarray);
let result = Tiny0.eval(testInstrs, list{})
Js.log(result)