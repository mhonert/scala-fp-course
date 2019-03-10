import cweek4.frp._

var a = Var(2)
var b = Var(3)
val c = Signal(a() + b())

val r1 = c()

b() = 7
val r2 = c()

a() = 10
val r3 = c()


