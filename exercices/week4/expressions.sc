trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr

//def eval(expr: Expr): Int =
//  expr match {
//    case Number(n)   => n
//    case Sum(e1, e2) => eval(e1) + eval(e2)
//  }

def show(expr: Expr): String =
  expr match {
    case Number(n)    => n.toString
    case Var(name)    => name
    case Sum(e1, e2)  => show(e1) + " + " + show(e2)
    case Prod(Sum(a1, a2), Sum(b1, b2)) => "(" + show(a1) + " + " +
      show(a2) + ")" + " * " +
      "(" + show(b1) + " + " +
      show(b2) + ")"
    case Prod(Sum(a1, a2), b) => "(" + show(a1) + " + " +
      show(a2) + ")" + " * " + show(b)
    case Prod(a, Sum(b1, b2)) => show(a) + " * " +
      "(" + show(b1) + " + " +
      show(b2) + ")"
    case Prod(e1, e2) => show(e1) + " * " + show(e2)
  }


show(Prod(Number(3), Sum(Number(4), Var("x"))))
show(Prod(Number(3), Number(4)))
show(Prod(Sum(Number(3), Number(2)), Sum(Number(4), Number(7))))
show(Prod(Sum(Number(3), Number(2)), Number(6)))

show(Sum(Prod(Number(2), Var("x")), Var("y")))
