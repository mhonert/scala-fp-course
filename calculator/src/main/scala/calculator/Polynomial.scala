package calculator

object Polynomial {

  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(discriminant(a(), b(), c()))
  }

  def discriminant(a: Double, b: Double, c: Double): Double = b*b - 4*a*c

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(computeSolutions(a(), b(), c(), delta()))
  }

  def computeSolutions(a: Double, b: Double, c: Double, delta: Double): Set[Double] =
    if (delta < 0 || (a == 0 && b == 0) || a.isNaN || b.isNaN || c.isNaN)
      Set()
    else if (a == 0) {
      Set(c / b)
    } else {
      val sol1 = computeSolution(a, b, delta, 1)
      if (delta > 0)
        Set(sol1, computeSolution(a, b, delta, -1))
      else
        Set(sol1)
    }

  def computeSolution(a: Double, b: Double, delta: Double, sign: Double): Double =
    (-b + sign*Math.sqrt(delta)) / (2*a)

}
