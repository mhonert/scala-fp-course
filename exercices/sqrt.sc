def abs(x:Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double = {
    def isGoodEnough(nextGuess: Double) =
      abs(guess - nextGuess) < 0.1e-50

    def improve() = (guess + x / guess) / 2

    val nextGuess = improve()
    if (isGoodEnough(nextGuess)) nextGuess
    else sqrtIter(nextGuess)
  }

  sqrtIter(1.0)
}

sqrt(0.001)
sqrt(0.1e-50)
sqrt(0.1e-20)
sqrt(1.0e20)
sqrt(1.0e50)


Math.sqrt(0.001)
Math.sqrt(0.1e-50)
Math.sqrt(0.1e-20)
Math.sqrt(1.0e20)
Math.sqrt(1.0e50)
