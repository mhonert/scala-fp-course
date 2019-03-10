import math.abs

val tolerance = 0.1e-50

def isCloseEnough(x: Double, y: Double) =
  abs(x - y) < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) =
  (x + f(x)) / 2

def sqrt(x: Double) = fixedPoint(averageDamp(guess => x / guess))(1)

sqrt(2)