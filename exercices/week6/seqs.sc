val s = "Hello World"

(0 until s.length) zip s


def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)


isPrime(7)
isPrime(25)
isPrime(29)


def primePairs(n: Int) =
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)


primePairs(20)

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for {
    (x, y) <- xs zip ys
  } yield x * y).sum

scalarProduct(List(2, 4, 5), List(2, 4, 1))
