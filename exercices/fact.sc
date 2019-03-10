import scala.annotation.tailrec

def fact(x: Int): Int = {
  @tailrec
  def fact(result: Int, x: Int): Int =
    if (x == 0) result
    else fact(result * x, x - 1)
  fact(1, x)
}

// loop(acc * n, n - 1)

fact(0)
fact(1)
fact(2)
fact(3)
fact(4)
fact(5)
