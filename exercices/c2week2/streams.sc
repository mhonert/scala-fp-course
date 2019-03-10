(0 to 10).toStream(3)

Stream.cons


1 #:: Stream.empty

def from(n: Int): Stream[Int] = n #:: from(n + 1)


from(1).take(10).toList


def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))



println(sieve(from(2)).take(100).toList)


def sqrt(n: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + n / guess) / 2

  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}





