trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = {
    new Generator[S] {
      def generate = f(self.generate)
    }
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = {
    new Generator[S] {
      def generate = f(self.generate).generate
    }
  }
}

val integers = new Generator[Int] {
  def generate = scala.util.Random.nextInt()
}


val booleans = for {i <- integers} yield if (i % 2 == 1) true else false

integers.generate

booleans.generate



