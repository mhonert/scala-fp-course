def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}


sum(x => x * x, 1, 4)


def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)


product(x => x * x)(1, 3)

def fact(n: Int) = product(x => x)(1, n)

fact(5)


// mapReduce
def gen(startValue: Int, f: (Int, Int) => Int)(g: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(acc, g(a)))
  }
  loop(a, startValue)
}

gen(0, (a, b) => a + b)((x: Int) => x)(1, 4)

