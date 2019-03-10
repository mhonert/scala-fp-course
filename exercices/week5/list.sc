List(1, 2, 3) updated (1, 7)

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}


init(List(1, 2, 3))
init(List(1, 2, 3, 4, 5))


def removeAt[T](n: Int, xs: List[T]): List[T] =
  if (xs.isEmpty) xs
  else if (n == 0) xs.tail
  else xs.head :: removeAt(n - 1, xs.tail)


removeAt(1, List(1, 2, 3, 4))


def flatten(xs: List[Any]): List[Any] =
  if (xs.isEmpty) List()
  else xs.head match {
    case List() => List()
    case y :: ys => flatten(List(y)) ::: flatten(ys) ::: flatten(xs.tail)
    case y => y :: flatten(xs.tail)
  }

flatten(List(1, 2, 3, List(4, 5), 5))
flatten(List(1, 2, 3, List(4, 5, 6), List(7, 8, 9)))
flatten(List(List(1, 2,3)))
flatten(List(List(), List()))


