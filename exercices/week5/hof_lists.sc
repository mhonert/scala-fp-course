def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] = xs map (x => x * x)

squareList2(List(1, 2, 3, 4))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => xs span (_ == x) match {
    case (a, b) => a :: pack(b)
  }
}

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (l => (l.head, l.size))

encode(List("a", "a", "a", "b", "c", "c", "a"))

List("a") partition (_ == "a")



def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, x) => x + 1)


mapFun(List(1, 2, 3), (x: Int) => x * 2)

lengthFun(List(1, 2, 3, 4))

