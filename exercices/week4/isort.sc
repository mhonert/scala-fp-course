def isort(xs: List[Int]): List[Int] = {
  xs match {
    case List() => List()
    case x :: ys => insert(x, isort(ys))
  }
}

def insert(x: Int, xs: List[Int]): List[Int] = {
  println("Insert: " + x)
  xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
}

//isort(List(4, 2, 7, 99, 98, 1))
isort(List(1, 2, 3))

isort(List(2, 1))    // 3
isort(List(3, 2, 1))    // 6
isort(List(4, 3, 2, 1)) // 10
isort(List(5, 4, 3, 2, 1)) // 15
isort(List(6, 5, 4, 3, 2, 1)) // 21

