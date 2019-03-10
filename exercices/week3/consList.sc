trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false

  override def toString = "[" + head + "|" + tail + "]"
}

class Nil[T] extends List[T] {
  override def isEmpty = true

  override def head = throw new NoSuchElementException("Nil.head")

  override def tail = throw new NoSuchElementException("Nil.tail")

  override def toString = "Nil"
}

def nth[T](n: Int, list: List[T]): T = {
  if (n < 0 || list.isEmpty) throw new IndexOutOfBoundsException()
  else if (n == 0) list.head
  else nth(n - 1, list.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(1, list)

