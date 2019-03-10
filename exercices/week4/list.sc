trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false

  override def toString = "[" + head + "|" + tail + "]"
}

object Nil extends List[Nothing] {
  override def isEmpty = true

  override def head = throw new NoSuchElementException("Nil.head")

  override def tail = throw new NoSuchElementException("Nil.tail")

  override def toString = "Nil"
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def incl(x: Int) = new NonEmpty(x, new Empty, new Empty)

  def contains(x: Int) = false

  def union(other: IntSet) = other

  override def toString = "."
}

class NonEmpty(x: Int, left: IntSet, right: IntSet) extends IntSet {

  def incl(x: Int) =
    if (x == this.x) this
    else if (x < this.x) new NonEmpty(this.x, left incl x, right)
    else new NonEmpty(this.x, left, right incl x)

  def contains(x: Int): Boolean =
    if (x == this.x) true
    else if (x < this.x) left contains x
    else right contains x

  def union(other: IntSet) =
    (right union left) union (other incl x)

  override def toString = "{" + left + x + right + "}"

}

def nth[T](n: Int, list: List[T]): T = {
  if (n < 0 || list.isEmpty) throw new IndexOutOfBoundsException()
  else if (n == 0) list.head
  else nth(n - 1, list.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, Nil)))

nth(1, list)

def f(xs: List[NonEmpty], x: Empty): List[IntSet] = xs prepend x


