abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int) = new NonEmpty(x, Empty, Empty)

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

val l1 = new NonEmpty(3, Empty, Empty)
val l2 = l1 incl 4
val l3 = l2 incl 2

val r1 = new NonEmpty(7, Empty, Empty)
val r2 = r1 incl 10
val r3 = r2 incl 8

val u = l3 union r3





