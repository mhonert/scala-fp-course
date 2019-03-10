abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat

  def count(x: Int): Int
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat =
    if (that.isZero) that
    else throw new IllegalStateException("Result is negative")

  override def predecessor: Nat = throw new IllegalStateException("Zero has no predecessor")

  override def toString = "0"

  override def count(x: Int) = x
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat =
    if (that.isZero) this
    else this.successor + that.predecessor

  override def -(that: Nat): Nat =
    if (that.isZero) this
    else this.predecessor - that.predecessor

  override def predecessor: Nat = n

  def count(x: Int): Int =
    predecessor.count(x + 1)

  override def toString = count(0).toString()
}


val one = Zero.successor
val two = one.successor
val three = two.successor
val ten = three + three + three + one

ten - two
