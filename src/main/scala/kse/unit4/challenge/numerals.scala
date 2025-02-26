package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = Successor(this)

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = (this, that) match
      case (_, Zero)                    => !this.isZero
      case (Zero, _)                    => false
      case (Successor(n), Successor(m)) => n > m

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = (this == that) || (this > that)

    @targetName("less than")
    infix def <(that: Numeral): Boolean = (this != that) && !(this > that)

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = (this == that) || !(this > that)

    @targetName("addition")
    infix def +(that: Numeral): Numeral = that match
      case Zero         => this
      case Successor(n) => Successor(this + n)

    @targetName("subtraction")
    infix def -(that: Numeral): Numeral = (this, that) match
      case (_, Zero)                    => this
      case (Zero, _)                    => Zero
      case (Successor(n), Successor(m)) => n - m

    def toInt: Int

    override def toString: String = s"Nat(${this.predecessor})"

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = true

    def predecessor: Numeral = throw new UnsupportedOperationException("Zero has no predecessor")

    @targetName("greater than")
    override infix def >(that: Numeral): Boolean = false

    @targetName("addition")
    override infix def +(that: Numeral): Numeral = that

    @targetName("subtraction")
    override infix def -(that: Numeral): Numeral = Zero

    override def toInt: Int = 0

    override def toString: String = "Zero"

    override def equals(obj: Any): Boolean = obj match
      case that: Zero.type => true
      case _               => false

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = false

    def predecessor: Numeral = n

    @targetName("greater than")
    override infix def >(that: Numeral): Boolean = that match
      case Zero         => true
      case Successor(m) => n > m

    @targetName("addition")
    override infix def +(that: Numeral): Numeral = Successor(n + that)

    @targetName("subtraction")
    override infix def -(that: Numeral): Numeral = that match
      case Zero         => this
      case Successor(m) => n - m

    override def toInt: Int = 1 + n.toInt

    override def toString: String = s"Successor(${this.toInt})"

    override def equals(obj: Any): Boolean = obj match
      case s: Successor => this.toInt == s.toInt
      case _            => false
