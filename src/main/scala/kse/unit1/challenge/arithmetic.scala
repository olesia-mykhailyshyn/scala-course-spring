package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val increment: Number => Number =
    value => value + 1

  val decrement: Number => Number =
    value => value - 1

  val isZero: Number => Boolean =
    value => value == 0

  val isNonNegative: Number => Boolean =
    value => value >= 0

  val abs: Number => Number =
    value =>
      if isNonNegative(value) then value
      else -value

  @tailrec
  def addition(left: Number, right: Number): Number =

    if isZero(left) then right
    else if isZero(right) then left
    else if isNonNegative(right) then addition(increment(left), decrement(right))
    else addition(decrement(left), increment(right))

  def multiplication(left: Number, right: Number): Number =
    @tailrec
    def multiplicationTailRec(left: Number, right: Number, acc: Number): Number =
      if isZero(right) then acc
      else if isNonNegative(right) then multiplicationTailRec(left, decrement(right), addition(acc, left))
      else if !isNonNegative(left) && !isNonNegative(right) then multiplicationTailRec(abs(left), decrement(abs(right)), addition(acc, abs(left)))
      else multiplicationTailRec(decrement(left), right, addition(acc, right))

    multiplicationTailRec(left, right, 0)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerTailRec(base: Number, p: Number, acc: Number): Number =
      if isZero(p) then acc
      else if isZero(decrement(base)) then 1
      else if isZero(decrement(p)) then multiplication(acc, base) // if p == 1
      else powerTailRec(base, decrement(p), multiplication(acc, base))

    powerTailRec(base, p, 1)
