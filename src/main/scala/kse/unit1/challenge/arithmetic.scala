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
//    require(left >= 0, "Left must be non-negative")
//    require(right >= 0, "Right must be non-negative")

    if isZero(left) then right
    else if isZero(right) then left
    else if isZero(left) && isZero(right) then 0
    else if isNonNegative(right) then addition(increment(left), decrement(right))
    else addition(decrement(left), increment(right))

  def multiplication(left: Number, right: Number): Number =
    @tailrec
    def multiplicationTailRec(left: Number, right: Number, acc: Number = 0): Number =
      if isZero(right) then acc
      else if isNonNegative(right) then multiplicationTailRec(left, decrement(right), addition(acc, left))
      else if !isNonNegative(left) && !isNonNegative(right) then multiplicationTailRec(abs(left), decrement(abs(right)), addition(acc, abs(left)))
      else multiplicationTailRec(decrement(left), right, addition(acc, right))

    multiplicationTailRec(left, right)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerTailRec(base: Number, p: Number, intermResult: Number = 1): Number =
      if isZero(p) then intermResult
      else if isZero(decrement(base)) then 1
      else if isZero(decrement(p)) then multiplication(intermResult, base) // if p == 1
      else powerTailRec(base, decrement(p), multiplication(intermResult, base))

    powerTailRec(base, p)
