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
    else if isNonNegative(right) then addition(increment(left), decrement(right))
    else addition(decrement(left), increment(right))

  def multiplication(left: Number, right: Number): Number =
    require(left >= 0, "Left must be non-negative")
    require(right >= 0, "Right must be non-negative")

    if isZero(left) then 0
    else if isZero(right) then 0
    else if left == 1 then right
    else if right == 1 then left
    else addition(multiplication(left, decrement(right)), left)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerTailRec(base: Number, p: Number, intermResult: Number = 1): Number =
      if p == 0 then intermResult
      else if p == 1 then multiplication(intermResult, base)
      else powerTailRec(base, decrement(p), multiplication(intermResult, base))

    powerTailRec(base, p)
