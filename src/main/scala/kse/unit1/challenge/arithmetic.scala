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
    // else if isZero(right) then left
    // else if isZero(left) && isZero(right) then 0
    else if isNonNegative(right) then addition(increment(left), decrement(right))
    else addition(decrement(left), increment(right))

  def multiplication(left: Number, right: Number): Number =
    if isZero(left) || isZero(right) then return 0
    // else if isZero(decrement(left)) then return right
    // else if isZero(decrement(right)) then return left

    @tailrec
    def multiplicationTailRec(left: Number, right: Number, intermResult: Number = 0): Number =
      if isZero(right) then intermResult
      else multiplicationTailRec(left, decrement(right), addition(intermResult, left))

    @tailrec
    def negate(value: Number, intermResult: Number = 0): Number =
      if isZero(value) then intermResult
      else negate(decrement(value), decrement(intermResult))

    if (isNonNegative(left) && isNonNegative(right)) || (!isNonNegative(left) && !isNonNegative(right)) then multiplicationTailRec(abs(left), abs(right))
    else negate(multiplicationTailRec(abs(left), abs(right)))

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
