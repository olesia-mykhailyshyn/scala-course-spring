package kse.unit2.challenge

import scala.annotation.{tailrec, targetName}

object booleans:

  case object True
  case object False

  type True    = True.type
  type False   = False.type
  type Boolean = True | False

  val negation: Boolean => Boolean =
    case True  => False
    case False => True

  val conjunction: (Boolean, => Boolean) => Boolean =
    (value1, value2) => if value1 == False then False else value2

  val disjunction: (Boolean, => Boolean) => Boolean =
    (value1, value2) => if value1 == True then True else value2

  val implication: (Boolean, => Boolean) => Boolean =
    (left, right) => negation(left) ∨ right

  val equivalence: (Boolean, => Boolean) => Boolean =
    (left, right) => (left → right) ∧ (right → left)

  extension (value: Boolean)

    @targetName("negation")
    infix def unary_! : Boolean = negation(value)

    @targetName("conjunction")
    infix def ∧(that: => Boolean): Boolean = conjunction(value, that)

    @targetName("disjunction")
    infix def ∨(that: => Boolean): Boolean = disjunction(value, that)

    @targetName("implication")
    infix def →(that: => Boolean): Boolean = implication(value, that)

    @targetName("equivalence")
    infix def ↔(that: => Boolean): Boolean = equivalence(value, that)

  def fold(operation: (Boolean, Boolean) => Boolean, unit: Boolean)(list: List[Boolean]): Boolean =
    @tailrec
    def foldReq(list: List[Boolean], acc: Boolean): Boolean =
      list match
        case Nil          => acc
        case head :: tail => foldReq(tail, operation(head, acc))

    foldReq(list, unit)

  val conjunctionOfElements: List[Boolean] => Boolean = list => fold((a, b) => a ∧ b, True)(list)
  val disjunctionOfElements: List[Boolean] => Boolean = list => fold((a, b) => a ∨ b, False)(list)

  extension (booleans: List[Boolean])
    infix def conjunction: Boolean = conjunctionOfElements(booleans)
    infix def disjunction: Boolean = disjunctionOfElements(booleans)
