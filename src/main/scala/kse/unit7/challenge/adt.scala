package kse.unit7.challenge

object adt:

  enum Try[+V]:

    case Success(value: V)

    case Failure(exception: Throwable)

    def flatMap[Q](f: V => Try[Q]): Try[Q] =
      this match
        case Success(v)  => f(v)
        case Failure(ex) => Failure(ex)

    def map[Q](f: V => Q): Try[Q] =
      this match
        case Success(v)  => Success(f(v))
        case Failure(ex) => Failure(ex)

  object Try:

    def apply[V](v: => V): Try[V] =
      try Success(v)
      catch case ex: Throwable => Failure(ex)
