package kse.unit7.challenge

object adt:

  enum Try[+V]:

    case Success(value: V)

    case Failure(message: String)

    def flatMap[Q](f: V => Try[Q]): Try[Q] =
      this match
        case Success(v)   => f(v)
        case Failure(msg) => Failure(msg)

    def map[Q](f: V => Q): Try[Q] =
      this match
        case Success(v)   => Success(f(v))
        case Failure(msg) => Failure(msg)

  object Try:

    def apply[V](v: V): Try[V] =
      if v == null then Failure("Null value") else Success(v)
