sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

sealed trait List[A] {

  def length: Int =
    this match {
      case End()        => 0
      case Pair(hd, tl) => 1 + tl.length
    }

  def contains(v: A): Boolean = {
    this match {
      case End()                   => false
      case Pair(hd, tl) if hd == v => true
      case Pair(_, tl)             => tl.contains(v)
    }
  }
  def apply(target: Int): Result[A] = {
    def recv(list: List[A], counter: Int): Result[A] = {
      list match {
        case End()                             => Failure("Bad things happened")
        case Pair(hd, tl) if counter == target => Success(hd)
        case Pair(_, tl)                       => recv(tl, counter + 1)
      }
    }

    recv(this, 0)
  }
}

case class End[A]() extends List[A]
final case class Pair[A](head: A, tail: List[A]) extends List[A]

val example = Pair(1, Pair(2, Pair(3, End())))
example.length
example.tail.length
End().length

example.contains(3)
example.contains(4)
End().contains(4)

example.apply(0)
example.apply(1)
// example.apply(4)

// recv(this, 0)
// rec(Pair(1, Pair(2, Pair(3, End()))), 0)
// rec(Pair(2, Pair(3, End())), 1)
