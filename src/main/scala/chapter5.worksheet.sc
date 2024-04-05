import scala.collection.View.Empty
object Anything {
  def apply(n: Int): Int = n + 1
}

//1 : 'Fixed Int,Int function f'
// fixed number of parameters (2)

val result = Anything(10) // 11
val result2 = Anything.apply(10) // 11

sealed trait IntList {
  def length: Int =
    this match {
      case End          => 0
      case Pair(hd, tl) => 1 + tl.length
    }
  // def double: IntList =
  //   this match {
  //     case End          => End
  //     case Pair(hd, tl) => Pair(hd * 2, tl.double)
  //   }

  def double: IntList =
    fold[IntList](End, (hd, tl) => Pair(hd * 2, tl))

  def product: Int =
    this match {
      case End          => 1
      case Pair(hd, tl) => hd * tl.product
    }

  def sum: Int =
    this match {
      case End          => 0
      case Pair(hd, tl) => hd + tl.sum
    }

  def fold[A](end: A, f: (Int, A) => A): A =
    this match {
      case End          => end
      case Pair(hd, tl) => f(hd, tl.fold(end, f))
    }
}

case object End extends IntList
case class Pair(hd: Int, tl: IntList) extends IntList

val sumFunction: (Int, Int) => Int = (a, b) => a + b
val sumFunctionShort: (Int, Int) => Int = _ + _
val sumFunctionV2 = (x: Int, y: Int) => x + y

val productFunction = (x: Int, y: Int) => x * y
val example = Pair(1, Pair(2, Pair(3, End)))
val data = List(1, 2, 3)
val result1 = example.fold(0, sumFunction) // 6
val prodResult = example.fold(1, productFunction) // 6

example.double // Pair(2, Pair(4, Pair(6), End))

def sum(x: Int, y: Int): Int = x + y

val sumWith5 = sum(5, _)

sealed trait Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B
}

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B = {
    node(left.fold(node, leaf), right.fold(node, leaf))
  }
}

case class Leaf[A](v: A) extends Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B = leaf(v)
}

val tree: Tree[String] =
  Node(
    Node(Leaf("To"), Leaf("iterate")),
    Node(
      Node(Leaf("is"), Leaf("human,")),
      Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))
    )
  )

val thing = tree.fold[String](_ + _, _ + " ")

case class MyPair[A, B](one: A, two: B)

sealed trait Sum[A, B]

case class Left[A, B](value: A) extends Sum[A, B]
case class Right[A, B](value: B) extends Sum[A, B]

Left[Int, String](1).value
Right[Int, String]("foo").value

sealed trait Maybe[A]
case class Full[A](a: A) extends Maybe[A]
case class Empty[A]() extends Maybe[A]
