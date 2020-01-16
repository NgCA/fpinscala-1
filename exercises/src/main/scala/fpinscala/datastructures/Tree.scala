package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  /*
  Exercise 3.25
  Write a function size that counts the number of nodes (leaves and branches) in a tree.
   */
  def size[A](tree: Tree[A]) : Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  /*
  Exercise 3.26
  Write a function maximum that returns the maximum element in a Tree[Int].
  (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x andy.)
   */
  def maximum(tree: Tree[Int]) : Int = {
    tree match {
      case Leaf(value) => value
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }
}