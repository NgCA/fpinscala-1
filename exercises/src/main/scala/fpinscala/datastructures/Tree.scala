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
}