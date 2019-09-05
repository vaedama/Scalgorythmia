package datastructures.immutable.tree

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/*
Functional implementation of a Binary Tree

Having +A in covariant position will help us say:
val tree: BTree[String] = Leaf
 */
sealed trait BinaryTree[+A] {

  // DFS: First visit root node -> left sub-tree -> right sub-tree
  // - traverses right sub-tree and populates the accumulator with right sub-tree values
  // - then traverses left sub-tree and prepends the accumulator with left sub-tree values
  // - then prepends root node value finally before exiting the recursive definition
  // O(n) time and O(log n) space
  def traversePreOrder: List[A] = {
    def loop(tree: BinaryTree[A], memo: List[A] = List.empty[A]): List[A] = tree match {
      case Branch(l, v, r) => v +: loop(l, loop(r, memo))
      case Leaf => memo
    }

    loop(this)
  }

  // DFS: First visit left sub-tree -> root node -> right sub-tree
  // - traverses right sub-tree and populates the accumulator with right sub-tree values
  // - prepends root node to the accumulator before moving to the left sub-tree
  // - traverses left sub-tree with the accumulator who's head is root and tail is right sub-tree values
  // O(n) time and O(log n) space
  def traverseInOrder: List[A] = {
    def loop(tree: BinaryTree[A], memo: List[A] = List.empty[A]): List[A] = tree match {
      case Branch(l, v, r) => loop(l, v +: loop(r, memo))
      case Leaf => memo
    }

    loop(this)
  }

  // DFS: First visit left sub-tree -> right sub-tree -> root node
  // - traverses right sub-tree with accumulator containing root node and prepends right sub-tree values
  // - traverses left sub-tree prepending left sub-tree values to the accumulator
  // O(n) time and O(log n) space
  def traversePostOrder: List[A] = {
    def loop(tree: BinaryTree[A], memo: List[A] = List.empty[A]): List[A] = tree match {
      case Branch(l, v, r) => loop(l, loop(r, v +: memo))
      case Leaf => memo
    }

    loop(this)
  }

  // BFS: Visit level-by-level
  // Initialize a queue with root of the tree.
  // Pop the queue
  // If the front element of the queue is a branch,
  // enqueue left tree followed by right tree and prepend the value to
  // the accumulator.
  // Iterate until the queue is empty and finally reverse the accumulator.
  // O(n) time and O(log n) space
  def traverseLevelOrder: List[A] = {
    @tailrec def loop(q: Queue[BinaryTree[A]], memo: List[A] = Nil): List[A] = {
      if (q.isEmpty) memo.reverse
      else {
        val (front, rearQ) = q.dequeue
        front match {
          case Branch(l, v, r) => loop(rearQ.enqueue(l).enqueue(r), v :: memo)
          case Leaf => loop(rearQ, memo)
        }
      }
    }

    loop(Queue(this))
  }

  // O(log n) time and space
  def insert[B >: A](b: B)(implicit ord: Ordering[B]): BinaryTree[B] = this match {
    case Branch(l, v, r) =>
      import ord._
      if (b < v) Branch(l.insert(b), v, r)
      else if (b > v) Branch(l, v, r.insert(b))
      else this
    case Leaf => Branch(Leaf, b, Leaf)
  }

  // O(log n) time and space
  def min: Option[A] = {
    @tailrec def loop(t: BinaryTree[A], min: A): A = t match {
      case Branch(l, v, _) => loop(l, v)
      case Leaf => min
    }

    this match {
      case Branch(l, v, _) => Some(loop(l, v))
      case Leaf => None
    }
  }

  // O(log n) time and space
  def contains[B >: A](b: B)(implicit ord: Ordering[B]): Boolean = this match {
    case Branch(l, v, r) =>
      import ord._
      if (b > v) r.contains(b)
      else if (b < v) l.contains(b)
      else true
    case Leaf => false
  }

  // O(n) time and space
  def size: Int = this match {
    case Branch(l, _, r) => 1 + l.size + r.size
    case Leaf => 0
  }

  // O(n) time and space
  def depth: Int = this match {
    case Branch(l, _, r) => 1 + l.depth.max(r.depth)
    case Leaf => 0
  }

  // O(n) time and space
  def isComplete: Boolean = size == Math.pow(2, depth) - 1

  // O(n) time and space
  def equals[B >: A](that: BinaryTree[B]): Boolean = (this, that) match {
    case (Branch(l1, v1, r1), Branch(l2, v2, r2)) if v1 == v2 => l1.equals(l2) && r1.equals(r2)
    case (Leaf, Leaf) => true
    case _ => false
  }

  // O(n) time and space
  def flip: BinaryTree[A] = this match {
    case Branch(l, v, r) => Branch(r.flip, v, l.flip)
    case Leaf => Leaf
  }

  // O(n) time because every node has to be processed
  // O(log n) space because at any given time there are log n entries on the recursive call stack
  def isBST[B >: A](implicit ord: Ordering[B]): Boolean = {
    import ord._
    this match {
      case Branch(Leaf, _, Leaf) => true
      case Branch(l: Branch[A], v, Leaf) if l.value <= v => l.isBST(ord)
      case Branch(Leaf, v, r: Branch[A]) if r.value >= v => r.isBST(ord)
      case Branch(l: Branch[A], v, r: Branch[A]) if l.value <= v && r.value >= v => l.isBST(ord) && r.isBST(ord)
      case _ => false
    }
  }

  // Avg. case: O(log n) time and space
  // Worst case: O(n) time and space
  def findClosestValueInBST[B >: A](target: B)(implicit num: Numeric[B]): Option[B] = {
    import num._
    @tailrec def loop(tree: BinaryTree[B], closest: Option[B] = None): Option[B] = tree match {
      case Branch(l, v, r) =>
        val newClosest = closest match {
          case None => v
          case Some(prev) =>
            val curCloseness = abs(target - v)
            val prevCloseness = abs(target - prev)
            if (curCloseness < prevCloseness) v else prev
        }
        if (target < v) loop(l, Some(newClosest))
        else loop(r, Some(newClosest))
      case Leaf => closest
    }

    loop(this)
  }

  // O(n) time because every node has to be processed
  // O(log n) space because at any given time there are log n entries on the recursive call stack
  def toList: List[A] = {
    def dfs(cur: BinaryTree[A], acc: List[A] = Nil): List[A] = cur match {
      case Branch(l, v, r) => v :: l.toList ::: r.toList
      case Leaf => acc
    }

    dfs(this)
  }

  // O(n) time because every node has to be processed
  // O(log n) space because at any given time there are log n entries on the recursive call stack
  def sum[B >: A](implicit num: Numeric[B]): B = {
    import num._
    def dfs(cur: BinaryTree[B], acc: B = zero): B = cur match {
      case Branch(l, v, r) => v + l.sum + r.sum
      case Leaf => acc
    }

    dfs(this)
  }

  // O(n) time because every node has to be processed
  // O(log n) space because at any given time there are log n entries on the recursive call stack
  def sumPathsRootToLeaf[B >: A](sum: B)(implicit num: Numeric[B]): List[List[B]] = {
    import num._
    def dfs(cur: BinaryTree[A], sum: B, path: List[B] = Nil): List[List[B]] = cur match {
      case Leaf if sum == 0 =>
        List(path)
      case Branch(Leaf, v, Leaf) =>
        if (v == sum) List(v :: path) else Nil
      case Branch(l, v, r) =>
        dfs(l, sum - v, v :: path) ::: dfs(r, sum - v, v :: path)
      case _ =>
        Nil
    }

    dfs(this, sum)
  }

  // O(n) time because every node has to be processed
  // O(log n) space because at any given time there are log n entries on the recursive call stack
  def height: Int = this match {
    case Branch(l, _, r) => 1 + l.height.max(r.height)
    case Leaf => 0
  }

  // O(n*h) time time because it's O(n log n) for binary tree and O(n^2) for linked list
  // O(n) space because worst-case of the tree would be a linked list
  def diameterSlow: Int = this match {
    case Branch(l, _, r) =>
      val opt1 = l.height + r.height // diameter passing through root
    val opt2 = l.diameterSlow // left tree diameter
    val opt3 = r.diameterSlow // right tree diameter
      opt1.max(opt2.max(opt3))
    case Leaf => 0
  }

  // O(n) time and space
  def diameter: Int = heightDiameter._2

  private def heightDiameter: (Int, Int) = this match {
    case Branch(l, _, r) =>
      val (lh, ld) = l.heightDiameter
      val (rh, rd) = r.heightDiameter
      val height = 1 + Math.max(lh, rh)
      val opt1 = lh + rh
      val opt2 = ld
      val opt3 = rd
      val diameter = opt1.max(opt2.max(opt3))
      (height, diameter)
    case Leaf => (0, 0)
  }

  def maxSumPath[B >: A](implicit num: Numeric[B]): List[B] = this match {
    case Branch(l, v, r) =>
      val ls = v :: l.maxSumPath(num)
      val rs = v :: r.maxSumPath(num)
      if (num.gt(ls.sum, rs.sum)) ls else rs
    case Leaf => Nil
  }

  override def toString: String = this match {
    case Branch(v, l, r) => "{" + l + v + r + "}"
    case Leaf => "\uD83C\uDF40"
  }

}

/*
A leaf node with value "x" is represented as:
Branch("x", Leaf, Leaf)

A branch with value "b" having two leaves who's values are "x" and "y" respectively is represented as:
Branch("b", Branch("x", Leaf, Leaf), Branch("y", Leaf, Leaf))

In the above branch:
Branch("x", Leaf, Leaf) represents left sub-tree and
Branch("y", Leaf, Leaf) represents right sub-tree
*/
case object Leaf extends BinaryTree[Nothing]

case class Branch[A](
  left: BinaryTree[A] = Leaf,
  value: A,
  right: BinaryTree[A] = Leaf) extends BinaryTree[A]

object BinaryTree {

  def empty[A]: BinaryTree[A] = Leaf

  def build[A](as: Seq[A]): BinaryTree[A] = as match {
    case Seq() => Leaf
    case Seq(h, t@_*) =>
      val k = t.length / 2
      Branch(build[A](t.take(k)), h, build[A](t.drop(k)))
  }

  def buildComplete(v: Int, depth: Int): BinaryTree[Int] =
    if (depth == 0) Leaf
    else Branch(buildComplete(2 * v, depth - 1), v, buildComplete(2 * v + 1, depth - 1))

  def buildBST[A](as: A*)(implicit ord: Ordering[A]): BinaryTree[A] =
    as.foldLeft(empty[A])(_ insert _)

}
