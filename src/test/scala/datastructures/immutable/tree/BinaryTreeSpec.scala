package datastructures.immutable.tree

import org.scalatest.FunSuite

class BinaryTreeSpec extends FunSuite {

  private val tree = BinaryTree.build(Seq(1, 2, 3, 4, 5, 6, 7)) // {{{🍀3🍀}2{🍀4🍀}}1{{🍀6🍀}5{🍀7🍀}}}

  test("traversePreOrder") {
    assert(tree.traversePreOrder == List(1, 2, 3, 4, 5, 6, 7))
  }

  test("traverseInOrder") {
    assert(tree.traverseInOrder == List(3, 2, 4, 1, 6, 5, 7))
  }

  test("traversePostOrder") {
    assert(tree.traversePostOrder == List(3, 4, 2, 6, 7, 5, 1))
  }

  test("traverseLevelOrder") {
    assert(tree.traverseLevelOrder == List(1, 2, 5, 3, 4, 6, 7))
  }

  test("insert") {
    assert(tree.insert(8) == Branch(
      Branch(Branch(Leaf, 3, Leaf), 2, Branch(Leaf, 4, Leaf)),
      1,
      Branch(Branch(Leaf, 6, Leaf), 5, Branch(Leaf, 7, Branch(Leaf, 8, Leaf)))))
  }

  test("contains") {
    assert(tree.contains(7))
  }

  test("flip") {
    assert(tree.flip.flip == tree)
  }

  test("isComplete") {
    assert(BinaryTree.buildComplete(1, 4).isComplete)
  }

  test("isBST: valid scenario") {

    /*

     5
    / \
   1   6
      / \
     4  7

    */

    val tree = Branch(
      Branch(Leaf, 1, Leaf),
      5,
      Branch(Branch(Leaf, 4, Leaf), 6, Branch(Leaf, 7, Leaf)))

    assert(tree.isBST)
  }

  test("isBST: invalid scenario") {

    /*

     5
    / \
   1   4
      / \
      3  6

    */

    val tree = Branch(
      Branch(Leaf, 1, Leaf),
      5,
      Branch(Branch(Leaf, 3, Leaf), 4, Branch(Leaf, 6, Leaf)))

    assert(!tree.isBST)
  }


  /*

    1
   / \
  2   5
 / \   \
3  4   6

   */

  val _3 = Branch(Leaf, 3, Leaf)
  val _4 = Branch(Leaf, 4, Leaf)
  val _2 = Branch(_3, 2, _4)

  val _6 = Branch(Leaf, 6, Leaf)
  val _5 = Branch(Leaf, 5, _6)

  val _1 = Branch(_2, 1, _5)

  test("toList") {
    assert(_1.toList == List(1, 2, 3, 4, 5, 6))
  }

  test("sum") {
    assert(_1.sum == 21)
  }

  test("sumPathsRootToLeaf") {
    val _12 = {
      /*

    12
   /  \
  7   1
  |  / \
  9 10 5

 */
      val _9 = Branch(Leaf, 9, Leaf)
      val _7 = Branch(Leaf, 7, _9)

      val _10 = Branch(Leaf, 10, Leaf)
      val _5 = Branch(Leaf, 5, Leaf)
      val _1 = Branch(_10, 1, _5)
      Branch(_7, 12, _1)
    }
    assert(_12.sumPathsRootToLeaf(23) == List(List(10, 1, 12)))
  }

  val tree1: BinaryTree[Int] = {
    /*

    1
  /  \
 2    3
 |   / \
 4  5  6

*/

    val _4 = Branch(Leaf, 4, Leaf)
    val _2 = Branch(_4, 2, Leaf)

    val _5 = Branch(Leaf, 5, Leaf)
    val _6 = Branch(Leaf, 6, Leaf)
    val _3 = Branch(_5, 3, _6)
    Branch(_2, 1, _3)
  }

  test("diameterSlow: tree 1") {
    assert(tree1.diameterSlow == 4)
  }

  test("diameter: tree 1") {
    assert(tree1.diameter == 4)
  }

  test("maxSumPath: tree 1") {
    assert(tree1.maxSumPath == List(1, 3, 6))
  }

  val tree2: BinaryTree[Int] = {

    /*

        1
       / \
      2  3
        / \
       5  6
      / \ |
     7  8 9
        | |
       10 11

 */

    val _2 = Branch(Leaf, 2, Leaf)

    val _7 = Branch(Leaf, 7, Leaf)

    val _10 = Branch(Leaf, 10, Leaf)
    val _8 = Branch(Leaf, 8, _10)
    val _5 = Branch(_7, 5, _8)

    val _11 = Branch(Leaf, 11, Leaf)
    val _9 = Branch(Leaf, 9, _11)
    val _6 = Branch(Leaf, 6, _9)

    val _3 = Branch(_5, 3, _6)
    Branch(_2, 1, _3)
  }

  test("diameterSlow: tree 2") {
    assert(tree2.diameterSlow == 6)
  }

  test("diameter: tree 2") {
    assert(tree2.diameter == 6)
  }

  test("maxSumPath: tree 2") {
    assert(tree2.maxSumPath == List(1, 3, 6, 9, 11))
  }

  val tree3: BinaryTree[Int] = {
    /*
        11
       / \
      2  3
        /
       5
      / \
     7  8
    /   /
  100  10
      / \
     1  1
     |  |
     2  2
     |  |
     3  3
     |  |
     4  4
     |  |
     5  5
     |  |
     6  6

     */
    val tree = BinaryTree.buildBST(1 to 6: _*)
    val _10 = Branch(tree, 10, tree)
    val _8 = Branch(_10, 8, Leaf)

    val _100 = Branch(Leaf, 100, Leaf)
    val _7 = Branch(_100, 7, Leaf)
    val _5 = Branch(_7, 5, _8)

    val _3 = Branch(_5, 3, Leaf)
    val _2 = Branch(Leaf, 2, Leaf)
    Branch(_2, 11, _3)
  }

  test("diameterSlow: tree 3") {
    assert(tree3.diameterSlow == 12)
  }

  test("diameter: tree 3") {
    assert(tree3.diameter == 12)
  }

  test("maxSumPath: tree 3") {
    assert(tree3.maxSumPath == List(11, 3, 5, 7, 100))
  }

}
