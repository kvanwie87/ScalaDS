package tree.binary

import scala.annotation.tailrec

object BinaryTreeProblems {
  def main(args: Array[String]): Unit = {
    val level2_0 = BTNode(4, BTEnd, BTEnd)
    val level2_1 = BTNode(5, BTEnd, BTEnd)
    val level2_2 = BTNode(6, BTEnd, BTEnd)
    val level2_3 = BTNode(7, BTEnd, BTEnd)

    val level1_0 = BTNode(2, level2_0, level2_1)
    val level1_1 = BTNode(3, level2_2, level2_3)

    val level0_0 = BTNode(1, level1_0, level1_1)
    //           1
    //        2     3
    //      4  5   6  7
    BinaryTreeTraversals.print_LevelOrder(level0_0)
    println()
    BinaryTreeTraversals.print_LevelOrder(mirrorTree(level0_0))
  }
  def mirrorTreeTraditionalRecursion[T](root : BTree[T]): BTree[T] = {
    if(root == BTEnd) BTEnd
    else BTNode[T](root.value, mirrorTreeTraditionalRecursion(root.right), mirrorTreeTraditionalRecursion(root.left))
  }
  def mirrorTree[T](root : BTree[T]): BTree[T] = {
    // Post order travesal make tree with swapped left and right
    @tailrec
    def getOrder(remaining: List[BTree[T]], order: List[BTree[T]]): List[BTree[T]] = {
      if (remaining.isEmpty) order
      else if (remaining.head.isEmpty) getOrder(remaining.tail, remaining.head :: order)
      else getOrder(remaining.head.left :: remaining.head.right :: remaining.tail, remaining.head :: order)
    }
    @tailrec
    def buildTree(order: List[BTree[T]], done: List[BTree[T]]): BTree[T] = {
      if(order.isEmpty) done.head
      else if(order.head.isEmpty) buildTree(order.tail, order.head :: done)
      else {
        val newLeft = done.head
        val newRight = done.tail.head
        val newNode = BTNode(order.head.value, newLeft, newRight)
        buildTree(order.tail, newNode :: done.drop(2))
      }
    }

    val order = getOrder(List(root), List.empty[BTree[T]])
    buildTree(order, List.empty[BTree[T]])
  }
}
