package tree.binary

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object BinaryTreeTraversals {
  def main(args: Array[String]): Unit = {
    val level2_0 = BTNode(4,BTEnd, BTEnd)
    val level2_1 = BTNode(5,BTEnd, BTEnd)
    val level2_2 = BTNode(6,BTEnd, BTEnd)
    val level2_3 = BTNode(7,BTEnd, BTEnd)

    val level1_0 = BTNode(2, level2_0, level2_1)
    val level1_1 = BTNode(3, level2_2, level2_3)

    val level0_0 = BTNode(1, level1_0, level1_1)
    val sampleBT: BTree[Int] = BTNode(1,
        BTEnd, BTEnd)

    //println(toList_Inorder(level0_0))
    //print_LevelOrder(level0_0)
    print_preorder2(level0_0)
  }
  //          1
  //      2       3
  //    4   5    6  7
  // Types of traversals
  // Inorder - 4 2 5 1 6 3 7
  // Preorder - 1 2 4 5 3 6 7
  // Postorder - 4 5 2 6 7 3 1
  // Level Order - 1 2 3 4 5 6 7

  def print_preorder2[T](root: BTree[T]): Unit = {
    // process current node
    // push children into stack
    // recurse

    // stack.push(root)
    //while stack.isnotempty
    // //be mindful of nulls
    //  currNode = stack.pop
    //  doStuff
    //  stack.push(currNode.right) stack.push(currNode.left)
    @tailrec
    def helper(stack: List[BTree[T]]): Unit = {
      if(stack.isEmpty) return
      if(stack.head.isEmpty) helper(stack.tail)
      else {
        print(s"${stack.head.value} ")
        helper(stack.head.left :: stack.head.right :: stack.tail)
      }
      /*val curr = stack.head
      print(s"${curr.value} ")
      val newstack1 = if(!curr.right.isEmpty) curr.right :: stack.tail else stack.tail
      val newstack2 = if(!curr.left.isEmpty) curr.left :: newstack1 else newstack1
      helper(newstack2)*/
    }
    helper(List(root))
  }


  def print_levelorder2[T](root: BTree[T]): Unit = {
    // queue.push(root)
    // while(queue.isNotEMpty)
    //    curr = queue.pop
    //    stuff
    //    queue.push(childrens)
    
    @tailrec
    def helper(q: Queue[BTree[T]]): Unit = {
      //Queue Internally is 2 List which are effectively stacks
      if(q.isEmpty) return
      
      print(s"${q.head.value} ")
      //q.head.left  q.head.right
      helper(q)
    }
    helper(Queue(root))
  }
  
  def print_PreOrder[T](root: BTree[T]): Unit = {
    if(root.isEmpty) return
    print(s"${root.value} ")
    print_PreOrder(root.left)
    print_PreOrder(root.right)
  }

  def print_PostOrder[T](root: BTree[T]): Unit = {
    if (root.isEmpty) return
    print_PostOrder(root.left)
    print_PostOrder(root.right)
    print(s"${root.value} ")
  }

  def print_InOrder[T](root: BTree[T]): Unit = {
    if (root.isEmpty) return
    print_InOrder(root.left)
    print(s"${root.value} ")
    print_InOrder(root.right)
  }

  def print_LevelOrder[T](root: BTree[T]): Unit = {
    @tailrec
    def helper(queue: Queue[BTree[T]]): Unit = {
      if(queue.isEmpty) return
      val curr = queue.head
      if(curr.isEmpty) helper(queue.tail)
      else {
        print(s"${curr.value} ")
        helper(queue.tail :+ curr.left :+ curr.right)
      }
    }

    helper(Queue(root))
  }

  def toList_LevelOrder[T](root: BTree[T]): List[T] = {
    @tailrec
    def helper(currLevel: List[BTree[T]], nextLevel: List[BTree[T]], result: List[T]): List[T] ={
      if(currLevel.isEmpty) {
        if(nextLevel.isEmpty) result.reverse
        else helper(nextLevel.reverse, List.empty[BTree[T]], result)
      } else {
        val curr = currLevel.head
        if(curr.isEmpty) helper(currLevel.tail, nextLevel, result)
        else helper(currLevel.tail, curr.right :: curr.left :: nextLevel, curr.value :: result)
      }
    }
    helper(List(root), List.empty[BTree[T]], List.empty[T])
  }

  def toList_PreOrder[T](root: BTree[T]): List[T] = {
    @tailrec
    def helper(todo: List[BTree[T]], result: List[T]) : List[T] = {
      if(todo.isEmpty) result.reverse
      else if(todo.head.isEmpty) helper(todo.tail, result)
      else helper(todo.head.left :: todo.head.right :: todo.tail, todo.head.value :: result)
    }
    helper(List(root), List.empty[T])
  }
  
  def toList_PostOrder[T](root: BTree[T]): List[T] = {
    @tailrec
    def helper(remaining: List[BTree[T]], order: List[BTree[T]]): List[T] = {
      if(remaining.isEmpty) order.foldLeft(List.empty[T])((list,node) => node.value :: list)
      else if(remaining.head.isEmpty) helper(remaining.tail, order)
      else helper(remaining.head.left :: remaining.head.right :: remaining.tail, remaining.head :: order)
    }
    helper(List(root), List.empty[BTree[T]])
  }

  def toList_Inorder[T](root: BTree[T]): List[T] = {
    @tailrec
    def helper(curr: BTree[T], stack: List[BTree[T]], result: List[T]): List[T] = {
      if(curr.isEmpty && stack.isEmpty) result.reverse
      else if(curr.isEmpty) helper(stack.head.right, stack.tail, stack.head.value :: result)
      else helper(curr.left, curr :: stack, result)
    }
    helper(root, List.empty[BTree[T]], List.empty[T])
  }

}
