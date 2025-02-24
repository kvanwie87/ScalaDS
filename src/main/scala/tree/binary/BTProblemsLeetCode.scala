package tree.binary

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object BTProblemsLeetCode {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  // https://leetcode.com/problems/binary-tree-preorder-traversal/description/?envType=problem-list-v2&envId=binary-tree
  def preorderTraversalRecursion1(root: TreeNode): List[Int] = {
    val result = ListBuffer[Int]()
    def recursiveHelper(curr: TreeNode): Unit = {
      if(curr == null) return
      result.append(curr.value)
      recursiveHelper(curr.left)
      recursiveHelper(curr.right)
    }
    recursiveHelper(root)
    result.toList
  }

  def preorderTraversalRecursion2(root: TreeNode): List[Int] = {
    if (root == null) Nil else root.value :: preorderTraversalRecursion2(root.left) ++ preorderTraversalRecursion2(root.right)
  }

  def preorderTraversalStack(root: TreeNode): List[Int] = {
    if(root == null) return Nil
    val result = ListBuffer[Int]()
    val stack = scala.collection.mutable.Stack[TreeNode]()
    stack.push(root)
    while(stack.nonEmpty) {
      val curr = stack.pop()
      if(curr != null) {
        result.append(curr.value)
        stack.push(curr.right)
        stack.push(curr.left)
      }
    }
    result.toList
  }

  def preorderTraversalTailrec(root: TreeNode): List[Int] = {
    @tailrec
    def helper(stack: List[TreeNode], result: List[Int]): List[Int] = {
      if(stack.isEmpty) result.reverse
      else if(stack.head == null) helper(stack.tail, result)
      else helper(stack.head.left :: stack.head.right :: stack.tail, stack.head.value +: result)
    }
    helper(List(root), List.empty[Int])
  }


  // https://leetcode.com/problems/binary-tree-inorder-traversal/description/?envType=problem-list-v2&envId=binary-tree

  def inorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) Nil else  (inorderTraversal(root.left) :+ root.value) ++ inorderTraversal(root.right)
  }

  def inorderTraversal2(root: TreeNode): List[Int] = {
    if (root == null) Nil else inorderTraversal2(root.left) ++ (root.value :: inorderTraversal2(root.right))
  }

  def inorderTraversalRecursive(root: TreeNode): List[Int] = {
    def recursiveHelper(curr: TreeNode, result: ListBuffer[Int]): ListBuffer[Int] = {
      if(curr == null) result
      else {
        recursiveHelper(curr.left, result)
        result.append(curr.value)
        recursiveHelper(curr.right, result)
      }
    }
    recursiveHelper(root, ListBuffer.empty[Int]).toList
  }

  def inorderTraversalStack(root: TreeNode): List[Int] = {
    if (root == null) return Nil
    val result = ListBuffer[Int]()
    val stack = scala.collection.mutable.Stack[(TreeNode, Int)]() // Store the node and a visitCount
    stack.push((root,0))
    while(stack.nonEmpty) {
      val (curr, visCount) = stack.pop()
      if(curr != null) {
        if(visCount == 1) { // on second visit process the node and push the right child
          result.append(curr.value)
          stack.push((curr.right, 0))
        } else { // First visit do not process the node just push it and its left child
          stack.push((curr, 1))
          stack.push((curr.left, 0))
        }
      }
    }
    result.toList
  }

  def inorderTraversalTailrec(root: TreeNode): List[Int] = {
    @tailrec
    def helper(stack: List[(TreeNode, Int)], result: List[Int]): List[Int] = {
      if(stack.isEmpty) result.reverse
      else if(stack.head._1 == null) helper(stack.tail, result)
      else {
        val (curr, visits) = stack.head // node and visit count stored in stack
        if(visits == 0) { // first visit increase the visit count and push the left child
          helper((curr.left, 0) :: (curr, 1) :: stack.tail, result)
        } else { // second first process the node and push the right child
          helper((curr.right, 0) :: stack.tail, curr.value :: result)
        }
      }
    }
    helper(List((root, 0)), List.empty[Int])
  }

  // https://leetcode.com/problems/binary-tree-postorder-traversal/description/?envType=problem-list-v2&envId=binary-tree
  def postorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) Nil else (postorderTraversal(root.left) ++ postorderTraversal(root.right)) :+ root.value
  }

  def postorderTraversal2(root: TreeNode): List[Int] = {
    def helper(curr: TreeNode): List[Int] = {
      if (curr == null) Nil else curr.value :: (helper(curr.right) ++ helper(curr.left)) // build it backwards with :: and then reverse
    }
    helper(root).reverse
  }

  def postorderTraversalRecursion(root: TreeNode): List[Int] = {
    def helper(curr: TreeNode, result: ListBuffer[Int]): ListBuffer[Int] = {
      if(curr == null) result
      else {
        helper(curr.left, result)
        helper(curr.right, result)
        result.append(curr.value)
      }
    }
    helper(root, ListBuffer.empty[Int]).toList
  }

  def postorderTraversalStack(root: TreeNode): List[Int] = {
    if(root == null) return Nil
    val result = ListBuffer[Int]()
    val stack = scala.collection.mutable.Stack[(TreeNode, Int)]() // Store node and visit count
    stack.push((root, 0))
    while(stack.nonEmpty) {
      val (curr, visits) = stack.pop()
      if(curr != null) {
        if (visits == 0) {
          stack.push((curr, 1))
          stack.push((curr.right, 0))
          stack.push((curr.left, 0))
        } else {
          result.append(curr.value)
        }
      }
    }
    result.toList
  }

  def postorderTraversalTailrec(root: TreeNode): List[Int] = {
    @tailrec
    def helper(stack: List[(TreeNode, Int)], result: List[Int]): List[Int] = {
      if(stack.isEmpty) result.reverse
      else if(stack.head._1 == null) helper(stack.tail, result)
      else {
        val (curr, visits) = stack.head // node and visit count stored in stack
        if(visits == 0) { // first visit increase the visit count and push the left child and right child
          helper((curr.left, 0) :: (curr.right, 0) :: (curr, 1) :: stack.tail, result)
        } else { // second first process the node
          helper(stack.tail, curr.value :: result)
        }
      }
    }
    helper(List((root, 0)), List.empty[Int])
  }

  // https://leetcode.com/problems/binary-tree-level-order-traversal/?envType=problem-list-v2&envId=binary-tree
  def levelOrderQueue(root: TreeNode): List[List[Int]] = {
    val result = new ListBuffer[List[Int]]
    var currLevelList = new ListBuffer[Int]
    var currLevel = new scala.collection.mutable.Queue[TreeNode]
    var nextLevel = new scala.collection.mutable.Queue[TreeNode]
    currLevel.enqueue(root)
    while(currLevel.nonEmpty) {
      val curr = currLevel.dequeue()
      if(curr != null) {
        currLevelList.append(curr.value)
        nextLevel.enqueue(curr.left)
        nextLevel.enqueue(curr.right)
      }
      if(currLevel.isEmpty) {
        currLevel = nextLevel
        nextLevel = new scala.collection.mutable.Queue[TreeNode]
        if(currLevelList.nonEmpty) result.append(currLevelList.toList)
        currLevelList = new ListBuffer[Int]
      }
    }
    result.toList
  }

  def levelOrder(root: TreeNode): List[List[Int]] = {
    @tailrec
    def helper(currLevel: List[TreeNode], nextLevel: List[TreeNode], currList: List[Int], result: List[List[Int]]): List[List[Int]] = {
      if(currLevel.isEmpty && nextLevel.isEmpty) (currList.reverse :: result).reverse
      else if(currLevel.isEmpty) helper(nextLevel.reverse, List.empty[TreeNode], List.empty[Int], currList.reverse :: result)
      else if(currLevel.head == null) helper(currLevel.tail, nextLevel, currList, result)
      else helper(currLevel.tail, currLevel.head.right :: currLevel.head.left :: nextLevel, currLevel.head.value :: currList, result)
    }
    helper(List(root), List.empty[TreeNode], List.empty[Int], List.empty[List[Int]])
  }
}
