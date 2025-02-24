package tree.binary

import scala.annotation.tailrec

object BTProblemsLeetCode2 {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  // https://leetcode.com/problems/count-complete-tree-nodes/?envType=problem-list-v2&envId=binary-tree
  def countNodes(root: TreeNode): Int = {
    if(root == null) 0 else 1 + countNodes(root.left) + countNodes(root.right)
  }
  def countNodesTailrec(root: TreeNode): Int = {
    @tailrec
    def helper(stack: List[TreeNode], count: Int): Int = {
      if(stack.isEmpty) count
      else if(stack.head == null) helper(stack.tail, count)
      else helper(stack.head.left :: stack.head.right :: stack.tail, count + 1)
    }
    helper(List(root), 0)
  }

  // https://leetcode.com/problems/maximum-depth-of-binary-tree/?envType=problem-list-v2&envId=binary-tree
  def maxDepth(root: TreeNode): Int = {
    if(root == null) 0 else 1 + Math.max(countNodes(root.left), countNodes(root.right))
  }

  def maxDepthTailrec(root: TreeNode): Int = {
    @tailrec
    def helper(stack: List[(TreeNode, Int)], maxDepth: Int): Int = {
      if (stack.isEmpty) maxDepth
      else {
        val (currNode, currDepth) = stack.head
        if (currNode == null) helper(stack.tail, maxDepth)
        else helper((currNode.left, currDepth + 1) :: (currNode.right, currDepth + 1) :: stack.tail, Math.max(currDepth, maxDepth))
      }
    }

    helper(List((root, 1)), 0)
  }

  // https: //leetcode.com/problems/balanced-binary-tree/?envType=problem-list-v2&envId=binary-tree
  def isBalanced(root: TreeNode): Boolean = {
    def helper(curr: TreeNode): (Boolean, Int) = {
      if (curr == null) (true, 0)
      else {
        val (leftIsBalanced, leftHeight) = helper(curr.left)
        val (rightIsBalanced, rightHeight) = helper(curr.right)
        (leftIsBalanced && rightIsBalanced && Math.abs(leftHeight - rightHeight) < 2, Math.max(leftHeight, rightHeight) + 1)
      }
    }
    helper(root)._1
  }

  def isBalancedConcise(root: TreeNode): Boolean = {
    def helper(curr: TreeNode): Int = { // information can be captured in just an Int by using -1 to mean unbalanced
      if (curr == null) 0
      else {
        val leftHeight = helper(curr.left)
        val rightHeight = helper(curr.right)
        if(leftHeight == -1 || rightHeight == -1 || Math.abs(leftHeight - rightHeight) > 1) -1 else Math.max(leftHeight, rightHeight) + 1
      }
    }
    helper(root) != -1
  }

  def isBalancedTailrec(root: TreeNode): Boolean = {
    @tailrec
    def helper(stack: List[(TreeNode, Int, Int)], currHeight: Int): Int = {
      if (stack.isEmpty) currHeight
      else if (stack.head._1 == null) helper(stack.tail, 0)
      else {
        val (curr, visits, lHeight) = stack.head // node, visit count and left height stored in stack
        if (visits == 0) { // first visit increase the visit count and push the left child
          helper((curr.left, 0, 0) :: (curr, 1, 0) :: stack.tail, currHeight + 1)
        } else if(visits == 1){ // second visit then currHeight value should be the height of the left sub tree, push the right child, increase visit count and set left height
          helper((curr.right, 0, 0) :: (curr, 2, currHeight) :: stack.tail, currHeight)
        } else { // third visit then currHeight should be the height of the right sub tree
          if(lHeight == -1 || currHeight == -1 || Math.abs(lHeight - currHeight) > 1) -1
          else helper(stack.tail, Math.max(lHeight, currHeight) + 1)
        }
      }
    }
    helper(List((root, 0, 0)), 0) != -1
  }
}
