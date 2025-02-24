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
}
