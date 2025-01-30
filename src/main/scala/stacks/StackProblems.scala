package stacks

object StackProblems {
  def main(args: Array[String]): Unit = {
    //scala.collection.mutable.Stack - mutable stack
    //scala.collection.immutable.List - immutable stack
    //testNextGreaterElement()
    testLargestRectangleArea()
  }
  /*
  Valid Parentheses
  https://leetcode.com/problems/valid-parentheses/description/
  Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.

  An input string is valid if:
    Open brackets must be closed by the same type of brackets.
    Open brackets must be closed in the correct order.
    Every close bracket has a corresponding open bracket of the same type.
   */
  def testValidParenteses(): Unit = {
    assert(isValid("()"))
    assert(isValid("()[]{}"))
    assert(!isValid("(]"))
    assert(isValid("([])"))
    assert(isValid(""))
    assert(!isValid("["))

    assert(isValid_v2("()"))
    assert(isValid_v2("()[]{}"))
    assert(!isValid_v2("(]"))
    assert(isValid_v2("([])"))
    assert(isValid_v2(""))
    assert(!isValid_v2("["))
  }
  def isValid(s: String): Boolean = {
    val openSet = Set('[','{','(')
    val closeMatch = Map(']'->'[', '}'->'{', ')'->'(')
    val stack = scala.collection.mutable.Stack[Char]()
    var i = 0
    while(i < s.length) {
      val currChar = s.charAt(i)
      if(openSet.contains(currChar)) {
        stack.push(currChar)
      } else {
        if(stack.nonEmpty && closeMatch(currChar) == stack.head) {
          stack.pop()
        } else return false
      }
      i+=1
    }
    stack.isEmpty
  }

  def isValid_v2(s: String): Boolean = {
    val openSet = Set('[', '{', '(')
    val closeMatch = Map(']' -> '[', '}' -> '{', ')' -> '(')
    @scala.annotation.tailrec
    def helper(remaining: String, stack: List[Char]): Boolean = {
      if(remaining.isEmpty) stack.isEmpty
      else if(openSet.contains(remaining.head)) helper(remaining.tail, remaining.head +: stack )
      else if(stack.nonEmpty && closeMatch(remaining.head) == stack.head) helper(remaining.tail, stack.tail)
      else false
    }
    helper(s, List.empty[Char])
  }

  /*
  Next Greater Element I
  https://leetcode.com/problems/next-greater-element-i/description/?envType=problem-list-v2&envId=stack
  The next greater element of some element x in an array is the first greater element that is to the right of x in the same array.
  You are given two distinct 0-indexed integer arrays nums1 and nums2, where nums1 is a subset of nums2.
  For each 0 <= i < nums1.length, find the index j such that nums1[i] == nums2[j] and determine the next greater element of nums2[j] in nums2.
  If there is no next greater element, then the answer for this query is -1.

  Return an array ans of length nums1.length such that ans[i] is the next greater element as described above.
   */
  def testNextGreaterElement(): Unit = {
    assert(nextGreaterElement(Array(4, 1, 2), Array(1, 3, 4, 2)) sameElements Array(-1, 3, -1))
    assert(nextGreaterElement(Array(2, 4), Array(1, 2, 3, 4)) sameElements Array(3, -1))

    assert(nextGreaterElement_v2(Array(4, 1, 2), Array(1, 3, 4, 2)) sameElements Array(-1, 3, -1))
    assert(nextGreaterElement_v2(Array(2, 4), Array(1, 2, 3, 4)) sameElements Array(3, -1))
  }
  def nextGreaterElement(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val resultMap = scala.collection.mutable.Map[Int,Int]()
    nums1.foreach(x => resultMap.put(x, -1))
    val stack = scala.collection.mutable.Stack[Int]()
    nums2.foreach(x => {
      while(stack.nonEmpty && x > stack.head) {
        val curr = stack.pop()
        if(resultMap.contains(curr)) resultMap.put(curr, x)
      }
      stack.push(x)
    })
    nums1.map(x => resultMap(x))
  }
  def nextGreaterElement_v2(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    val numSet = nums1.toSet
    @scala.annotation.tailrec
    def helper(i: Int, stack: List[Int], result: Map[Int, Int]): Map[Int,Int] = {
      if(i == nums2.length) result
      else if(stack.nonEmpty && nums2(i) > stack.head) helper(i, stack.tail, if(numSet.contains(stack.head)) result + (stack.head -> nums2(i)) else result)
      else helper(i+1, nums2(i) +: stack, result)
    }
    val resultMap = helper(0,List.empty[Int], Map.empty[Int,Int])
    nums1.map(resultMap.getOrElse(_, -1))
  }

  /*
  Largest Rectangle in Histogram
  https://leetcode.com/problems/largest-rectangle-in-histogram/description/
  Given an array of integers heights representing the histogram's bar height where the width of each bar is 1,
  return the area of the largest rectangle in the histogram.

  https://www.geeksforgeeks.org/largest-rectangular-area-in-a-histogram-using-stack/
   */
  def testLargestRectangleArea(): Unit = {

    //assert(largestRectangleArea(Array(2,1,5,6,2,3)) == 10)
    //assert(largestRectangleArea(Array(2,4)) == 4)

    assert(largestRectangleArea_v2(Array(2, 1, 5, 6, 2, 3)) == 10)
    assert(largestRectangleArea_v2(Array(2, 4)) == 4)
  }
  def largestRectangleArea(heights: Array[Int]): Int = {
    // Index:  [0,1,2,3,4,5]
    // Input:  [2,1,5,6,2,3]
    // Prefix: [-1,-1,1,2,1,4] // First index to the left that is smaller value
    // Suffix: [0,5,3,3,5,5] // First index to the right that is smaller value
    val prefix = Array.ofDim[Int](heights.length)
    val suffix = Array.ofDim[Int](heights.length)
    heights.indices.foreach(curr => {
      var prev = curr - 1
      while(prev >= 0 && heights(prev) >= heights(curr)) {
        prev = prefix(prev)
      }
      prefix(curr) = prev
    })
    (heights.length - 1 to 0 by -1).foreach(curr => {
      var next = curr + 1
      while(next < heights.length && heights(next) >= heights(curr)) {
        next = suffix(next)
      }
      suffix(curr) = next
    })
    heights.indices.map(i => heights(i) * (suffix(i) - prefix(i) - 1)).max
  }
  def largestRectangleArea_v2(heights: Array[Int]): Int = {
    @scala.annotation.tailrec
    def calculatePrefix(i: Int = 0,offset:Int = -1, result: IndexedSeq[Int] = IndexedSeq.empty[Int]): IndexedSeq[Int] = {
      if(i == heights.length) result
      else if(offset >= 0 && heights(offset) >= heights(i)) calculatePrefix(i, result(offset), result)
      else calculatePrefix(i + 1, i, result :+ offset)
    }
    @scala.annotation.tailrec
    def calculateSuffix(i: Int = heights.length - 1, offset:Int = heights.length, result: IndexedSeq[Int] = IndexedSeq.empty[Int]): IndexedSeq[Int] = {
      if(i == -1) result
      else if(offset < heights.length && heights(offset) >= heights(i)) calculateSuffix(i, result(heights.length - 1 - offset), result)
      else calculateSuffix(i - 1, i, result :+ offset)
    }
    val prefix = calculatePrefix()
    val suffix = calculateSuffix()
    heights.indices.map(i => heights(i) * (suffix(heights.length - 1 - i) - prefix(i) - 1)).max
  }
  //https://www.geeksforgeeks.org/largest-rectangular-area-in-a-histogram-using-stack/
  def largestRectangleArea_v3(heights: Array[Int]): Int = {
    val n = heights.length
    val stack = scala.collection.mutable.Stack[Int]()
    var result = 0
    heights.indices.foreach(i => {
      while(stack.nonEmpty && heights(stack.head) > heights(i)) {
        val heightIdx = stack.pop()
        val width = if(stack.isEmpty) i else i - stack.head - 1
        result = Math.max(result, heights(heightIdx) * width)
      }
      stack.push(i)
    })
    while(stack.nonEmpty) {
      val heightIdx = stack.pop()
      result = Math.max(result, heights(heightIdx) * (if(stack.isEmpty) n else n - stack.head - 1))
    }

    result
  }
}
