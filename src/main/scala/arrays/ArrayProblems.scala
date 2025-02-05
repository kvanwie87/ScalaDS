package arrays

import scala.annotation.tailrec

object ArrayProblems {
  def main(args: Array[String]): Unit = {
    //val input = Array(1,2,3,4,5,6)
    //rotate_v3(Array(1,2,3,4,5,6), 100)
    //input.foreach(print)
    testFindLHS()
  }
  /*
  https://leetcode.com/problems/rotate-array/description/
  Given an integer array nums, rotate the array to the right by k steps, where k is non-negative.
   */
  def rotate_v1(nums: Array[Int], k: Int): Unit = {
    val result = new Array[Int](nums.length)
    nums.indices.foreach(i => {
      val offset = (i + k) % nums.length
      result(offset) = nums(i)
    })
    result.indices.foreach(i=> {
      nums(i) = result(i)
    })
  }

  def rotate_v2(nums: Array[Int], k: Int): Unit = {
    val normalizeK = k % nums.length
    def reverse(start: Int, stop: Int): Unit = {
      (start to stop).lazyZip(stop to start by -1).takeWhile((i,j) => i<=j).foreach((i,j) => {
        val temp = nums(i)
        nums(i) = nums(j)
        nums(j) = temp
      })
    }
    reverse(0, nums.length - 1)
    reverse(0, normalizeK - 1)
    reverse(normalizeK, nums.length - 1)
  }

  def rotate_v3(nums: Array[Int], k: Int): Unit = {
    // see rotate_v1 above
    // for loops in scala are syntactic sugar for foreach so it is easy to switch between them
    val result = new Array[Int](nums.length)
    for(i <- nums.indices) {
      val offset = (i + k) % nums.length
      result(offset) = nums(i)
    }
    for(i <- result.indices) {
      nums(i) = result(i)
    }
  }

  /*
  Longest Harmonious Subsequence
  https://leetcode.com/problems/longest-harmonious-subsequence/description/?envType=problem-list-v2&envId=sliding-window
  We define a harmonious array as an array where the difference between its maximum value and its minimum value is exactly 1.

  Given an integer array nums, return the length of its longest harmonious subsequence among all its possible subsequences.
  Input: nums = [1,3,2,2,5,2,3,7]
  Output: 5
  Explanation: The longest harmonious subsequence is [3,2,2,2,3].
   */
  def testFindLHS(): Unit = {
    println(findLHS_v4(Array(1,3,2,2,5,2,3,7)))
  }
  def findLHS_v1(nums: Array[Int]): Int = {
    val numsSorted = nums.sorted
    var (left,right) = (0,1)
    var max = 0;
    while(right <= nums.length - 1) {
      val diff = numsSorted(right) - numsSorted(left)
      if(diff == 1) {
        max = Math.max(max, right - left + 1)
        right = right + 1
        }
      else if(diff > 1) {
        left = left + 1
      } else {
        right = right + 1
      }
    }
    max
  }

  def findLHS_v2(nums: Array[Int]): Int = {
    val mapOfNums: Map[Int,Int] = nums.foldLeft(Map.empty[Int,Int])((acc, x) => acc + ((x, acc.getOrElse(x, 0) + 1)))
    mapOfNums.map(entry => {
      if(mapOfNums.contains(entry._1 + 1)) entry._2 + mapOfNums(entry._1 + 1)
      else 0
    }).max
  }

  def findLHS_v3(nums: Array[Int]): Int = {
    val numsSorted = nums.sorted
    @tailrec
    def helper(left: Int, right: Int, max: Int): Int = {
      if(right == nums.length) max
      else {
        val diff = numsSorted(right) - numsSorted(left)
        if (diff == 1) helper(left, right + 1, Math.max(max, right - left + 1))
        else if (diff > 1) helper(left + 1, right, max)
        else helper(left, right + 1, max)

      }
    }
    helper(0,1,0)
  }

  def findLHS_v4(nums: Array[Int]): Int = {
    // See findLHS_v2
    // Any foldLeft can be converted to a for loop by using a mutable variable in place of the accumulator and doing a regular interation over the range/collection
    val mapOfNums: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map.empty[Int, Int]
    for(x <- nums) {
      mapOfNums.put(x, mapOfNums.getOrElse(x, 0) + 1)
    }
    (for(entry <- mapOfNums) yield { // for comprehension are just syntactic sugar for maps and flatMap so you can easily convert between them
      if (mapOfNums.contains(entry._1 + 1)) entry._2 + mapOfNums(entry._1 + 1)
      else 0
    }).max
  }
}
