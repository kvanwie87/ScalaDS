package linkedlist

import scala.annotation.tailrec

object LinkedListProblems {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def main(args: Array[String]): Unit = {
    //testGetDecimalValue()
    //testHasCycle()
    testIsPalindrome()
  }
  /*
  https://leetcode.com/problems/convert-binary-number-in-a-linked-list-to-integer/?envType=problem-list-v2&envId=linked-list
  Convert Binary Number in a Linked List to Integer
   */
  def testGetDecimalValue(): Unit = {
    val test1_0 =  new ListNode(1, null)
    val test1_1 = new ListNode(0, test1_0)
    val test1_2 =  new ListNode(1, test1_1)
    getDecimalValue_v2(test1_2) // 101 = 5
  }
  def getDecimalValue_v1(head: ListNode): Int = {
    var curr = head
    var result = 0
    while(curr != null) {
      result *= 2
      result += curr.x
      curr = curr.next
    }
    result
  }
  def getDecimalValue_v2(head: ListNode): Int = {
    @tailrec
    def helper(curr: ListNode, result: Int): Int = {
      if(curr == null) result
      else helper(curr.next, (result * 2) + curr.x)
    }
    helper(head, 0)
  }

  /*
  https://leetcode.com/problems/linked-list-cycle/description/?envType=problem-list-v2&envId=linked-list
  Linked List Cycle
  Given head, the head of a linked list, determine if the linked list has a cycle in it.
  There is a cycle in a linked list if there is some node in the list that can be reached again by continuously following the next pointer.
  Return true if there is a cycle in the linked list. Otherwise, return false.
   */
  def testHasCycle(): Unit = {
    val test1_0 = new ListNode(-4, null)
    val test1_1 = new ListNode(0, test1_0)
    val test1_2 = new ListNode(2, test1_1)
    val test1_3 = new ListNode(3, test1_2)
    test1_0.next = test1_2
    assert(hasCycle(test1_3))
    test1_0.next = null
    assert(!hasCycle(test1_3))
  }
  def hasCycle_v1(head: ListNode): Boolean = {
    if(head == null || head.next == null)
      return false

    var slow = head;
    var fast = head.next
    while(fast != slow) {
      if(fast.next == null || fast.next.next == null)
        return false
      slow = slow.next
      fast = fast.next.next
    }
    true
  }
  def hasCycle(head: ListNode): Boolean = {
    @tailrec
    def helper(slow: ListNode, fast: ListNode): Boolean = {
      if(fast == null || fast.next == null) false
      else if(slow == fast) true
      else helper(slow.next, fast.next.next)
    }
    helper(head, if(head == null) null else head.next)
  }

  /*
  https://leetcode.com/problems/palindrome-linked-list/?envType=problem-list-v2&envId=linked-list
  Palindrome Linked List
  Given the head of a singly linked list, return true if it is a palindrome or false otherwise.
   */
  def testIsPalindrome(): Unit = {
    val test1_0 = new ListNode(1, null)
    val test1_1 = new ListNode(2, test1_0)
    val test1_2 = new ListNode(2, test1_1)
    val test1_3 = new ListNode(1, test1_2)
    assert(isPalindrome(test1_3))
    printLL(test1_3)
  }
  // Could use extra space to solve list copies, stacks, recursion
  def isPalindrome(head: ListNode): Boolean = {
    //printLL(head)
    def reverse(start: ListNode): ListNode = {
      var curr = start
      var prev: ListNode = null
      while(curr != null){
        val next = curr.next
        curr.next = prev
        prev = curr
        curr = next
      }
      prev
    }

    var slow = head
    var fast = head.next
    while(fast != null && fast.next != null) {
      slow = slow.next
      fast = fast.next.next
    }

    var start = head
    var rev = reverse(slow.next)
    while(rev != null) {
      if(rev.x != start.x) {
        return false
      }
      start = start.next
      rev = rev.next
    }
    true
  }

  def printLL(head: ListNode): Unit = {
    var curr = head
    while(curr != null) {
      print(s"${curr.x} ")
      curr = curr.next
    }
    println()
  }
}
