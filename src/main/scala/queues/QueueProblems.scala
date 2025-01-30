package queues

import scala.annotation.tailrec

object QueueProblems {
  def main(args: Array[String]): Unit = {
    //testReverseQueue()
    testGenerate()
  }
  /*
  Implement Queue using Stacks
  https://leetcode.com/problems/implement-queue-using-stacks/description/
   */
  class MyQueue() {
    private val outStack: scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack[Int]()
    private val inStack: scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack[Int]()
    def push(x: Int): Unit = {
      inStack.push(x)
    }

    def pop(): Int = {
      flip()
      outStack.pop()
    }

    def peek(): Int = {
      flip()
      outStack.head
    }

    def empty(): Boolean = {
      flip()
      outStack.isEmpty
    }

    private def flip(): Unit = {
      if(outStack.isEmpty) while(inStack.nonEmpty) outStack.push(inStack.pop())
    }

  }
  /*
  Implement Stack using Queues
  https://leetcode.com/problems/implement-stack-using-queues/description/
   */
  class MyStack() {
    private val queue: scala.collection.mutable.Queue[Int] = scala.collection.mutable.Queue[Int]()
    def push(x: Int): Unit = {
      queue.enqueue(x)
      (1 until queue.size).foreach(_ => {
        queue.enqueue(queue.head)
        queue.dequeue()
      })
    }

    def pop(): Int = {
      queue.dequeue()
    }

    def top(): Int = {
      queue.head
    }

    def empty(): Boolean = {
      queue.isEmpty
    }

  }


  /*
  Reverse a queue
  https://www.geeksforgeeks.org/reversing-a-queue/
   */
  def testReverseQueue(): Unit = {
    val init = scala.collection.mutable.Queue[Int](1,2,3,4,5,6)
    val reversed = reverseQueue(init)
    reversed.indices.foreach(_ => print(s"${reversed.dequeue()} "))

    val init2 = scala.collection.immutable.Queue[Int](1, 2, 3, 4, 5, 6)
    val reversed2 = reverseQueue(init2)
    reversed2.iterator.foreach(x => print(s"${x} "))
  }
  def reverseQueue(queue: scala.collection.mutable.Queue[Int]): scala.collection.mutable.Queue[Int] = {
    val stack = scala.collection.mutable.Stack[Int]()
    while(queue.nonEmpty) stack.push(queue.dequeue())
    while(stack.nonEmpty) queue.enqueue(stack.pop())
    queue
  }
  def reverseQueue(queue: scala.collection.immutable.Queue[Int]): scala.collection.immutable.Queue[Int] = {
    @tailrec
    def helper(remaining: scala.collection.immutable.Queue[Int], stack: List[Int], result: scala.collection.immutable.Queue[Int]): scala.collection.immutable.Queue[Int] = {
      if(remaining.nonEmpty) helper(remaining.tail, remaining.head +: stack , result)
      else if(stack.nonEmpty) helper(remaining, stack.tail, result.enqueue(stack.head))
      else result
    }
    helper(queue, List.empty[Int], scala.collection.immutable.Queue.empty[Int])
  }

  /*
  Generate Binary Numbers
  https://www.geeksforgeeks.org/problems/generate-binary-numbers-1587115620/1
   */
  def testGenerate(): Unit = {
    println(generate_v2(10))
  }
  def generate(n: Int): List[String] = {
    val result = scala.collection.mutable.ListBuffer[String]()
    val queue = scala.collection.mutable.Queue[String]()
    queue.enqueue("1")
    (0 until n).foreach(_ => {
      result.addOne(queue.head)
      queue.enqueue(queue.head + "0")
      queue.enqueue(queue.head + "1")
      queue.dequeue()
    })
    result.toList
  }

  def generate_v2(n: Int): List[String] = {
    @tailrec
    def helper(i: Int, queue: scala.collection.immutable.Queue[String], result: List[String]): List[String] = {
      if(i == n) result.reverse
      else helper(i + 1, queue.tail.enqueue(queue.head + "0").enqueue(queue.head + "1"), queue.head +: result)
    }
    helper(0, scala.collection.immutable.Queue("1"), List.empty[String])
  }
}
