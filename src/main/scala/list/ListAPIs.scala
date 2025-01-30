package list

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.math
import scala.util.Random

object ListAPIs {
  def listAPIExamples() = {
    val sampleList = List(1, 2, 3)
    val emptyList = List.empty // Same as Nil
    println(s"HEAD: ${sampleList.head}")
    println(s"TAIL: ${sampleList.tail}")
    println(s"EMPTY: ${sampleList.isEmpty}")

    println(s"PREPEND1: ${0 :: sampleList}")
    println(s"PREPEND2: ${0 +: sampleList}")
    println(s"PREPEND3: ${sampleList.prepended(0)}")


    List(1, sampleList)

    println(s"APPEND1: ${sampleList :+ 4}")
    println(s"APPEND2: ${sampleList.appended(4)}")

    println(s"CONCAT1: ${List(1, 2) ++ List(3, 4)}")
    println(s"CONCAT2: ${List(1, 2).concat(List(3, 4))}")

    println(List(0, 1) ::: List(2, 3))


    println(List(0, 1, 2, 3).map(x => x + 1))
    println(List(0, 1, 2, 3).filter(x => x % 2 == 0)) // Positive filter
    println(List(0, 1, 2, 3).filterNot(x => x % 2 == 0)) // Negative filter
    println(List(0, 1, 2, 3).foldLeft("")((acc, curr) => acc + curr)) //0123
    println(List(0, 1, 2, 3).foldRight("")((curr, acc) => acc + curr)) //3210

  }
  def main(args: Array[String]): Unit = {
    //println(ListAPIs.get(4,List(0,1,2,3)))
    //val bigList = (1 to 100000).toList
   // println(ListAPIs.length(bigList))
   //println(ListAPIs.reverseViaRecursion(List(0,1,2,3)))
    
   println(ListAPIs.insertionSort2(List(0, 4, 2, 3, 1, 5))((x,y) => y-x))
    println(ListAPIs.insertionSort2(List(0, 4, 2, 3, 1, 5))(new Ordering[Int]{
      override def compare(x: Int, y: Int): Int = y - x
    }))
    //Println(ListAPIs.removeAt(3,List(0,1,2,3,4,5,6)))
    //println(ListAPIs.rle(List(1,1,2,3,3,3,3,3,4,4,4,5,5,6)))
    //println(ListAPIs.rle(List()))
    //println(ListAPIs.duplicateEach(List(1,2,3), 3))
    //println(ListAPIs.shiftLeft(List(1,2,3,4,5,6,7,8), 3))
    //println(ListAPIs.sampleKTimes(List(1,2,3,4,5,6,7,8),5))
    //println(ListAPIs.mergeSortedLists(List(1,2,3),List(0,2,4),Ordering.Int))
    //println(ListAPIs.mergeSortRecursive(List(4,5,1,2,5,1,0,4,3),Ordering.Int))
    //println(ListAPIs.partitionByHead(List(4,5,1,2,5,1,0,4,3),Ordering.Int))
    //println(ListAPIs.quickSort(List(4,5,1,2,5,1,0,4,3),Ordering.Int))
    //testDuplicate()
    //println(insertIntoResult(-5, List(), List(-2,-1,0,1,2,4,5,6,7)))
  }

  def get[B](index: Int, list: List[B]): B =  {
    @tailrec
    def helper(curr: Int, list: List[B]): B = {
      if(curr < 0 || list.isEmpty) throw new NoSuchElementException()
      else if(curr == 0)  list.head
      else helper(curr - 1, list.tail)
    }
    helper(index, list)
  }
  def length[B](list: List[B]): Int = list.foldLeft(0)((count, curr) => count + 1)
  def lengthViaRecursion[B](list: List[B]): Int = {
    @tailrec
    def lengthTailRec(count: Int, list: List[B]): Int = {
      if (list.isEmpty) count
      else lengthTailRec(count + 1, list.tail)
    }
    lengthTailRec(0,list)
  }

  def reverse[B](list: List[B]): List[B] = list.foldLeft(List.empty)((acc, curr) => curr :: acc)

  def reverseViaRecursion[B](list: List[B]): List[B] = {
    @tailrec
    def reverseTailRec(reverseList: List[B], list: List[B]): List[B] = {
      if(list.isEmpty) reverseList
      else reverseTailRec(list.head :: reverseList, list.tail)
    }
    reverseTailRec(List.empty, list)
  }

  def concatViaRecursion[B](list1: List[B], list2: List[B]) = {
    @tailrec
    def concatTailrec(remaining: List[B], acc: List[B]) : List[B] = {
      if(remaining.isEmpty) acc
      else concatTailrec(remaining.tail, remaining.head :: acc)
    }
    concatTailrec(list1.reverse, list2)
  }

  def concat[B](list1: List[B], list2: List[B]) = list1.foldRight(list2)((curr,acc) => curr :: acc)

  def removeAt[B](index: Int, list: List[B]): List[B] = {
    @tailrec
    def helper(curr: Int, remaining: List[B], result: List[B]): List[B] = {
      if (index == curr) result.reverse ++ remaining.tail
      else helper(curr + 1, remaining.tail, remaining.head +: result)
    }
    helper(0, list, List.empty[B])
  }

  def runLengthEncoding[B](list: List[B]): List[(B, Int)] = {
    if(list.isEmpty) List.empty[(B, Int)]
    @tailrec
    def helper(prev: B, count: Int, remaining: List[B], result: List[(B, Int)]): List[(B, Int)] = {
      // end of list
      // curr == prev
      // curr != prev
      if(remaining.isEmpty) ((prev,count) :: result).reverse
      else if(remaining.head == prev)
        helper(remaining.head, count + 1, remaining.tail, result)
      else
        helper(remaining.head, 1, remaining.tail, (prev,count) :: result)
    }

    helper(list.head, 1, list.tail, List.empty[(B, Int)])
  }

  def duplicateEach[B](list: List[B], count: Int): List[B] = {
    @tailrec
    def helper(remaining: List[B], curr: Int, result: List[B]) : List[B] = {
      if(remaining.isEmpty) result.reverse
      else if(curr == count) helper(remaining.tail, 0, result)
      else helper(remaining, curr + 1, remaining.head :: result)
    }
    helper(list, 0, List.empty[B])
  }

  def shiftLeft[B](list: List[B], count: Int): List[B] = {
    @tailrec
    def helper(curr: Int, remaining: List[B], shift: List[B]) : List[B] ={
      if(curr == count) remaining ++ shift.reverse
      else helper(curr + 1, remaining.tail, remaining.head :: shift)
    }
    helper(0, list, List.empty[B])
  }

  def sample[B](list: List[B], size: Int): List[B] = {
    if(list.isEmpty || size < 0) return List.empty[B]
    val random = new Random(System.currentTimeMillis())
    val maxIndex = list.length
    @tailrec
    def helper(count: Int, list: List[B], result: List[B]): List[B] = {
      if(count == size) result
      else helper(count + 1, list, list(random.nextInt(maxIndex)) :: result)
    }
    helper(0,list,List.empty[B])
  }

  def insertionSort[B](list: List[B], ordering: Ordering[B]): List[B] = list.foldLeft(List.empty[B])((sorted, curr) => {
    val (before, after) = sorted.partition(ordering.lteq(_, curr))
    (before :+ curr) ++ after
  })

  def insertionSortRecursion[B](list: List[B], ordering: Ordering[B]): List[B] = {
   @tailrec
   def insertIntoSorted(toInsert: B, before: List[B], after: List[B]): List[B] = {
     if (after.isEmpty || ordering.lteq(toInsert, after.head)) before.reverse ++ (toInsert +: after)
     else insertIntoSorted(toInsert, after.head +: before, after.tail)
   }

   @tailrec
   def foreachTailRec(remaining: List[B], sorted: List[B]): List[B] = {
     if (remaining.isEmpty) sorted
     else foreachTailRec(remaining.tail, insertIntoSorted(remaining.head, List.empty[B], sorted))
   }

   foreachTailRec(list, List.empty[B])
 }

  def mergeSort[B](list: List[B], ordering: Ordering[B]): List[B] = list.map(List(_)).reduce((x,y) => mergeSortedLists(x,y,ordering))

  def mergeSortRecursive[B](list: List[B], ordering: Ordering[B]): List[B] = {
    @tailrec
    def helper(currPhase: List[List[B]], nextPhase: List[List[B]]) :List[B] = {
      if(currPhase.isEmpty) {
        if(nextPhase.tail.isEmpty) nextPhase.head
        else helper(nextPhase, List.empty[List[B]])
      }
      else if (currPhase.tail.isEmpty) helper(currPhase.head :: nextPhase, List.empty[List[B]])
      else {
        val sortedList1 = currPhase.head
        val sortedList2 = currPhase.tail.head
        val merged = mergeSortedLists(sortedList1, sortedList2, ordering)
        helper(currPhase.tail.tail, merged :: nextPhase)
      }
    }
    helper(list.map(List(_)), List.empty[List[B]])
  }
  def mergeSortedLists[B](list1: List[B], list2: List[B], ordering: Ordering[B]): List[B] = {
    @tailrec
    def helper(remaining1: List[B], remaining2: List[B], result: List[B]): List[B] = {
      if(remaining1.isEmpty) result.reverse ++ remaining2
      else if(remaining2.isEmpty) result.reverse ++ remaining1
      else if(ordering.lteq(remaining1.head, remaining2.head))
        helper(remaining1.tail, remaining2, remaining1.head :: result)
      else
        helper(remaining1, remaining2.tail, remaining2.head :: result)
    }
    helper(list1, list2, List.empty[B])
  }

  def quickSort[B](list: List[B], ordering: Ordering[B]): List[B] = {
    @tailrec
    def helper(remaining:List[List[B]], result: List[B]): List[B] = {
      if(remaining.isEmpty) result.reverse
      else if(remaining.head.isEmpty) helper(remaining.tail,result)
      else if(remaining.head.tail.isEmpty) helper(remaining.tail, remaining.head.head :: result)
      else {
        val (lt, piv, gt) = partitionByHead(remaining.head, ordering)
        helper(lt :: piv :: gt :: remaining.tail, result)
      }
    }
    helper(List(list), List.empty[B])
  }


  def partitionByHead[B](list: List[B], ordering: Ordering[B]): (List[B],List[B],List[B]) = {
    if(list.isEmpty) return (List.empty[B], List.empty[B], List.empty[B])
    val pivot = list.head
    @tailrec
    def helper(remaining: List[B], lessThan: List[B], greaterThanEq: List[B]): (List[B], List[B], List[B]) = {
      if(remaining.isEmpty) (lessThan, List(pivot), greaterThanEq)
      else if(ordering.lt(remaining.head, pivot))
        helper(remaining.tail, remaining.head :: lessThan, greaterThanEq)
      else helper(remaining.tail, lessThan, remaining.head :: greaterThanEq)
    }
    helper(list.tail, List.empty[B], List.empty[B])
  }

  
  def rle[B](list: List[B]): List[(B,Int)] = {
    if(list.isEmpty) return List.empty[(B,Int)]
    @tailrec
    def helper(prev: B, count: Int, remaining: List[B], result: List[(B,Int)]): List[(B,Int)] = {
      if(remaining.isEmpty) ((prev, count) :: result).reverse
      else if (prev == remaining.head) helper(prev, count + 1, remaining.tail, result)
      else helper(remaining.head, 1, remaining.tail, (prev, count) :: result)
    }
    helper(list.head, 1, list.tail, List.empty[(B,Int)])
  }

  def testDuplicate() : Unit = {
    println(duplicateViaTailRec(List(1,2,3),3))//Expect 1,1,1,2,2,2,3,3,3
  }

  def duplicate[B](nums: List[B], k: Int): List[B] = nums.flatMap(x => (1 to k).map(i => x))

  def duplicateViaTailRec[B](nums: List[B], k: Int): List[B] = {
    @tailrec
    def helper(remaining: List[B], times: Int, result: List[B]):  List[B] = {
     if(remaining.isEmpty) result.reverse
     else if(times < k) helper(remaining, times + 1, remaining.head :: result)
     else helper(remaining.tail, 0, result)
    }
    helper(nums,0,List.empty[B])
  }

  def sampleKTimes[B](list: List[B], k: Int): List[B] = {
    val rand = new Random(System.currentTimeMillis())
    //for 1 to k
    //  val idx = rand.next(list.size)
    //  result.add(list.get(idx))
    val n = list.size
    //(1 to k).toList.map(_ => list(rand.nextInt(n)))
    @tailrec
    def helper(times:Int, result: List[B]): List[B] = {
      if(times > k) result
      else {
        val idx = rand.nextInt(n)
        val sample = list(idx)
        helper(times + 1, sample :: result)
      }
    }
    helper(1, List.empty[B])
  }

  //4,5,2,1,3 -> 1,2,3,4,5

         // before x after
  //7 -> 1,2,4,5
  // while list head is less than 3
  // traverse
  // add 3 once you stop
  def insertionSort2[B](list: List[B])(implicit order: Ordering[B]): List[B] = {
    // for each element
    // insert into proper spot in the result
    @tailrec
    def insertIntoResult(x: B, before: List[B], after: List[B]): List[B] = {
      if(after.isEmpty || order.lt(x, after.head)) before.reverse ::: (x :: after)
      else insertIntoResult(x, after.head :: before, after.tail)
    }

    @tailrec
    def helper(input: List[B], result: List[B]): List[B] = {
      if(input.isEmpty) result
      else helper(input.tail, insertIntoResult(input.head, List.empty[B],result))
    }
    helper(list, List.empty[B])
  }
}
