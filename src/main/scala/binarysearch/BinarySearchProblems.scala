package binarysearch

object BinarySearchProblems {
  def main(args: Array[String]): Unit = {
    testLastZero()
  }
  // An array with 0s on the left and 1s on the right. Return the index of the last 0 or -1 if there is none
  def testLastZero(): Unit = {
    assert(lastZero(null) == -1)
    assert(lastZero(Array()) == -1)
    assert(lastZero(Array(0,0,0,0,0)) == 4)
    assert(lastZero(Array(1,1,1,1,1)) == -1)
    assert(lastZero(Array(0)) == 0)
    assert(lastZero(Array(0,0)) == 1)
    assert(lastZero(Array(0,1,1,1)) == 0)
    assert(lastZero(Array(0,0,1,1)) == 1)
    assert(lastZero(Array(0,0,0,1)) == 2)
    assert(lastZero(Array(0,1,1,1,1)) == 0)
    assert(lastZero(Array(0,0,1,1,1)) == 1)
    assert(lastZero(Array(0,0,0,1,1)) == 2)
    assert(lastZero(Array(0,0,0,0,1)) == 3)
  }
  def lastZero(arr: Array[Int]): Int = {
    if(arr == null || arr.isEmpty) return -1
    if(arr.last == 0) return arr.length - 1 // Entire array is 0s
    if(arr(0) == 1) return -1               // Entire array is 1s
    var low = 0
    var high = arr.length
    while(low <= high) {
      val mid = (low + high) / 2
      if(arr(mid) == 0 && arr(mid + 1) == 1) return mid // Index out of bounds exception not possible here because edge case already handled
      if(arr(mid) == 1) high = mid - 1 // If mid is a one then the last zero must be on the left
      else low = mid + 1              // Otherwise last zero is on the right
    }
    -1
  }
}
