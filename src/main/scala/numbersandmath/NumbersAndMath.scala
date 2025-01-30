package numbersandmath

import scala.annotation.tailrec
import scala.util.Random

object NumbersAndMath {
  implicit class enhanceNumbers(n: Int) {
    def isPrime: Boolean = NumbersAndMath.isPrime(n)

    def decompose: List[Int] = NumbersAndMath.decompose(n)
  }
  
  def main(args: Array[String]): Unit = {
    //testIsPrime()
    //testDecompose()
    //testApproximatePi()
   // testConstructLargestNumber()
    //testUglyNumber()
    testDuplicates()
    
   // println(2.isPrime)
    //println(21.decompose)
    //testReverseInteger()
    //testParseInteger()
  }

  def testIsPrime(): Unit = {
    assert(isPrime(3))
    assert(isPrime(23))
    assert(!isPrime(21))
    assert(isPrime(2147483629))
    assert(!isPrime(-23))
    assert(!isPrime(0))
    assert(!isPrime(1))
  }

  def isPrime(x: Int): Boolean = {
    //for 2 to x/2
    //  if check is factor
    //    false
    //true
    if(x <= 1) return false
    @tailrec
    def helper(counter: Int):Boolean = {
      if(counter > x/2) true
      else if(x % counter == 0) false
      else helper(counter + 1)
    }
    helper(2)
  }

  def isPrimeOld(x: Int): Boolean = {
    if(x < 2) return false
    @tailrec
    def helper(divisor: Int): Boolean = {
      if(divisor > Math.sqrt(x)) true
      else x % divisor != 0 && helper(divisor + 1)
    }
    helper(2)
  }

  def testDecompose(): Unit = {
    assert(decompose(3) == List(3))
    assert(decompose(23) == List(23))
    assert(decompose(21) == List(3,7))
    assert(decompose(16) == List(2,2,2,2))
    assert(decompose(2147483629) == List(2147483629))
    assert(decompose(-23) == List())
    assert(decompose(0) == List())
    assert(decompose(1) == List())
    assert(decompose(660) == List(2,2,3,5,11))
  }

  def decompose(x: Int): List[Int] = {
    // for 2 to n/2
    //    while factor
    //        add to factors list and value = value / factor
    // if empty add the x
    if (x < 2) return List.empty[Int]
    @tailrec
    def helper(factor: Int, value: Int, result: List[Int]): List[Int] = {
      if(factor > x/2) if(result.isEmpty) x :: result else result.reverse
      else if (value % factor == 0) helper(factor, value/factor, factor::result)
      else helper(factor + 1, value, result)
    }
    helper(2, x, List.empty[Int])
  }
  
  def decomposeOld(x: Int): List[Int] = {
    if(x < 2) return List.empty[Int]

    @tailrec
    def helper(remaining: Int, divisor: Int, result: List[Int]): List[Int] = {
      if(divisor > Math.sqrt(remaining)) (remaining :: result).reverse
      else if(remaining % divisor == 0) helper(remaining / divisor, divisor, divisor :: result)
      else helper(remaining, divisor + 1, result)
    }
    helper(x,2,List.empty[Int])
  }
  
  /***
   * Area of Circle is pi * radius^2
   * Area of circle with radius 1 is therefor pi
   * A square that can encompose that circle is length 2 with a area of 4
   * The circle is pi/4 of the squares area
   * If you generate N points within the circle then roughly N * (pi/4) of them will fall within the circle
   * N is total points, M is points in circle, N * (pi/4) ~= M, pi/4 ~= M/N, pi ~= 4*(M/N)
   * A point is within the circle if x^2 + y^2 < radius^2
   */

  def testApproximatePi(): Unit = {
    println(s"Math.PI: ${Math.PI}")
    println(approximatePi(10))
    println(approximatePi(100))
    println(approximatePi(1000))
    println(approximatePi(10000))
    println(approximatePi(100000))
    println(approximatePi(1000000))
  }

  def approximatePi(n: Int): Double = {
    val random = new Random(System.currentTimeMillis())
    (1 to n).map(_ =>
      val x = random.nextDouble()
      val y = random.nextDouble()
      (x, y)
    ).count((x, y) => x * x + y * y < 1) * 4 / n.toDouble
  }

 /* def testConstructLargestNumber(): Unit = {
    println(constructLargestNumber(List(10,2))) //210
    println(constructLargestNumber(List(3,30,5,9,34))) //9 > 5 > 34 > 3 > 30   //303 or 330
    println(constructLargestNumber(List(2020,20,1010,10,2,22)))
    println(constructLargestNumber(List(1)))
    println(constructLargestNumber(List()))
    println(constructLargestNumber(List(0,0,0)))
  }

  def constructLargestNumber(nums: List[Int]): String = {
    // convert int list to str list
    // sort str list in desc'ing
    // concat str list into a single str
    val strList = nums.map(x => x.toString)
    val strSorted = strList.sorted(new Ordering[String]{
      override def compare(x: String, y: String): Int = {
        @tailrec
        def helper(i: String, j: String): Int = {
          val iFirst = i.head
          val jFirst = j.head
          
        }
        helper(x,y)
      }
    })
    println(strSorted)
    //strSorted.foldLeft("")((acc, curr) => acc ++ curr)
    @tailrec
    def helper(input: List[String], result: String): String = {
      if(input.isEmpty) result
      else helper(input.tail, result ++ input.head)
    }
    helper(strSorted, "")
  }*/

  def constructLargestNumberOld(nums: List[Int]): String = {
    val res = nums.map(_.toString).sorted(Ordering[String].reverse).foldLeft("")(_+_)
    if(res.isEmpty || res.startsWith("0")) "0" else res
  }

  def testUglyNumber(): Unit = {
    println(s"1: ${uglyNumber(1)}")
    println(s"6: ${uglyNumber(6)}")
    println(s"25: ${uglyNumber(25)}")
    println(s"100: ${uglyNumber(100)}")
    println(s"14: ${uglyNumber(14)}")
    println(s"39: ${uglyNumber(39)}")
  }

  // Only the factors 2, 3 and 5
  // 1 is ugly
  // Only positive inputs
  // Ugly: 6, 25, 100
  // Not: 14, 39
  def uglyNumberOLD(x: Int): Boolean = {
    if(x == 1) return true
    else if(x < 1) return false
    @tailrec
    def helper(remaining: Int, divisors: List[Int]): Boolean = {
      if(remaining == 1) true
      else if(divisors.isEmpty) false
      else if(remaining % divisors.head == 0) helper(remaining / divisors.head, divisors)
      else helper(remaining, divisors.tail)
    }
    helper(x, List(2,3,5))
  }


  def uglyNumber(x:Int): Boolean = {
    // 6 = 2 * 3, 25 = 5 * 5, 100 = 5 * 5 * 2 * 2  UGLY ( 1 is considered ugly)
    // 14 = 2 * 7, 39 = 3 * 13 NON-UGLY
    // 6 -> 3 -> 1, 25 -> 5 -> 1, 100 -> 50 -> 25 -> 5 -> 1
    val uglyFactors = List(2,3,5)
    val factors = x.decompose
    //factors.forall(x => uglyFactors.contains(x))
    @tailrec
    def helper(factors: List[Int]): Boolean = {
      if(factors.isEmpty) true
      else if(!uglyFactors.contains(factors.head)) false
      else helper(factors.tail)
    }
    helper(factors)
  }

  def testDuplicates():Unit = {
    println(duplicates(List(1,2,3,4,1,2,3)))
    println(duplicates(List(1)))
    val nums = (1 to 100000).toList
    println(duplicates(nums ++ List(52369426) ++ nums))
  }

  // All numbers appear exactly twice except one: find that number
  // List(1,2,3,4,5,7,1,2,3,4,5,6,7) -> 6
  def duplicatesOLD(list: List[Int]): Int = {
    @tailrec
    def helper(remaining: List[Int], found: Set[Int]): Int = {
      if(remaining.isEmpty) found.head
      else if(found.contains(remaining.head)) helper(remaining.tail, found - remaining.head)
      else helper(remaining.tail, found + remaining.head)
    }
    helper(list, Set.empty[Int])
  }

  def duplicates(list: List[Int]) : Int = {
    //X ^ X => 0
    // 0 ^ Y => Y
    list.foldLeft(0)((acc, x) => acc ^ x)
  }

  def duplicatesBetter(list: List[Int]): Int = list.foldLeft(0)(_ ^ _)

  // 1253 => 3521 if overflow then 0
  // 1253 , 0 -> 125, 0*10 + 3
  // 125,3 -> 12 , 35
  // 12,35 -> 1, 350+2
  // 1,352 -> 0, 3520 + 1
  // 0,3521 -> 3521
  def reverseInteger(x: Int): Int = {
    @tailrec
    def helper(input: Long, result: Long) : Long = {
      if(input == 0) result
      else helper(input / 10, (result * 10) + (input % 10))
    }
    val longRes = helper(x, 0)
    if (longRes > Int.MaxValue || longRes < Int.MinValue) 0 else longRes.toInt
  }

  def testReverseInteger(): Unit = {
    assert(reverseInteger(1253) == 3521)
    assert(reverseInteger(-1253) == -3521)
    //2147483647
    assert(reverseInteger(Int.MinValue) == 0) // -2147483648
    assert(reverseInteger(2147483647) == 0)//7463847412 is out of range
  }

  def testParseInteger(): Unit = {
    //println(parseInteger("7463847412"))
    assert(parseInteger("123") == 123)
    assert(parseInteger("7463847412") == Int.MaxValue)
    assert(parseInteger("-123") == -123)
    assert(parseInteger("   -55") == -55)
    println(parseInteger("-2147483648"))
  }

  def parseInteger(str: String): Int = {
    // str.trim
    // "123" -> 123
    // "7463847412" -> Int.MaxValue
    // "123" -> 1
    val trimmed= str.trim
    val isNeg = trimmed.startsWith("-")
    val trimmedNoSign = if(trimmed.startsWith("-")) trimmed.drop(1) else trimmed
    @tailrec
    def helper(input: String, result: Long, isNeg: Boolean): Int = {
      if(input.isEmpty) if(isNeg) -result.toInt else result.toInt
      else {
        val maybeResult = result * 10 + input.head.toString.toLong
        if(maybeResult > Int.MaxValue && !isNeg) Int.MaxValue
        else if(maybeResult > 1L + Int.MaxValue && isNeg) Int.MinValue
        else helper(input.tail, maybeResult, isNeg)
      }
    }
    helper(trimmedNoSign, 0, isNeg)
  }
}




