package string

import scala.annotation.tailrec

object StringProblems {
  def main(args: Array[String]): Unit = {
    //stringAPIs()
    //testCount()
    //testIsPalindrome()
    //testHasValidParentheses()
    //testCompareVersionNumbers()
    //testReorganizeString()
    //println("hello world".groupBy(identity).map(x => (x._1, x._2.length)))
    //testIsAnagram()
    testMultiple()
  }
  def stringAPIs(): Unit = {
    val hello = "hello"
    println(s"FIRST CHAR: ${hello.head}")
    println(s"REMAINING CHARS: ${hello.tail}")
  }
  def testCount(): Unit = {
    println(count("hello world"))
  }
  def countOLD(str: String): Map[Char, Int] = {
    @tailrec
    def helper(remaining: String, result: Map[Char, Int]): Map[Char, Int] = {
      if(remaining.isEmpty) result
      else if(result.contains(remaining.head)) helper(remaining.tail, result + (remaining.head -> (result(remaining.head) + 1)))
      else helper(remaining.tail, result + (remaining.head -> 1))
    }
    helper(str, Map.empty[Char,Int])
  }
  def count(str: String): Map[Char, Int] = {
    // "aabacba" = Map("a" -> 4, "b" -> 2, "c" -> 1)
    // for each char
    // update map
    @tailrec
    def helper(input: String, result: Map[Char, Int]): Map[Char, Int] = {
      if(input.isEmpty) result
      else {
        val currChar = input.head
        val contains = result.contains(currChar)
        val currVal = if(contains) result(currChar) + 1 else 1
        val newResult = result + (currChar -> currVal)
        helper(input.tail, newResult)
      }
    }
    //helper(str, Map.empty[Char,Int])

    str.groupBy(identity).map(_ -> _.length)
  }
  def testIsPalindrome(): Unit = {
    println(s"abccba: ${isPalindrome("abccba")}")
    println(s": ${isPalindrome("")}")
    println(s"abcba: ${isPalindrome("abcba")}")
    println(s"abcdba: ${isPalindrome("abcdba")}")
  }
  def isPalindrome(str: String): Boolean = {
    val n = str.length
    val (front, back) = str.splitAt(n / 2)
    val backTrim = if(n % 2 ==0) back else back.drop(1)
    front.reverse == backTrim
  }
  def testHasValidParentheses(): Unit = {
    println(hasValidParentheses("()"))
    println(hasValidParentheses("()()"))
    println(hasValidParentheses("(())"))
    println(hasValidParentheses("()(()()"))
    println(hasValidParentheses("()(()))())()"))
    println(hasValidParentheses("())("))
  }
  def hasValidParenthesesOLD(str: String): Boolean = {
    @tailrec
    def helper(remaining:String, open: Int):Boolean = {
      if(open < 0) false
      else if(remaining.isEmpty) if(open == 0) true else false
      else if(remaining.head == '(') helper(remaining.tail, open + 1)
      else  helper(remaining.tail, open - 1)
    }
    helper(str, 0)
  }

  def hasValidParentheses(str: String): Boolean = {
    //"(( ))"
    true
  }
  def testCompareVersionNumbers(): Unit = {
    println(compareVersionNumbers("0.9","1.0.3.4"))
    println(compareVersionNumbers("1.1.0","1.0.3.4"))
    println(compareVersionNumbers("1.1.0", "2.0"))
    println(compareVersionNumbers("2.1", "2.0"))
    println(compareVersionNumbers("2.1", "2.01"))
    println(compareVersionNumbers("2.0.0", "2"))
  }

  // -1: v1 < v2
  //  0: v1 == v2
  //  1: v1 > v2
  def compareVersionNumbersOLD(v1: String, v2: String): Int = {
    val v1Parts = v1.split('.').map(_.toInt)
    val v2Parts = v2.split('.').map(_.toInt)
    @tailrec
    def helper(v1Ver: Seq[Int], v2Ver: Seq[Int]): Int = {
      if(v1Ver.isEmpty || v2Ver.isEmpty) {
        if(v1Ver.nonEmpty && v1Ver.exists(_ != 0)) 1
        else if(v2Ver.nonEmpty && v2Ver.exists(_ != 0)) -1
        else 0
      }
      else if(v1Ver.head != v2Ver.head) if(v1Ver.head > v2Ver.head) 1 else -1
      else helper(v1Ver.tail, v2Ver.tail)
    }
    helper(v1Parts, v2Parts)
  }

  def compareVersionNumbers(v1: String, v2: String): Int = {
    // split strings into lists, convert to int
    // compare lists one index at a time until difference found
    // return result
    val ver1 = v1.split("\\.").toList.map(_.toInt)
    val ver2 = v2.split("\\.").toList.map(_.toInt)
    def helper(ver1: List[Int], ver2: List[Int]): Int = {
      if(ver1.isEmpty && ver2.isEmpty) 0
      else {
        val ver1Curr = if(ver1.isEmpty) 0 else ver1.head
        val ver2Curr = if(ver2.isEmpty) 0 else ver2.head
        if(ver1Curr < ver2Curr) -1
        else if(ver1Curr > ver2Curr) 1
        else helper(if(ver1.isEmpty) ver1 else ver1.tail, if(ver2.isEmpty) ver2 else ver2.tail)
      }
    }
    helper(ver1, ver2)
  }

  // "aaabc" -> "abaca", "acaba"
  // "aaa" -> ""
  // rearrange chars so that no two adjacent chars are identical
  def reorganizeStringOLD(str: String): String = {
    val freqMap = str.groupMapReduce(identity)(_ => 1)(_ + _)
    val n = str.length
    val nRnd = if(n % 2 == 0) n else n + 1
    if(freqMap.maxBy(_._2)._2 > nRnd / 2) return ""
    @tailrec
    def helper(remainingChars: Map[Char, Int], prev: Char = '\u0000', result: String = ""): String = {
      if(remainingChars.isEmpty) result
      else {
        val next = remainingChars.filter(_._1 != prev).maxBy(_._2)
        val nextChar = next._1
        val nextRemaining = next._2 - 1
        val remaining = if(nextRemaining == 0) remainingChars.removed(nextChar) else remainingChars + (nextChar -> nextRemaining)
        helper(remaining, nextChar, result + nextChar)
      }
    }
    helper(freqMap)
  }

  def reorganizeString(str: String): String = {
    if(str.isBlank) return ""
    val freqMap = str.foldLeft(Map.empty[Char,Int])((map,char)=> map + (char -> (map.getOrElse(char,0) + 1)))
    // get the highest freq char (dont do repeats), add the result
    def getMax(map: Map[Char, Int]): Char = {
      map.maxBy(_._2)._1
    }
    def decMap(toDec: Char, map: Map[Char, Int]): Map[Char, Int] = {
      if(map.contains(toDec)) {
        val count = map(toDec) - 1
        if(count < 1) map - toDec
        else map + (toDec -> count)
      } else map
    }
    @tailrec
    def helper(map: Map[Char, Int], prev: Char, result: String): String = {
      if(map.isEmpty) result
      else {
        val curr = getMax(map.filter(_._1 != prev))
        helper(decMap(curr, map), curr, result + curr)
      }
    }
    val mostFreq = getMax(freqMap)
    val totalCount = str.length // 10
    val countOfMostFreq = freqMap(mostFreq) // 6
    val countOfOthers = totalCount - countOfMostFreq // 4
    if(countOfMostFreq - countOfOthers > 1) ""
    else helper(decMap(mostFreq, freqMap), mostFreq, mostFreq.toString)
  }

  def testReorganizeString(): Unit = {
    println(reorganizeString("aaabc"))
    println("TEST2:" + reorganizeString("aaaaaabbbc"))
    println(reorganizeString(""))
  }

  def testIsAnagram(): Unit = {
    assert(isAnagram("desserts","stressed"))
    assert(!isAnagram("Scala","Haskell"))
  }

  def isAnagram(str1: String, str2: String): Boolean = {
    val str1Counts = count(str1)
    //val str2Counts = count(str2)
    //str2Counts == str1Counts
    @tailrec
    def helper(input: String, result: Map[Char, Int]): Boolean = {
      if(input.isEmpty) true
      else {
        val currChar = input.head
        val contains = result.contains(currChar)
        val currVal = if(contains) result(currChar) - 1 else -1
        if(currVal == -1) false
        else helper(input.tail, result + (currChar -> currVal))
      }
    }
    helper(str2, str1Counts)
  }

  def testGenerateAllValidParentheses(): Unit = {
    //1 = "()"
    //2 = "()()", "(())"
    //3 = "()()()", "(()())",      "(())()", "()(())", "((()))"
  }

  def generateAllValidParentheses(n: Int): List[String] = {
    List.empty[String]
  }
  
  def testJustify(): Unit = {
    val input = "We hold these truths to be self-evident, that all men are created equal, that they are endowed by their Creator with certain unalienable Rights, that among these are Life, Liberty and the pursuit of Happiness. That to secure these rights, Governments are instituted among Men, deriving their just powers from the consent of the governed, That whenever any Form of Government becomes destructive of these ends, it is the Right of the People to alter or to abolish it, and to institute new Government, laying its foundation on such principles and organizing its powers in such form, as to them shall seem most likely to effect their Safety and Happiness. Prudence, indeed, will dictate that Governments long established should not be changed for light and transient causes; and accordingly all experience hath shewn, that mankind are more disposed to suffer, while evils are sufferable, than to right themselves by abolishing the forms to which they are accustomed. But when a long train of abuses and usurpations, pursuing invariably the same Object evinces a design to reduce them under absolute Despotism, it is their right, it is their duty, to throw off such Government, and to provide new Guards for their future security. Such has been the patient sufferance of these Colonies; and such is now the necessity which constrains them to alter their former Systems of Government. The history of the present King of Great Britain is a history of repeated injuries and usurpations, all having in direct object the establishment of an absolute Tyranny over these States. To prove this, let Facts be submitted to a candid world."
    println(input)
    println(justify(input, 50))
  }

  def justify(text: String, width: Int): String = {
    // split string into rows with approximate length
    // add spaces until exact length
    @tailrec
    def buildRows(strings: List[String], curr: List[String], count: Int, result: List[List[String]]): List[List[String]] = {
      if(strings.isEmpty) (curr.reverse :: result).reverse
      else if(count + strings.head.length > width) buildRows(strings, List.empty[String], 0, curr.reverse :: result)
      else buildRows(strings.tail, strings.head :: curr, count + strings.head.length + 1, result)
    }
    def justifyRow(words: List[String]): String = {
      val spots = words.length - 1
      val totalSpace = width - words.map(_.length).sum
      val spaceSize = totalSpace / spots
      val remainingTotalSpace = totalSpace % spots
      val (wordsForBigSpace, wordsForSmallSpace) = words.splitAt(remainingTotalSpace + 1)
      if(wordsForBigSpace.isEmpty) words.mkString(" " * spaceSize)
      else wordsForBigSpace.mkString(" " * (spaceSize + 1)) ++ (" " * spaceSize) ++ wordsForSmallSpace.mkString(" " * spaceSize)
    }
    val words = text.split(" ").toList
    val rows = buildRows(words, List.empty[String], 0, List.empty[List[String]])
    val rowsJustified = rows.map(justifyRow)
    rowsJustified.mkString("\n")
  }

  def testRansonNote(): Unit = {
    val mag = "Hello my name is Yuki and I am a dumb cat"
    assert(ransomNote("Yuki is a dumb cat", mag))
    assert(!ransomNote("Hello my name is Jyojyo", mag))
    assert(!ransomNote("Yuki is a dumb dumb cat", mag))
  }

  def ransomNote(note: String, magazine: String): Boolean = {
      // Mag: Hello my name is Yuki and I am a dumb cat
      // Note: Yuki is a dumb cat
      // Can't make: Hello my name is Jyojyo
      // Can't make: Yuki is a dumb dumb cat

      // parse magazine into frequency map
      // interate over words and check if in map decrment as needed return false if not there
      // return true

      def buildFrequencyMap(words: List[String]): Map[String, Int] = words.foldLeft(Map.empty[String, Int])((acc, curr) => {
        //if(acc.contains(curr)) acc + (curr -> (acc(curr) + 1))
        //else acc + (curr -> 1)
        acc + (curr -> (acc.getOrElse(curr, 0) + 1))
      })
      @tailrec
      def compareWordsToMap(words: List[String], freq: Map[String, Int]): Boolean = {
        if(words.isEmpty) true
        else {
          val curr = words.head
          if(freq.contains(curr)) compareWordsToMap(words.tail,
            if(freq(curr) > 1)
              freq + (curr -> (freq(curr) - 1))
            else freq - curr
          )
          else false
        }
      }

      val magWords = magazine.split(" ").toList
      val freqMap = buildFrequencyMap(magWords)
      val noteWords = note.split(" ").toList
      compareWordsToMap(noteWords, freqMap)
  }

  def testReverseWords() :Unit = {
    println(reverseWords("Alice loves Scala"))
    println(reverseWords("  hello    world  "))
    assert(reverseWords("Alice loves Scala") == "Scala loves Alice")
    assert(reverseWords("  hello    world  ") == "world hello")
  }

  def reverseWords(str: String): String = {
    // split string by space,
    // convert to list,
    // prepend words to result list,
    // build result string from result list
    str.trim.split("\\s+").toList.reverse.mkString(" ")
  }

  def testMultiple(): Unit = {
    println(multiple("53601","12345"))
  }

  def multiple(x: String, y: String): String = {
    def multiplyPart(n: Int, pow: Int, str: String): String = {
      if(n == 0) return "0"
      @tailrec
      def helper(remaining: String, carry: Int, result: String): String = {
        if (remaining.isEmpty) if (carry > 0) carry.toString ++ result else result
        else {
          val currDig = remaining.head
          val currProd = (currDig.asDigit * n) + carry
          val currRes = currProd % 10
          val newCarry = currProd / 10
          helper(remaining.tail, newCarry, currRes.toString ++ result)
        }
      }

      helper(str.reverse, 0, "") ++ ("0" * pow)
    }
    def combinePart(str1:String, str2:String) = {
      @tailrec
      def helper(carry: Int, remaining1: String, remaining2: String, result: String): String = {
        if(remaining1.isEmpty && remaining2.isEmpty) result
        else {
          val dig1 = if(remaining1.isEmpty) 0 else remaining1.head.asDigit
          val dig2 = if(remaining2.isEmpty) 0 else remaining2.head.asDigit
          val currSum = dig1 + dig2 + carry
          val currRes = currSum % 10
          val newCarry = currSum / 10
          helper(newCarry,if(remaining1.isEmpty) remaining1 else remaining1.tail, if(remaining2.isEmpty) remaining2 else remaining2.tail, currRes.toString ++ result)
        }
      }
      helper(0, str1.reverse, str2.reverse, "")
    }
    x.reverse.zipWithIndex
      .map((digStr, idx)=>multiplyPart(digStr.asDigit, idx, y))
      .reduce(combinePart)
  }
}
