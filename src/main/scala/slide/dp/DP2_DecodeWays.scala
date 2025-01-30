package slide.dp

object DP2_DecodeWays {
  def main(args: Array[String]): Unit = {
    println((65 to 65 + 25).map(_.toChar).zipWithIndex.map(x=>(x._1,x._2 + 1)))
    println(decodeWays(12))//AB or L
    println(decodeWays(121))//ABA, LA, AW
    //println(decodeWays(121042))
  }
  def decodeWays(encoded: Int): Int = {
    val encodedStr = encoded.toString
    def computeSubproblem(prev: (Int,Int), idx: Int): Int = {
      val lastTwoDigits = encodedStr.slice(idx-1,idx+1).toInt
      if(lastTwoDigits == 10 || lastTwoDigits == 20) prev._1
      else if(lastTwoDigits < 10 || lastTwoDigits > 26) prev._2
      else prev._1 + prev._2
    }
    (1 until encodedStr.length).foldLeft((1,1))((prev, sliceIdx) => (prev._2,computeSubproblem(prev,sliceIdx)))._2
  }
}
