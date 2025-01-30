package slide.dp

object DP1_RodCutting {
  def main(args: Array[String]): Unit = {
  // length 1,2,3,4,5, 6, 7, 8
  // price  1,5,8,9,10,17,17,20
  // Rod 8 = 2+6 = 5 + 17 = 22
  println(maxValue(8, IndexedSeq(1,2,3,4,5, 6, 7, 8), IndexedSeq(1,5,8,9,10,17,17,20)))
  }
  def maxValue(rodSize: Int, length: IndexedSeq[Int], value: IndexedSeq[Int]): Int = {
    val lengthValues = length.zip(value)
    def currMaxValue(subProblems: IndexedSeq[Int], currRodLength: Int): Int = {
      lengthValues.filter((len,v) => len <= currRodLength).foldLeft(0)((currMax, lengthValue) => Math.max(currMax, lengthValue._2 + subProblems(currRodLength - lengthValue._1)))
    }
    (1 to rodSize).foldLeft(IndexedSeq(0))((acc, i) => acc :+ currMaxValue(acc, i))(rodSize)
  }
}
