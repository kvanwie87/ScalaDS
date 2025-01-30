package slide.dp

object DP1_Segmentable {
  def main(args: Array[String]): Unit = {
    println(isSegmentable("oncoda", Set("a","one","on","cod","code")))
  }
  def isSegmentable(str: String, dict: Set[String]): Boolean = {
    def canSegmentSubproblem(subProblems: IndexedSeq[Boolean], i: Int): Boolean = {
      val substr = str.take(i)
      (0 until substr.length).exists(sliceIdx => dict(substr.drop(sliceIdx)) && subProblems(sliceIdx))
    }
    (1 to str.length).foldLeft(IndexedSeq[Boolean](true))((subs, idx) => subs :+ canSegmentSubproblem(subs,idx))(str.length)
  }
}
