package slide.dp

object DP2_MaxMeetingWeight {
  case class Meeting(start: Int, end: Int, weight: Int)
  def main(args: Array[String]): Unit = {
    println(maxNonOverlappingMeetings(IndexedSeq(Meeting(0,3,4),Meeting(2,5,1),Meeting(3,6,2),Meeting(1,8,9),Meeting(4,10,12),Meeting(7,11,6),Meeting(5,12,5))))
  }
  def maxNonOverlappingMeetings(meetings: IndexedSeq[Meeting]): Int = {
    def computeSubProblem(dp: IndexedSeq[Int], idx: Int): Int ={
      val prevSub = dp(idx)
      val firstNonOverlapping = (idx-1 to 0 by -1).collectFirst({ case x if (meetings(x).end <= meetings(idx).start) => x + 1 })
      val currSub = meetings(idx).weight + dp(firstNonOverlapping.getOrElse(0))
      Math.max(prevSub,currSub)
    }
    meetings.indices.foldLeft(IndexedSeq(0))((dp, idx) => dp :+ computeSubProblem(dp,idx))(meetings.length)
  }
}
