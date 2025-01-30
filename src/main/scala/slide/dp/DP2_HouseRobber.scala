package slide.dp

object DP2_HouseRobber {
  def main(args: Array[String]): Unit = {
    println(maxRob(IndexedSeq.empty[Int]))
    println(maxRob(IndexedSeq(22,12,8,15,20,17)))
  }
  def maxRob(houses: IndexedSeq[Int]): Int = {
    houses.indices.foldLeft((0,0))((prev, idx)=>(prev._2, Math.max(houses(idx) + prev._1, prev._2)))._2
  }
}
