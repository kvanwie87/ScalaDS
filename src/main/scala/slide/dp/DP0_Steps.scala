package slide.dp

object DP0_Steps {
  def main(args: Array[String]): Unit = {
    // 0,1,2,3,4,5,6
    //(1,1,2,4,7)
   // println(possibleWays(0)) //
   // println(possibleWays(1))
   // println(possibleWays(2))
   // println(possibleWays(3))
    //(0 until 10).foldLeft(0)((sum, curr)=> sum + curr)
  }
  def possibleWays(n: Int): Int = {
    (0 until n).foldLeft((0,0,1))((previousSubProblems, index) => {
      val currSubProblemValue = previousSubProblems._1 + previousSubProblems._2 + previousSubProblems._3
      (previousSubProblems._2, previousSubProblems._3, currSubProblemValue)
    })._3

    //(0 until n).foldLeft((0,0,1))((acc, i) => (acc._2, acc._3, acc._1 + acc._2 + acc._3))._3
  }
}
