package loopconstructs

object WhileLoops {
  def main(args: Array[String]): Unit = {
    // Scala has while loops, nothing special with them really pretty much exactly like java
    var i = 100
    while (i > 0) {
      i -= 1
      //println(i)
    }


    // Older versions of scala have a do while loop but they removed it in Scala 3
    // https://docs.scala-lang.org/scala3/reference/dropped-features/do-while.html
    // var x = 10
    //do {
   //   println(x)
    //  x -= 1
   // } while(x > 0)

    // Can get the same effect as a do while by doing
   /*
   while ( { <body>; <cond>}) {} // here the body will get executed before the cond gets checked
          */
   var x = 10
   while({
     println(s"Number: ${x}")
     x -= 1
     x > 0
   }){}
  }
}
