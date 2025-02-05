package loopconstructs

object ForLoops {
  def main(args: Array[String]): Unit = {
    // https://www.baeldung.com/scala/for-loops

    // General structure of for loop
    var generator = 1 to 10
    // for loop will have some generator which could be some kind of range or iterable collection and a receiver that is the variable that will contain the current iteration
    for(receiver <- generator) {
      //do stuff
    }

    for(i <- 0 until 10) {// for(int i = 0; i < 10; i++) in java

    }

    // Can have multiple ranges
    for(i <- 0 to 3; j <- 8 until 12) {
      // This is effectively a nested loop
      /*
        in java
        for(int i = 0; i <= 3; i++) {
          for(int j = 8; j < 12; j++) {

       */
      //println(s"Sample 3: ${i} ${j}")
    }
    //similar to
    for(i <- 0 to 3) {
      for(j <- 8 until 12) {
        //println(s"Sample 3: ${i} ${j}")
      }
    }


    var someList = List("a","b","c")
    for(letter <- someList) { // for(String letter : someList) in java

    }

    // can filter out iterations with if guards
    for(i <- 0 to 100 if i % 2 == 0) {
      //println(s"Number is even: ${i}")
    }

    // if you mouse over <- you will see it is just a foreach and if guards are just withFilter
    (0 to 100).withFilter(i => i % 2 == 0).foreach(i => println(s"Number is even: ${i}"))


    case class Person(name: String, age: Int, weight: Double)
    val people = List(Person("Bob", 18, 150), Person("Jeff", 23, 160), Person("Henry", 32, 220))
    for(Person(name,age,weight) <- people) { // Not really unique to for loops but you can use unapply to deconstruct anything that implements unapply
      //println(s"$name $age $weight")
    }

    // Tuples and Maps deconstruct as below
    val myMap = Map("A" -> 1, "B" -> 2, "C" -> 3)
    for((key,value) <- myMap) {

    }
    val myTuples = List((1,2,3),(2,3,4),(3,4,5))
    for((x,y,z) <- myTuples) {

    }
  }
}
