package basics.traits

class Traits_1 {
  /** *
   * Traits as interfaces
   * * */
  trait Jumpable {
    val height: Int // Abstract field

    def jump(): String // Abstract method
  }

  trait Swimmable(var speed: Int) {
    def swim(): String // Abstract method
  }

  // A class can implement multiple traits
  class Frog(var swimSpeed: Int) extends Jumpable, Swimmable(swimSpeed) {
    val height: Int = 10 // Must implement abstract field, no override required

    def jump(): String = "Frog jumps!" // Not required to use override

    override def swim(): String = "Frog swims!"
  }

  class Human extends Jumpable with Swimmable(10) { // Can also use with instead of comma
    val height: Int = 5

    override def jump(): String = "Human jumps!"

    override def swim(): String = "Human swims!"
  }

  class Animal {
    def sound: String = "Animal sound"
  }

  class Dog extends Animal with Jumpable { // Can extend a single class and any number of traits
    val height: Int = 5

    override def sound: String = "Woof"

    override def jump(): String = "Dog jumps!"
  }

  /** *
   * Traits with concrete methods and fields
   * * */
  trait Walkable {
    var speed: Int = 10 // Concrete field

    def walk(): String = s"Walking at $speed speed" // Concrete method
  }

  /** *
   * Extending a trait
   * * */
  trait Logger {
    def log(message: String): Unit = println(s"Log: $message") // Concrete method
  }

  trait SpecialLogger extends Logger {
    override def log(message: String): Unit = println(s"Special Log: $message") // Override concrete method
  }

  /** *
   * Combining abstract and concrete methods
   * * */
  trait Logger2 {
    def log(message: String): Unit // Abstract method

    def logWithTimestamp(message: String): Unit = { // Concrete method that uses the abstract method
      val timestamp = java.time.LocalDateTime.now()
      println(s"[$timestamp] $message")
    }
  }

  /** *
   * Instance of class with traits
   * * */
  // You can give an individual instance of a class a trait when it is constructed
  class MyApp {
    def run(): Unit = {
      val frog = new Frog(5)
      println(frog.jump()) // Output: Frog jumps!
      println(frog.swim()) // Output: Frog swims!
      println(frog.height) // Output: 10
      println(frog.speed) // Output: 5
    }
  }

  val app = new MyApp with Logger // Create an instance of MyApp with Logger trait
}

