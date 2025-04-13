package basics.inheritance

object Inheritance_1 {
  /***
   * Extending a class
   ***/
  class Person {
    final def something(): Int = 1 // final means this method cannot be overridden
    var name: String = ""
    def greet(): String = s"Hello, my name is $name"
  }
  // You form a subclass in Scala by using the extends keyword
  // The subclass inherits all the fields and methods of the superclass
  class Employee extends Person {
    // The body of the subclass will specify the fields and methods that are specific to the subclass
    var employeeId: Int = 0
    // You can override methods of the superclass in the subclass
    override def greet(): String = s"Hello, my name is $name and my employee ID is $employeeId"

    // override def something(): Int = 2 // This will not compile because something() is final in the superclass
  }

  /** *
   * Overriding methods
   * * */
  // Nonfinal methods can be overridden in subclasses
  // You can invoke the superclass method using super
  class Student extends Person {
    var studentId: Int = 0
    override def greet(): String = {
      val superGreeting = super.greet() // Calls the greet method of the superclass
      s"$superGreeting and my student ID is $studentId"
    }
  }

  /** *
   * Type checks and casting
   * * */
  // You can check the type of an object using isInstanceOf and cast it using asInstanceOf
  val person: Person = new Employee
  if (person.isInstanceOf[Employee]) { // Check if person is an instance of Employee
    val employee: Employee = person.asInstanceOf[Employee] // Cast person to Employee
    println(employee.greet())
  } else {
    println(person.greet())
  }
  // This is not recommended in Scala, as it is better to use pattern matching
  person match
    case employee: Employee =>
      println(employee.greet())
    case _ =>
      println(person.greet())

  /** *
   * Constructors
   * * */
  class Animal(val name: String) { // A class has one primary constructor which is intertwined with the class definition
    var age: Int = 0
    var weight: Double = 0.0
    def this(name: String, age: Int) = { // A class can have multiple auxiliary constructors
      this(name) // All auxiliary constructors must call the primary constructor or another auxiliary constructor
      this.age = age
    }
    def this(name: String, age: Int, weight: Double) = { // Another auxiliary constructor
      this(name, age) // Calls another auxiliary constructor
      this.weight = weight
    }
    def speak(): String = s"$name makes a sound"
  }
  class Dog(name: String, age: Int) extends Animal(name, age) { // The primary constructor of the subclass must call a constructor of the superclass
    override def speak(): String = s"$name barks"
    def this(name: String, age: Int, weight: Double) = { // An auxiliary constructor in the subclass
      this(name, age) // Must call primary constructor or another auxiliary constructor but can not call the superclass constructor directly
      this.weight = weight
    }
  }

  // Anonymous classes

  // Abstract classes

  // Abstract fields

  // Overriding fields

  // Open and sealed classes

  // Protected fields and methods

  // Constructor order

  // Scala inheritance hierarchy

  // Object equality
}
