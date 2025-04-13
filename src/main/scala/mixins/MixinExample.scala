package mixins

object MixinExample {
  abstract class Animal {
    def sound: String
  }
  class Cat extends Animal {
    override def sound: String = "Meow"
  }
  class Dog extends Animal {
    override def sound: String = "Woof"
  }
  class JumpingCat extends Cat, Jumpable {
    override def sound: String = super.sound + " and purring!"
  }

  // Mixin trait
  trait Jumpable {
    def jump(): String = "thing can jump!"
  }

  def main(args: Array[String]): Unit = {
    val cat = new Cat
    val dog = new Dog
    val petCat = new JumpingCat

    println(cat.sound) // Output: Meow
    println(dog.sound) // Output: Woof

    println(petCat.sound) // Output: Meow and purring!
    println(petCat.jump()) // Output: thing can jump!

    // println(cat.play()) // This will not compile, as Cat does not mixin Pet
    val catWithPet = new Cat with Jumpable // Mixing in the Pet trait
    println(catWithPet.jump()) // Output: thing can jump!

    // In java you can do this with anonymous classes but it is more verbose
  }
}
