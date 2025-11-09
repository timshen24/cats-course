package part1intro

import cats.implicits._

object TCVariance {
  class Animal

  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is reversed in the generic type
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "has a T" => covariance, "acts on T" => contravariance
  // variance affect how type class instances are being fetched
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit sm: SoundMaker[T]): Unit = println("Roar!") // implementation omitted

  makeSound[Animal]
  makeSound[Cat] // works because SoundMaker is contravariant

  implicit object OptionSoundMaker extends SoundMaker[Option[Animal]]

  makeSound[Option[Animal]] // works because SoundMaker is contravariant
  makeSound[Some[Animal]] // works because SoundMaker is contravariant

  // covariant type class
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "Animal everywhere"
  }

  implicit object CatShow extends AnimalShow[Cat] {
    override def show: String = "So many cats!"
  }

  private def orgnizeShow[T](implicit event: AnimalShow[T]): String = event.show

  def main(args: Array[String]): Unit = {
    println(orgnizeShow[Cat]) // ok - the compiler will inject CatShow as implicit
//    println(orgnizeShow[Animal]) // will not compile - ambiguity between GeneralAnimalShow and CatShow
    // Cats recommend this:
    Option(2) === Option.empty[Int] // works because Eq is covariant
  }
}
