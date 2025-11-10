package part2

object Semigroups {
  import cats.Semigroup
  import cats.instances.int._
  private val naturalIntSemigroup = Semigroup[Int]
  private val intCombination = naturalIntSemigroup.combine(2, 46) // 48

  import cats.instances.string._
  private val naturalStringSemigroup = Semigroup[String]
  private val stringCombination = naturalStringSemigroup.combine("I love ", "cats!") //

  def reduceInts(ints: List[Int]): Int = ints.reduce(naturalIntSemigroup.combine)
  def reduceStrings(strings: List[String]): String = strings.reduce(naturalStringSemigroup.combine)

  // What if we could define a general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO 1: support a new type
  // hint1: use the same pattern we used with Eq
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (e1, e2) =>
    Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // extension methods for Semigroups - combine is available as |+|
  import cats.syntax.semigroup._
  val combinedInts = 1 |+| 2 // 3
  val combinedExpense = Expense(1, 100.0) |+| Expense(2, 250.0) // Expense(2, 350.0)

  // TODO 2: implement reduceThings2
  // 注意和reduceThings1的区别，这里不需要传入implicit参数，直接用context bound！
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val strings = List("I ", "love ", "Scala ", "and ", "Cats!")
    println(reduceThings(numbers))
    println(reduceThings(strings))

    import cats.instances.option._
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions))

    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringOptions))

    val expenses = List(
      Expense(1, 100.50),
      Expense(2, 200.75),
      Expense(3, 50.25)
    )
    println(reduceThings(expenses))

    println(reduceThings2(expenses))
  }
}
