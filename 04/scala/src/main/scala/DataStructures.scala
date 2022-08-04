object DataStructures extends App {
  // Exercise. Write a function that checks if all values in a `List` are equal.
  // Think about what you think your function should return if `list` is empty, and why.
  @scala.annotation.tailrec
  def allEqual[T](list: List[T]): Boolean = list match {
    case Nil | List(_) => true
    case x1 :: x2 :: t if x1 == x2 => allEqual(t)
    case _ => false
  }

  assert(allEqual(List("a", "a", "a", "a")))
  assert(allEqual(List("a")))
  assert(!allEqual(List("a", "a", "b", "a")))

  // Vegetables
  // ----------
  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val vegetablePrices = Map(
    "tomatoes" -> 4,
    "peppers" -> 5,
    "olives" -> 17,
  )

  val vegetableAmounts = Map(
    "tomatoes" -> 17,
    "peppers" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  // Exercise. Calculate the total cost of all vegetables, taking vegetable amounts (in units) from
  // `vegetableAmounts` and prices per unit from `vegetablePrices`. Assume the price is 10 if not available
  // in `vegetablePrices`.
  val totalVegetableCost: Int =
    vegetableAmounts.map(x => x._2 * vegetablePrices.getOrElse(x._1, 10)).sum

  assert(totalVegetableCost == 5012)

  // Exercise. Given the vegetable weights (per 1 unit of vegetable) in `vegetableWeights` and vegetable
  // amounts (in units) in `vegetableAmounts`, calculate the total weight per type of vegetable, if known.
  //
  // For example, the total weight of "olives" is 2 * 32 == 64.
  val totalVegetableWeights: Map[String, Int] =
    vegetableAmounts.flatMap(x => vegetableWeights.get(x._1).map(z => (x._1, z * x._2)))

  assert(totalVegetableWeights == Map("cucumbers" -> 6460, "olives" -> 64))

  // Exercise: Return a set with all subsets of the provided set `set` with `n` elements
  // For example, `allSubsetsOfSizeN(Set(1, 2, 3), 2) == Set(Set(1, 2), Set(2, 3), Set(1, 3))`.
  // Hints for implementation:
  //   - Handle the trivial case where `n == 1`.
  //   - For other `n`, for each `set` element `elem`, generate all subsets of size `n - 1` from the set
  //     that don't include `elem`, and add `elem` to them.

  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = {
    val arr = set.toVector
    def inner(lo: Int, n: Int): List[List[A]] = n match {
      case 0 => List(Nil)
      case i => for {
        j <- List.range(lo, arr.length)
        ks <- inner(j + 1, i - 1)
      } yield arr(j) :: ks
    }
    inner(0, n).map(_.toSet).toSet
  }

  assert(allSubsetsOfSizeN(Set(1, 2, 3), 2) == Set(Set(1, 2), Set(2, 3), Set(1, 3)))

  // Homework
  //
  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
  //
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] =
    map.groupBy(_._2).map { x => (x._2.keys.toSet, x._1) }.toList.sortBy(_._2)

  val input = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
  val expected = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)
  assert(sortConsideringEqualValues(input) == expected)

  val values = Set("a1", "a2", "b1", "c1", "c2", "d1").map { x => x -> x.head.toInt }.toMap
  assert(sortConsideringEqualValues(values) == List(
      Set("a1", "a2") -> 'a'.toInt,
      Set("b1") -> 'b'.toInt,
      Set("c1", "c2") -> 'c'.toInt,
      Set("d1") -> 'd'.toInt,
    )
  )
}