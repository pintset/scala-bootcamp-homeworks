object CollectionsExercises extends App {
  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] =
    list.foldLeft(List(zero)) { (l, x) => f(l.head, x) :: l }.reverse

  val numbers = (1 to 100).toList
  assert(scanLeft(0)(numbers)(_ + _) == numbers.scanLeft(0)(_ + _))

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] =
    s.foldLeft[List[(Char, Int)]](Nil) { (state, x) => state match {
      case Nil => List((x, 1))
      case (h, c) :: t if h == x => (h, c + 1) :: t
      case xs => (x, 1) :: xs
    }}.reverse

  assert(count("aaaabbbcca") == List(('a', 4), ('b', 3), ('c', 2), ('a', 1)))
}