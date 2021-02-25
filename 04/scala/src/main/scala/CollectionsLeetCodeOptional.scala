object CollectionsLeetCodeOptional extends App {
  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  //
  // A string is a valid parentheses string (denoted VPS) if it meets one of the following:
  //   * It is an empty string "", or a single character not equal to "(" or ")",
  //   * It can be written as AB (A concatenated with B), where A and B are VPS's, or
  //   * It can be written as (A), where A is a VPS.
  // We can similarly define the nesting depth depth(S) of any VPS S as follows:
  //   * depth("") = 0
  //   * depth(C) = 0, where C is a string with a single character not equal to "(" or ")".
  //   * depth(A + B) = max(depth(A), depth(B)), where A and B are VPS's.
  //   * depth("(" + A + ")") = 1 + depth(A), where A is a VPS.
  // For example, "", "()()", and "()(()())" are VPS's (with nesting depths 0, 1, and 2), and ")(" and "(()" are not VPS's.
  // Given a VPS represented as string s, return the nesting depth of s.
  //
  // Example 1
  // Input: s = "(1+(2*3)+((8)/4))+1"
  // Output: 3
  // Explanation: Digit 8 is inside of 3 nested parentheses in the string.
  //
  // Example 2
  // Input: s = "(1)+((2))+(((3)))"
  // Output: 3
  //
  // Example 3
  // Input: s = "1+(2*3)/(2-1)"
  // Output: 1
  //
  // Example 4
  // Input: s = "1"
  // Output: 0

  def maxDepth(s: String): Int =
    s.foldLeft(0, 0) { (state, c) =>
      val (max, cur) = state
      c match {
        case '(' => (Math.max(max, cur + 1), cur + 1)
        case ')' => (max, cur - 1)
        case _ => (max, cur)
      }
    } match {
      case (x, 0) => x
      case _ => 0
    }

  assert(maxDepth("(1+(2*3)+((8)/4))+1") == 3)
  assert(maxDepth("(1)+((2))+(((3)))") == 3)
  assert(maxDepth("1+(2*3)/(2-1)") == 1)
  assert(maxDepth("1") == 0)

  // https://leetcode.com/problems/split-a-string-in-balanced-strings/
  //
  // Balanced strings are those that have an equal quantity of 'L' and 'R' characters.
  // Given a balanced string s, split it in the maximum amount of balanced strings.
  // Return the maximum amount of split balanced strings.
  //
  // Example 1
  // Input: s = "RLRRLLRLRL"
  // Output: 4
  // Explanation: s can be split into "RL", "RRLL", "RL", "RL", each substring contains same number of 'L' and 'R'.
  //
  // Example 2
  // Input: s = "RLLLLRRRLR"
  // Output: 3
  // Explanation: s can be split into "RL", "LLLRRR", "LR", each substring contains same number of 'L' and 'R'.
  //
  // Example 3
  // Input: s = "LLLLRRRR"
  // Output: 1
  // Explanation: s can be split into "LLLLRRRR".
  //
  // Example 4
  // Input: s = "RLRRRLLRLL"
  // Output: 2
  // Explanation: s can be split into "RL", "RRRLLRLL", since each substring contains an equal number of 'L' and 'R'

  def balancedStringSplit(s: String): Int = {
    def toState(count: Int, cur: Int): (Int, Int) =
      if (cur == 0) (count + 1, cur) else (count, cur)

      s.foldLeft(0, 0) { case ((count, cur), c) =>
        c match {
          case 'L' => toState(count, cur - 1)
          case 'R' => toState(count, cur + 1)
          case _ => (count, cur)
        }
      }._1
    }

  assert(balancedStringSplit("RLRRLLRLRL") == 4)
  assert(balancedStringSplit("RLLLLRRRLR") == 3)
  assert(balancedStringSplit("LLLLRRRR") == 1)
  assert(balancedStringSplit("RLRRRLLRLL") == 2)

  // https://leetcode.com/problems/matrix-block-sum/
  //
  // Given a m * n matrix mat and an integer K, return a matrix answer where each answer[i][j] is the sum of all elements mat[r][c] for i - K <= r <= i + K, j - K <= c <= j + K, and (r, c) is a valid position in the matrix.
  //
  // Example 1
  // Input: mat = [[1,2,3],[4,5,6],[7,8,9]], K = 1
  // Output: [[12,21,16],[27,45,33],[24,39,28]]
  //
  // Example 2
  // Input: mat = [[1,2,3],[4,5,6],[7,8,9]], K = 2
  // Output: [[45,45,45],[45,45,45],[45,45,45]]

  def matrixBlockSum(mat: Array[Array[Int]], k: Int): Array[Array[Int]] = {
    val rowCount = mat.length
    val columnCount = mat(0).length
    val indices: (Int, Int) => IndexedSeq[(Int, Int)] = (i, j) => for {
        x <- (i - k) to (i + k)
        y <- (j - k) to (j + k)
        if x >= 0 && y >= 0 && x < rowCount && y < rowCount
    } yield (x, y)

    Array.tabulate(rowCount, columnCount)((r, c) => indices(r, c).map(x => mat(x._1)(x._2)).sum)
  }

  def assert2DArrays(mat1: Array[Array[Int]], mat2: Array[Array[Int]]): Unit =
    assert(mat1 zip mat2 forall(x => x._1 sameElements x._2))

  val mat = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
  assert2DArrays(matrixBlockSum(mat, 1), Array(Array(12, 21, 16), Array(27, 45, 33), Array(24, 39, 28)))
  assert2DArrays(matrixBlockSum(mat, 2), Array.fill(3)(Array.fill(3)(45)))
}