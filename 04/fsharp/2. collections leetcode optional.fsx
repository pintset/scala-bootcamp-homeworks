#load "shouldEqualTo.fsx"
open ShouldEqualTo

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

let maxDepth s =
    let folder (max, cur) = function
        | '(' -> System.Math.Max(max, cur + 1), cur + 1
        | ')' -> max, cur - 1
        | _ -> max, cur

    s |> Seq.fold folder (0, 0) |> function (x, 0) -> x | _ -> 0

"(1+(2*3)+((8)/4))+1" |> maxDepth |> shouldEqualTo 3
"(1)+((2))+(((3)))" |> maxDepth |> shouldEqualTo 3
"1+(2*3)/(2-1)" |> maxDepth |> shouldEqualTo 1
"1" |> maxDepth |> shouldEqualTo 0
"" |> maxDepth |> shouldEqualTo 0
"()()" |> maxDepth |> shouldEqualTo 1
"()(()())" |> maxDepth |> shouldEqualTo 2
")(" |> maxDepth |> shouldEqualTo 0
"(()" |> maxDepth |> shouldEqualTo 0

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

let balancedStringSplit s =
    let toState count cur =
        if cur = 0 then count + 1, cur else count, cur
    let folder (count, cur) = function
        | 'L' -> cur - 1 |> toState count
        | 'R' -> cur + 1 |> toState count
        | _ -> count, cur
    
    s |> Seq.fold folder (0, 0) |> fst

"RLRRLLRLRL" |> balancedStringSplit |> shouldEqualTo 4
"RLLLLRRRLR" |> balancedStringSplit |> shouldEqualTo 3
"LLLLRRRR" |> balancedStringSplit |> shouldEqualTo 1
"RLRRRLLRLL" |> balancedStringSplit |> shouldEqualTo 2

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

let matrixBlockSum k (mat: int [,]) =
    let rowCount, columnCount = Array2D.length1 mat, Array2D.length2 mat
    let indices i j =
        [ for x in i - k .. i + k do
            for y in j - k .. j + k do
                if x >= 0 && y >= 0 && x < rowCount && y < columnCount then yield (x, y) ]

    Array2D.init rowCount columnCount (fun r c -> indices r c |> List.sumBy (fun (i, j) -> mat.[i, j]))

array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] |> matrixBlockSum 1 |> shouldEqualTo (array2D [
    [12; 21; 16]
    [27; 45; 33]
    [24; 39; 28]
])

array2D [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] |> matrixBlockSum 2 |> shouldEqualTo (Array2D.create 3 3 45)