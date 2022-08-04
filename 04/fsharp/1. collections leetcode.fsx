#load "shouldEqualTo.fsx"
open ShouldEqualTo

// https://leetcode.com/problems/running-sum-of-1d-array/
//
// Given an array nums. We define a running sum of an array as runningSum[i] = sum(nums[0]…nums[i]).
//
// Example 1
// Input: nums = [1,2,3,4]
// Output: [1,3,6,10]
// Explanation: Running sum is obtained as follows: [1, 1+2, 1+2+3, 1+2+3+4].
//
// Example 2
// Input: nums = [1,1,1,1,1]
// Output: [1,2,3,4,5]
// Explanation: Running sum is obtained as follows: [1, 1+1, 1+1+1, 1+1+1+1, 1+1+1+1+1].
//
// Example 3
// Input: nums = [3,1,2,10,1]
// Output: [3,4,6,16,17]

let runningSum = function
    | [] -> []
    | h :: t -> t |> List.scan (+) h

[ 1; 2; 3; 4 ] |> runningSum |> shouldEqualTo [ 1; 3; 6; 10 ]
[ 3; 1; 2; 10; 1 ] |> runningSum |> shouldEqualTo [ 3; 4; 6; 16; 17 ]

// https://leetcode.com/problems/shuffle-the-array/
//
// Given the array nums consisting of 2n elements in the form [x1,x2,...,xn,y1,y2,...,yn].
// Return the array in the form [x1,y1,x2,y2,...,xn,yn].
//
// Example 1
// Input: nums = [2,5,1,3,4,7], n = 3
// Output: [2,3,5,4,1,7] 
// Explanation: Since x1=2, x2=5, x3=1, y1=3, y2=4, y3=7 then the answer is [2,3,5,4,1,7].
//
// Example 2
// Input: nums = [1,2,3,4,4,3,2,1], n = 4
// Output: [1,4,2,3,3,2,4,1]
//
// Example 3
// Input: nums = [1,1,2,2], n = 2
// Output: [1,2,1,2]

let shuffle = function
    | [] -> []
    | nums when List.length nums % 2 = 0 ->
        [ for (i1, i2) in nums |> List.splitAt (nums.Length / 2) ||> List.zip do yield! [ i1; i2 ] ]
        // or nums |> List.splitAt (nums.Length / 2) ||> List.zip |> List.map (fun (i1, i2) -> [ i1; i2 ]) |> List.collect id
        // or nums |> List.splitAt (nums.Length / 2) ||> List.fold2 (fun state i2 i1 -> i1 :: i2 :: state) [] |> List.rev
    | xs -> xs

[ 2; 5; 1; 3; 4; 7 ] |> shuffle |> shouldEqualTo [ 2; 3; 5; 4; 1; 7 ]
[ 1; 2; 3; 4; 4; 3; 2; 1 ] |> shuffle |> shouldEqualTo [ 1; 4; 2; 3; 3; 2; 4; 1 ]
[ 'a'; 'c'; 'b'; 'd' ] |> shuffle |> shouldEqualTo [ 'a'..'d' ]
[ 1; 1; 2; 2 ] |> shuffle |> shouldEqualTo [ 1; 2; 1; 2 ]
[ 1; 2 ] |> shuffle |> shouldEqualTo [ 1; 2 ]
[ 1 ] |> shuffle |> shouldEqualTo [ 1 ]
[] |> shuffle |> shouldEqualTo []
[ 1..5 ] |> shuffle |> shouldEqualTo [ 1..5 ]


// https://leetcode.com/problems/richest-customer-wealth/
//
// You are given an m x n integer grid accounts where accounts[i][j] is the amount of money the i​​​​​​​​​​​th​​​​ customer has in the j​​​​​​​​​​​th​​​​ bank. Return the wealth that the richest customer has.
// A customer's wealth is the amount of money they have in all their bank accounts. The richest customer is the customer that has the maximum wealth.
//
// Example 1
//
// Input: accounts = [[1,2,3],[3,2,1]]
// Output: 6
// Explanation:
// 1st customer has wealth = 1 + 2 + 3 = 6
// 2nd customer has wealth = 3 + 2 + 1 = 6
// Both customers are considered the richest with a wealth of 6 each, so return 6.
//
// Example 2
// Input: accounts = [[1,5],[7,3],[3,5]]
// Output: 10
// Explanation: 
// 1st customer has wealth = 6
// 2nd customer has wealth = 10 
// 3rd customer has wealth = 8
// The 2nd customer is the richest with a wealth of 10.
//
// Example 3
// Input: accounts = [[2,8,7],[7,1,3],[1,9,5]]
// Output: 17

let maximumWealth = function
    | [] -> 0
    | accounts -> accounts |> List.map List.sum |> List.max

[ [ 1; 2; 3 ]; [ 3; 2; 1 ] ] |> maximumWealth |> shouldEqualTo 6
[ [ 1; 5 ]; [ 7; 3 ]; [ 3; 5 ] ] |> maximumWealth |> shouldEqualTo 10
[ [ 2; 8; 7 ]; [ 7; 1; 3 ]; [ 1; 9; 5 ] ] |> maximumWealth |> shouldEqualTo 17
[ [ 1 ] ] |> maximumWealth |> shouldEqualTo 1
[ [ ] ] |> maximumWealth |> shouldEqualTo 0
[] |> maximumWealth |> shouldEqualTo 0

// https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
// Given the array candies and the integer extraCandies, where candies[i] represents the number of candies that the ith kid has.
// For each kid check if there is a way to distribute extraCandies among the kids such that he or she can have the greatest number of candies among them. Notice that multiple kids can have the greatest number of candies.
//
// Example 1
// Input: candies = [2,3,5,1,3], extraCandies = 3
// Output: [true,true,true,false,true]
// Explanation: 
// Kid 1 has 2 candies and if he or she receives all extra candies (3) will have 5 candies --- the greatest number of candies among the kids. 
// Kid 2 has 3 candies and if he or she receives at least 2 extra candies will have the greatest number of candies among the kids. 
// Kid 3 has 5 candies and this is already the greatest number of candies among the kids. 
// Kid 4 has 1 candy and even if he or she receives all extra candies will only have 4 candies. 
// Kid 5 has 3 candies and if he or she receives at least 2 extra candies will have the greatest number of candies among the kids. 
//
// Example 2
// Input: candies = [4,2,1,1,2], extraCandies = 1
// Output: [true,false,false,false,false]
// Explanation: There is only 1 extra candy, therefore only kid 1 will have the greatest number of candies among the kids regardless of who takes the extra candy.
//
// Example 3
// Input: candies = [12,1,12], extraCandies = 10
// Output: [true,false,true]

let kidsWithCandies extraCandies candies =
    candies |> List.map (fun c -> List.max candies <= c + extraCandies)

[ 2; 3; 5; 1; 3 ] |> kidsWithCandies 3 |> shouldEqualTo [ true; true; true; false; true ]
[ 4; 2; 1; 1; 2 ] |> kidsWithCandies 1 |> shouldEqualTo [ true; false; false; false; false ]
[ 12; 1; 12 ] |> kidsWithCandies 10 |> shouldEqualTo [ true; false; true ]
[ 12; 1; 12 ] |> kidsWithCandies 0 |> shouldEqualTo [ true; false; true ]
[] |> kidsWithCandies 5 |> shouldEqualTo []
[ 5 ] |> kidsWithCandies 0 |> shouldEqualTo [ true ]
[ 5 ] |> kidsWithCandies 5 |> shouldEqualTo [ true ]
[] |> kidsWithCandies 0 |> shouldEqualTo []

// https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points/
//
// Given n points on a 2D plane where points[i] = [xi, yi], Return the widest vertical area between two points such that no points are inside the area.
// A vertical area is an area of fixed-width extending infinitely along the y-axis (i.e., infinite height). The widest vertical area is the one with the maximum width.
// Note that points on the edge of a vertical area are not considered included in the area.
//
// Example 1
// Input: points = [[8,7],[9,9],[7,4],[9,7]]
// Output: 1
// Explanation: Both the red and the blue area are optimal.
//
// Example 2
// Input: points = [[3,1],[9,0],[1,0],[1,4],[5,3],[8,8]]
// Output: 3

let maxWidthOfVerticalArea = function
    | [] | [ [] ] -> 0
    | points ->
        points |> List.choose List.tryHead |> List.distinct |> List.sort |> List.windowed 2
               |> List.map (function [ x1; x2 ] -> x2 - x1 | _ -> 0) |> function [] -> 0 | xs -> List.max xs

[ [ 8; 7 ]; [ 9; 9 ]; [ 7; 4 ]; [ 9; 7 ] ] |> maxWidthOfVerticalArea |> shouldEqualTo 1
[ [ 3; 1 ]; [ 9; 0 ]; [ 1; 0 ]; [ 1; 4 ]; [ 5; 3 ]; [ 8; 8 ] ] |> maxWidthOfVerticalArea |> shouldEqualTo 3
[ [] ] |> maxWidthOfVerticalArea |> shouldEqualTo 0
[] |> maxWidthOfVerticalArea |> shouldEqualTo 0
[ [ 1 ] ] |> maxWidthOfVerticalArea |> shouldEqualTo 0