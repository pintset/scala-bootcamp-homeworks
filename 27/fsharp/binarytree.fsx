type BinaryTree<'T when 'T : comparison> =
    | Node of left: BinaryTree<'T> * leaf: 'T * right: BinaryTree<'T>
    | Empty

module BinaryTree =
    let rec insert element tree = 
        match tree with
        | Empty -> Node (Empty, element, Empty)
        | Node (left, leaf, right) when element < leaf -> Node (insert element left, leaf, right)
        | Node (left, leaf, right) -> Node (left, leaf, insert element right)
        | _ -> tree

    let rec merge tree1 tree2 =
        match tree1, tree2 with
        | Empty, Empty -> Empty
        | Node (left, leaf, right), tree2 -> Node (left, leaf, merge right tree2)
        | tree1, Node (left, leaf, right) -> Node (merge tree1 left, leaf, right)

    let rec remove element tree =
        match tree with
        | Empty -> Empty
        | Node (left, leaf, right) when element = leaf -> merge left right
        | Node (left, leaf, right) when element < leaf -> Node (remove element left, leaf, right)
        | Node (left, leaf, right)                     -> Node (left, leaf, remove element right)

    let rec contains element tree =
        match tree with
        | Empty -> false
        | Node (_, leaf, _)    when element = leaf -> true
        | Node (left, leaf, _) when element < leaf -> contains element left
        | Node (_, _, right)                       -> contains element right

type PosBinaryTree<'T when 'T : comparison> =
    | N of PosBinaryTree<'T> * 'T * x: int * y: int * PosBinaryTree<'T>
    | E

module PosBinaryTree =
    let ofSortedTree tree =
        let rec layout t x depth =
            match t with
                | Empty -> (x, E)
                | Node (left, key, right) ->
                    let x', lTree = layout left x (depth + 1)
                    let x'', rTree = layout right (x' + 1) (depth + 1)
                    (x'', N(lTree, key, x', depth, rTree))
        let _, finalTree = layout tree 1 1
        finalTree

let tree = Empty
let newTree =
    tree
    |> BinaryTree.insert 7
    |> BinaryTree.insert 3
    |> BinaryTree.insert 2
    |> BinaryTree.insert 5
    |> BinaryTree.insert 9
    |> BinaryTree.insert 8
    |> BinaryTree.insert 10
    |> BinaryTree.insert 6
    |> BinaryTree.insert 1

let print tree =
    let width = 10
    let height = 10

    let laidOutTree = PosBinaryTree.ofSortedTree tree

    let canvas = Array2D.init width height (fun _ _ -> " ")

    let rec draw (tree: PosBinaryTree<'a>) =
       match tree with
           | E -> ()
           | N (l, v, x, y, r) ->
               canvas.[y,x] <- string v
               draw l
               draw r

    draw laidOutTree

    for y in 0..(width - 1) do
       for x in 0..(height - 1) do
           stdout.Write(canvas.[y,x])
       stdout.WriteLine()

print newTree
newTree |> BinaryTree.remove 5 |> BinaryTree.remove 3 |> BinaryTree.remove 10 |> BinaryTree.remove 9 |> BinaryTree.remove 2 |> print

