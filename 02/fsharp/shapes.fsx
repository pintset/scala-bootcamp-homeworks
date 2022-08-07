type ILocated =
    abstract member x: double
    abstract member y: double

type IBounded =
    abstract member minX: double
    abstract member maxX: double
    abstract member minY: double
    abstract member maxY: double

type IArea =
    abstract member area: double

type Shape =
    inherit ILocated
    inherit IBounded
    inherit IArea

let point x y =
    { new Shape with
        member _.x: double = x
        member _.y: double = y

        member _.minX: double = x
        member _.maxX: double = x

        member _.minY: double = y
        member _.maxY: double = y

        member _.area: double = 1 }

// Still shape
type Point(x, y) =
    interface Shape with
        member _.x: double = x
        member _.y: double = y

        member _.minX: double = x
        member _.maxX: double = x

        member _.minY: double = y
        member _.maxY: double = y

        member _.area: double = 1 

let point1 = point 1 1
let point2 = point 1 1

// false :(
point1 = point2
