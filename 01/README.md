## 01. Least Common Multiple & Greatest Common Division

Implement functions that calculate https://en.wikipedia.org/wiki/Least_common_multiple and https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

```scala
  def lcm(a: Int, b: Int): Int = ???
  def gcd(a: Int, b: Int): Int = ???
```

Create a new Git public repository for your homework solutions, use `basics` package for this homework.

You can use `sbt new scala/hello-world.g8` to start a new bare-bones Scala SBT project.

### Implementation

Scala version of implementation uses monadic approach to parse and supply parameters for gcm/lcd calculation. F# version uses applicative approach.

In addition, F# version *gcdlcmsafe.fsx* operates with custom `NaturalNumber` type, which represents natural numbers of `int`.
