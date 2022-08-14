## 20. Unit Testing 2: Mocks, Compiler Testing, Parametric Reasoning

Optional Homework. There is json parser implementation: https://github.com/evolution-gaming/scala-bootcamp/blob/master/src/main/scala/com/evolutiongaming/bootcamp/testing/Json.scala 

Create property test which will test the following property: parse(print(json)) == json: https://github.com/evolution-gaming/scala-bootcamp/blob/master/src/test/scala/com/evolutiongaming/bootcamp/testing/JsonSpec.scala

In order to do that define your own generator Gen[Json]. Also it may help do define Shrink[Json], which will try to shrink generated json objects for failing tests.