## 18. Cats Effect: IO

Provide your own implementation of a subset of `IO` functionality.

Provide also tests for this functionality in EffectsHomework1Spec (which you should create).

Refer to the following docs about the meaning of each method as needed:
- https://typelevel.org/cats-effect/docs/2.x/datatypes/io
- https://typelevel.org/cats-effect/api/2.x/cats/effect/IO$.html
- https://typelevel.org/cats-effect/api/2.x/cats/effect/IO.html

There are two main ways how to implement IO:
- Executable encoding  - express every constructor and operator for our model in terms of its execution
- Declarative encoding - express every constructor and operator for our model as pure data in a recursive tree structure

While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this task using executable encoding, that is:
- Add a `private val run: () => A` parameter to the class `IO` private constructor
- Have most of the methods return a `new IO(...)`

Ask questions in the bootcamp chat if stuck on this task.