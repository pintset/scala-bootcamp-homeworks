## 19. Cats Effect: Resource, ContextShift, Fiber

### MinHash

1. Read from the console the file path.
   1.1 Use Blocking Thread Pool
   1.2 Check the transmitted data(Error Handling + Validation).
2. Read from the console the seed.
   2.1 Use Blocking Thread Pool
   2.2 Check the transmitted data(Error Handling + Validation).
3. Read the data from the file.
4. Calculate the signature (in parallel if possible).
   4.1 Use Separate Thread Pool(ContextShift)
   4.2 Split text into words
   4.3 Calculate hash for each word
   4.4 Take the minimal hash
   4.5 \*Repeat the process for n different hash functions.
5. Save the signature in memory(think about storage).
6. Terminate the application.

```scala
def javaHash(word: String, seed: Int = 0): Int = {
  var hash = 0

  for (ch <- word.toCharArray)
    hash = 31 * hash + ch.toInt

  hash = hash ^ (hash >> 20) ^ (hash >> 12)
  hash ^ (hash >> 7) ^ (hash >> 4)
}
```

```Scala
def knuthHash(word: String, constant: Int): Int = {
  var hash = 0
  for (ch <- word.toCharArray)
    hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
  hash % constant
}
```

https://habr.com/ru/post/250673/
