## 10. HKT & Type Classes

### JSON Encoder/Decoder
Implentation of JSON ADT, Encoder and Decoder with implicits. Functor & Contravariant

### MutableBoundedCache

Lo and behold! Brand new super-useful collection library for Scala!

Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]], a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size of the data stored.

As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use a thing called size score. Its calculation rules:

- size score of a Byte is 1

- Int - 4 (as primitive JVM int consists of 4 bytes)

- Long - 8

- Char - 2 (one UTF-16 symbol is 2 bytes)

- String - 12 (supposedly the size of the JVM object header) + length * size score of Char

- score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all the fields

- score for any sequence (Array[T], List[T], Vector[T]) is 12 (our old friend object header) + sum of scores of all elements

- score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values