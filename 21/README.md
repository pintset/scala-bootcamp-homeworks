## 21. Shared State in FP: Ref, Deferred, Semaphore, MVar

### Concurrent cache with expiration

Please implement a Cache which allows concurrent access.
checking expiration could be represented as some infinite process somewhere in background

Tip: you can use following structure to get current time suspended in effect : Clock[F].realTime(MILLISECONDS).flatMap(...)

Conditions:

- Cached items should have an expiration timestamp after which they are evicted.

- If we will put a value with the same key then it should renew expiration
