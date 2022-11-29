## 24. WebSocket with http4s

### Guessing Game

Write a server and a client that play a number guessing game together. The same as [Homework 23](https://github.com/pintset/scala-bootcamp-homeworks/tree/main/23) but communication has to be done using WebSocket

Communication flow should be as follows:

1. The client asks the server to start a new game by providing the minimum and the maximum number that can be guessed.

2. The server comes up with some random number within the provided range.

3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to the client, whether the current number is lower, greater or equal to the guessed one.

4. The game ends when the number is guessed or there are no more attempts left. At this point the client should terminate, while the server may continue running forever.

5. The server should support playing many separate games (with different clients) at the same time.

The exact protocol and message format to use is not specified and should be designed while working on the task.

### Implementation details

1. Server supports `http` and `ws` clients simultaneously. In [ClientApp.scala](scala/src/main/scala/ClientApp.scala) choose (comment/uncomment) one of the following lines:
```scala
// services.Http.resource[F](uri"https://localhost:9001")
services.Ws.resource[F](uri"ws://localhost:9001")
   .map { GameClient[F] _ andThen gameBuilder }
   .use { game => settingsService.getSettings >>= game }
   .void
```

2. The contract is unified for both protocols. In particular, start request like
```
POST /start HTTP/1.1
Content-Type: application/json

{
    "min": 0,
    "max": 100,
    "attemptCount": 5
}
```
will return game GUID (game instance handle) in a response, e.g.
```json
"e4fa6631-97ad-4d63-8651-d608ae92b9e0"
```
Then you can use the id above to play the game, e.g. send guess request:
```
POST /guess HTTP/1.1
Content-Type: application/json

{
    "gameId": "e4fa6631-97ad-4d63-8651-d608ae92b9e0",
    "guess": 50
}
```
as a result of which the server will identify whether the provided guess is lower or higher than the number guessed by the server, e.g.
```json
{
    "attemptsLeft": 4,
    "attemptResult": "Lower"
}
```
In case game was not found, you will have the following body as part of 404 response:
```json
{
    "error": "There is no game with id: e4fa6631-97ad-4d63-8651-d608ae92b9e1"
}
```

Everything described above is the same for WebSocket part, except you do not need to provide any notion of action you doing (start, guess). Just send JSONs and the server will identify what you want basing on their contents

3. The provided Scala client [ClientApp.scala](scala/src/main/scala/ClientApp.scala) supports lazy game start, i.e. the server will not start the game (think of a number) until the client (player) provide its guess first.
4. Moreover, in case of WebSockets the client will not establish connection with the server until the first guess is made by a player.
5. [ClientApp.scala](scala/src/main/scala/ClientApp.scala) supports automated playing. Use bot game builder for that:
```scala
// val gameBuilder = consoleGame[F] _
val gameBuilder = botGame[F] _

// services.Http.resource[F](uri"https://localhost:9001")
services.Ws.resource[F](uri"ws://localhost:9001")
  .map { GameClient[F] _ andThen gameBuilder }
  .use { game => settingsService.getSettings >>= game }
  .void

```
6. This version is updated to use Ember and Cats 3. There is a http version of the game written with [Blaze and Cats 2](https://github.com/pintset/scala-bootcamp-homeworks/tree/main/23)
7. I wanted to create the second game inside this project - Wordle, but generalization of the current implementation into generic-guess game makes the code complex to understand. It looks like some other code design approach is required in the first place. However, you can find generalized against guess (where a guess is`String`, not `Int`) working version of Wordle here: https://github.com/pintset/scala-bootcamp-homeworks/tree/HW-24-Wordle/24

