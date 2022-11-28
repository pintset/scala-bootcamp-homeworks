package server

final case class Game[A](answer: A, attemptCount: Int, attemptsLeft: Int)
