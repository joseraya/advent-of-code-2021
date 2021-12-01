package aoc2021

import cats.effect.IO
import cats.effect.IOApp

import fs2.Pipe
import fs2.io.file.Path

/*
The first order of business is to figure out how quickly the depth increases, just so you know what you're dealing with - you never know if the keys will get carried into deeper water by an ocean current or a fish or something.

To do this, count the number of times a depth measurement increases from the previous measurement. (There is no measurement before the first measurement.) In the example above, the changes are as follows:


 */
// count the number of times a depth measurement increases
object Day1 extends IOApp.Simple {

  def readFile(resource: String) = fs2.io.file.Files[IO]
    .readAll(Path(this.getClass.getResource(resource).getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .filter(_.nonEmpty)
    .map(_.toInt)

  def countIncreases: Pipe[IO, Int, Int] = _
    .sliding(2)
    .map(c => c(1) > c(0))
    .fold(0){case (acc, v) => if (v) acc +1 else acc}

  def puzzle1: IO[Unit] = readFile("/day_1/input.txt")
    .through(countIncreases)
    .evalTap(c => IO(println(c)))
    .compile
    .drain

  def puzzle2: IO[Unit] = readFile("/day_1/input.txt")
    .sliding(3)
    .map(c => c.foldLeft(0){ case (acc, v) => acc + v})
    .through(countIncreases)
    .evalTap(c => IO(println(c)))
    .compile
    .drain

  def run: IO[Unit] = puzzle2
}
