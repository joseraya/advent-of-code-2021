package aoc2021

import cats.effect.IO
import cats.effect.IOApp

import fs2.Pipe
import fs2.io.file.Path

object Day1 extends IOApp.Simple {

  def readFile(resource: String) = fs2.io.file.Files[IO]
    .readAll(Path(this.getClass.getResource(resource).getPath))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .filter(_.nonEmpty)
    .map(_.toInt)

  val input = readFile("/day_1/input.txt")

  def filterIncreases: Pipe[IO, Int, Int] = _
    .sliding(2)
    .filter(c => c(1) > c(0))
    .map(_(1))

  //1475
  def puzzle1: IO[Unit] = input
    .through(filterIncreases)
    .compile
    .count
    .flatMap(IO.println)

  //1516
  def puzzle2: IO[Unit] = input
    .sliding(3)
    .map(c => c.toList.sum)
    .through(filterIncreases)
    .compile
    .count
    .flatMap(IO.println)

  def run: IO[Unit] = puzzle1
}
