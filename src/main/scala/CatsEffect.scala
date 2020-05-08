import cats.CommutativeApply.ops.toAllCommutativeApplyOps
import cats.syntax.all._
import cats.instances.list._
import cats.effect._
import cats.effect.concurrent.{MVar, Ref, Semaphore}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext


object CatsEffect extends IOApp {

  def runPrinter(mVar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      value <- mVar.take
      _ <- IO(println(value))
      _ <- rec
    } yield ()

    Resource.make(rec.start)(_.cancel.flatMap(_ => IO(println("Printer closed")))).void
  }

  def runCounter(mVar: MVar[IO, String]): Resource[IO, Unit] = {
    def rec(counter: Int): IO[Unit] = for {
      _ <- mVar.put((counter).toString)
      _ <- IO.sleep(1.seconds)
      _ <- rec(counter + 1)
    } yield ()

    Resource.make(rec(0).start)(_.cancel.flatMap(_ => IO(println("Counter closed")))).void
  }

  val runProgram: Resource[IO, Unit] = for {
    mVar <- Resource.make(MVar.empty[IO, String])(_ => IO(print("")))
    _ <- runPrinter(mVar)
    _ <- runCounter(mVar)
  } yield()

  override def run(args: List[String]): IO[ExitCode] = runProgram.use(_ => IO.never)
}
