import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import cats.data.{IndexedStateT, StateT}
import cats.{Applicative, Functor, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.language.higherKinds

import scala.collection.JavaConverters._

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

trait GetFiles[F[_], Dir, File] {
  def getFiles(dir: Dir): F[List[File]]
}

trait GetFileName[F[_], File]{
  def getFileName(file: File): F[String]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(file: File, dest: Dir): F[File]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               printer: Printer[F, File],
                               getFiles: GetFiles[F, Dir, File],
                               getFileName: GetFileName[F, File],
                               moveFile: MoveFile[F, Dir, File]) {
  def run(dir: Dir, dirName: String, fileNames: List[String]): F[Unit] = for {
    newDir <- mkDir.mkDir(dir, dirName)
    _ <- fileNames.traverse(name => mkFile.mkFile(newDir, name))
    files <- getFiles.getFiles(newDir)
    _ <- files.traverse(file => printer.printName(file))
    names <- files.traverse(file => getFileName.getFileName(file))
    letters = names.map(_.head)
    dirs <- letters.traverse(letter => mkDir.mkDir(newDir, letter.toString))
    _ <- files.zip(dirs).traverse(p => moveFile.moveFile(p._1, p._2))
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path]
  with GetFiles [F, Path, Path] with GetFileName[F, Path] with MoveFile [F, Path, Path] {

  override def moveFile(file: Path, dest: Path): F[Path] = {
    Files.move(file, dest.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
  }

  override def getFileName(file: Path): F[String] = {
    file.getFileName.toString.pure[F]
  }

  override def getFiles(dir: Path): F[List[Path]] = {
    Files.list(dir).filter(Files.isRegularFile(_)).iterator().asScala.toList.pure[F]
  }

  override def mkDir(dir: Path, name: String): F[Path] = {
    if (Files.exists(dir.resolve(name))) {
      dir.resolve(name).pure[F]
    } else {
      Files.createDirectories(dir.resolve(name)).pure[F]
    }
  }

  override def mkFile(dir: Path, name: String): F[Path] = {
    if (Files.exists(dir.resolve(name))) {
      dir.resolve(name).pure[F]
    } else {
      Files.createFile(dir.resolve(name)).pure[F]
    }
  }
}

  class ConsoleRealPrinter[F[_] : Applicative] extends Printer[F, Path] {
    override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
  }

object TypeClasses {
  def main(args: Array[String]): Unit = {

    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsoleRealPrinter[Id] = new ConsoleRealPrinter[Id]
    val program = new Program[Id, Path, Path]

    program.run(Paths.get("."), "test_dir", List("foo", "bar", "baz"))
  }
}



