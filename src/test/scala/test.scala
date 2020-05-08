import java.nio.file.{Files, Path, Paths}
import java.util.Comparator

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class test extends AnyFlatSpec with Matchers {
  val root: Path = Paths.get("./tmp")
  Files.createDirectory(root)

  implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsoleRealPrinter[Id] = new ConsoleRealPrinter[Id]
  val program = new Program[Id, Path, Path]

  program.run(root, "test_dir", List("foo", "bar", "baz"))

  var dir: Path = root.resolve("test_dir")

  Files.exists(dir) shouldBe true

  var fDir: Path = dir.resolve("f")
  var bDir: Path = dir.resolve("b")

  Files.exists(fDir) shouldBe true
  Files.isDirectory(fDir) shouldBe true
  Files.exists(bDir) shouldBe true
  Files.isDirectory(bDir) shouldBe true

  val foo: Path = fDir.resolve("foo")
  val bar: Path = bDir.resolve("bar")
  val baz: Path = bDir.resolve("baz")

  Files.exists(foo) shouldBe true
  Files.isRegularFile(foo) shouldBe true
  Files.exists(bar) shouldBe true
  Files.isRegularFile(bar) shouldBe true
  Files.exists(baz) shouldBe true
  Files.isRegularFile(baz) shouldBe true

  Files.walk(root).sorted(Comparator.reverseOrder())
    .forEach(file => Files.deleteIfExists(file))
}