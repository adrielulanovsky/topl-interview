package traffic.service

import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

class FileServiceSpec extends Specification with CatsEffect {
  val service: FileService[IO] = FileService.impl
  "readFile" should {
    "Read an existing file with content" in {
      service.readFile("src/test/scala/traffic/service/file1.txt").map(_ === "content of file 1")
    }
    "Read an existing file with no content" in {
      service.readFile("src/test/scala/traffic/service/file2.txt").map(_ === "")
    }
  }
  "writeFile" should {
    "write a file in the specified path" in {
      val content = "content of file 3"
      val path = "src/test/scala/traffic/service/file3.txt"
      service.writeToFile(content, path)
        .flatMap(_ => service.readFile(path)
          .map(_ === content))
    }
  }
}
