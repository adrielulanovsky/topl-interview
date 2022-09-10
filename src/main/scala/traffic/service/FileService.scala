package traffic.service

import cats.effect.{IO, Resource}

import scala.io.{BufferedSource, Source}

object FileService {
  def readFile(filename: String): IO[String] = {
    val acquire: IO[BufferedSource] = IO(Source.fromFile(filename))
    Resource.fromAutoCloseable(acquire)
      .use { source =>
        IO(source.getLines.mkString)
      }
  }

}
