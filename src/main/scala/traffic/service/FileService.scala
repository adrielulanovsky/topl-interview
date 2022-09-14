package traffic.service

import cats.effect.{Resource, Sync}
import cats.implicits.catsSyntaxApply

import java.io.{File, FileWriter}
import scala.io.{BufferedSource, Source}

trait FileService[F[_]] {
  def readFile(filename: String): F[String]
  def writeToFile(content: String, filename: String): F[Unit]
}

object FileService {
  def impl[F[_]: Sync]: FileService[F] = new FileService[F] {
    override def readFile(filename: String): F[String] = {
      val acquire: F[BufferedSource] = Sync[F].delay(Source.fromFile(filename))
      Resource.fromAutoCloseable(acquire)
        .use { source =>
          Sync[F].delay(source.getLines.mkString)
        }
    }

    override def writeToFile(content: String, filename: String): F[Unit] = {
        val acquire: F[FileWriter] = Sync[F].blocking(new FileWriter(new File(filename)))
        Resource.fromAutoCloseable(acquire).use { writer =>
          Sync[F].blocking(writer.write(content)) *> Sync[F].blocking(writer.flush())
        }
      }
  }
}
