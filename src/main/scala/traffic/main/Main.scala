package traffic.main

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.circe._
import io.circe.parser._
import traffic.model.Model._
import traffic.service.FileService.readFile



object Main extends IOApp {
  val IntersectionPattern = "([a-zA-Z]+)(\\d+)".r

  def validateIntersection(s: String): ValidatedNec[String, Intersection] = s match {
    case IntersectionPattern(avenue, street) => Intersection(avenue, street).validNec
    case _ => "Intersection format not valid. Should be <avenue name><street name>. E.g.: A1, B4, G17".invalidNec
  }

  def processIntersectionInput(startingOrEnding: String): IO[Intersection] = {
    val receiveRawIntersection: IO[String] = for {
      _ <- IO.println(s"Please input the $startingOrEnding intersection: ")
      rawIntersection <- IO.readLine
    } yield rawIntersection

    receiveRawIntersection.flatMap { rawIntersection =>
      validateIntersection(rawIntersection) match {
        case Valid(intersection) => IO.pure(intersection)
        case Invalid(errors) =>
          errors.map(IO.println).combineAll.flatMap(_ => processIntersectionInput(startingOrEnding))
      }
    }
  }

  def processFileInput: IO[TrafficMeasurements] = {
    def processFile(filename: String): IO[TrafficMeasurements] = {
      readFile(filename)
        .flatMap { rawJson =>
          parse(rawJson) match {
            case Right(validJson) => validJson.as[TrafficMeasurements] match {
              case Right(trafficMeasurements) => IO.pure(trafficMeasurements)
              case Left(decodingFailure) =>
                IO.println(s"Error decoding the JSON file: ${decodingFailure.message}")
                  .flatMap(_ => processFileInput)
            }
            case Left(parsingFailure) =>
              IO.println(s"Error parsing the JSON file: ${parsingFailure.message}")
                .flatMap(_ => processFileInput)
          }
        }
    }

    for {
      _ <- IO.println("Please input the data file full location: ")
      filename <- IO.readLine
      trafficMeasurements <- processFile(filename)
    } yield trafficMeasurements
  }

  def calculateAvgMeasurements(measurements: TrafficMeasurements): List[Measurement] = {
    val joinedMeasurements = measurements.trafficMeasurements.flatMap{a => a.measurements}
    val measurementsMap =
      joinedMeasurements
        .groupMap(_.roadSegment)(_.transitTime)
        .view
        .mapValues(transitTimes => transitTimes.sumAll / transitTimes.length)
        .toMap
    measurementsMap.map(Measurement.apply _.tupled).toList
  }

  def dijkstra(measurements: List[Measurement], startingIntersection: Intersection, endingIntersection: Intersection) = ???

  def echoForever: IO[Nothing] = {
    val program = for {
      _ <- IO.println("Please input the data file location: ")
      filename <- IO.readLine
      trafficMeasurements <- processFileInput

      startingIntersection <- processIntersectionInput("starting")
      endingIntersection <- processIntersectionInput("ending")

      avgMeasurements = calculateAvgMeasurements(trafficMeasurements)
      //dijkstra(avgMeasurements)

      _ <- IO.println(s"Your starting intersection is Avenue ${startingIntersection.avenue} and Street ${startingIntersection.street}")
      _ <- IO.println(s"Your ending intersection is Avenue ${endingIntersection.avenue} and Street ${endingIntersection.street}")
    } yield 1
    program.foreverM
  }

  override def run(args: List[String]): IO[ExitCode] = {
    echoForever.as(ExitCode.Success)
  }
}
