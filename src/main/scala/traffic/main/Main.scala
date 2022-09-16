package traffic.main

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.effect.{ExitCode, IO, IOApp}
import cats._
import cats.implicits._
import io.circe._
import io.circe.parser._
import io.circe.syntax.EncoderOps
import traffic.model.Model._
import traffic.service.DijkstraService.{Cost, Edge, Graph}
import traffic.service.{DijkstraService, FileService}

import java.nio.file.{Files, Paths}

object Main extends IOApp {
  val IntersectionPattern = "([A-Z]+)(\\d+)".r

  def validateIntersectionFormat(
      s: String
  ): ValidatedNec[String, Intersection] =
    s match {
      case IntersectionPattern(avenue, street) =>
        Intersection(avenue, street).validNec
      case _ =>
        "Intersection format not valid. Should be {avenue name}{street name}. E.g.: A1, B4, G17".invalidNec
    }

  def processIntersectionInput(
      startingOrEnding: String,
      intersections: Set[Intersection]
  ): IO[Intersection] = {
    val receiveRawIntersection: IO[String] = for {
      _ <- IO.println(s"Please input the $startingOrEnding intersection: ")
      rawIntersection <- IO.readLine
    } yield rawIntersection

    receiveRawIntersection.flatMap { rawIntersection =>
      val validatedIntersection =
        validateIntersectionFormat(rawIntersection)
          .andThen(i =>
            if (intersections.contains(i)) i.validNec
            else
              s"The provided intersection (${i.show}) doesn't exist in the dataset.".invalidNec
          )

      validatedIntersection match {
        case Valid(intersection) =>
          IO.println(
            s"Your ${startingOrEnding} intersection is ${intersection.show}"
          ).as(intersection)
        case Invalid(errors) =>
          errors
            .traverse_(IO.println) >> processIntersectionInput(
            startingOrEnding,
            intersections
          )
      }
    }
  }

  val pathInput: IO[String] = for {
    _ <- IO.println("Please input the data file path: ")
    path <- IO.readLine
  } yield path

  def validatePath(path: String): IO[String] =
    if (Files.exists(Paths.get(path))) IO.pure(path)
    else {
      for {
        _ <- IO.println(
          "The provided path doesn't exist. Please provide a valid path"
        )
        rawPath <- pathInput
        path <- validatePath(rawPath)
      } yield path
    }

  def processFile(
      path: String,
      fileService: FileService[IO]
  ): IO[TrafficMeasurements] = {
    fileService
      .readFile(path)
      .flatMap { rawJson =>
        parse(rawJson) match {
          case Right(validJson) =>
            validJson.as[TrafficMeasurements] match {
              case Right(trafficMeasurements) => IO.pure(trafficMeasurements)
              case Left(decodingFailure) =>
                IO.println(
                  s"Error decoding the JSON file: ${decodingFailure.message}"
                ) >> processFileInput(fileService)
            }
          case Left(parsingFailure) =>
            IO.println(
              s"Error parsing the JSON file: ${parsingFailure.message}"
            ) >> processFileInput(fileService)
        }
      }
  }

  def processFileInput(
      fileService: FileService[IO]
  ): IO[TrafficMeasurements] = {
    for {
      rawPath <- pathInput
      validPath <- validatePath(rawPath)
      trafficMeasurements <- processFile(validPath, fileService)
    } yield trafficMeasurements
  }

  def mean(transitTimes: List[BigDecimal]): BigDecimal =
    transitTimes.sumAll / transitTimes.length

  def calculateAvgMeasurements(
      measurements: TrafficMeasurements,
      avgFunction: List[BigDecimal] => BigDecimal
  ): List[Measurement] = {
    val joinedMeasurements =
      measurements.trafficMeasurements.flatMap(_.measurements)
    val measurementsMap =
      joinedMeasurements
        .groupMap(_.roadSegment)(_.transitTime)
        .view
        .mapValues(avgFunction)
        .toMap
    measurementsMap.map((Measurement.apply _).tupled).toList
  }

  def buildEdges(
      measurements: List[Measurement]
  ): Set[Edge[Intersection]] = {
    measurements
      .map { m =>
        (
          m.roadSegment.startingIntersection,
          m.roadSegment.endingIntersection,
          m.transitTime
        )
      }
      .toSet
  }

  def formatResultMessage(
      start: Intersection,
      end: Intersection,
      maybeResult: Option[(List[Intersection], Cost)]
  ): String = {
    val json = maybeResult
      .map { case (path, cost) =>
        val roadSegments: List[RoadSegment] =
          path.zip(path.drop(1)).map((RoadSegment.apply _).tupled)

        Result(start, end, roadSegments, cost)
      }
      .map(_.asJson)

    json.fold(
      "\"" + s"It is impossible to reach intersection ${end.show} from ${start.show}" + "\""
    )(_.show)
  }

  def loop: IO[Unit] = {
    val dijkstraService = DijkstraService.impl[Intersection]
    val fileService = FileService.impl[IO]

    for {
      trafficMeasurements <- processFileInput(fileService)
      //I chose to use the arithmetic mean, but parametrized the function to allow for different alternatives
      avgMeasurements = calculateAvgMeasurements(trafficMeasurements, mean)

      edges = buildEdges(avgMeasurements)
      graph = dijkstraService.buildGraph(edges)
      intersections = dijkstraService.getNodes(graph)

      startingIntersection <- processIntersectionInput(
        "starting",
        intersections
      )
      endingIntersection <- processIntersectionInput("ending", intersections)

      maybeResult = dijkstraService.pathWithCost(
        graph,
        startingIntersection,
        endingIntersection
      )

      resultMessage = formatResultMessage(
        startingIntersection,
        endingIntersection,
        maybeResult
      )

      resultFilePath = "result.json"
      _ <- fileService.writeToFile(resultMessage, resultFilePath)
      _ <- IO.println(s"Result written to file ${resultFilePath}")
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {
    loop.as(ExitCode.Success)
  }
}
