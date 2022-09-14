package traffic.model

import cats.{Eq, Show}
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.Decoder
import io.circe.generic.auto._, io.circe.syntax._

object Model {
  case class Intersection(avenue: String, street: String)

  object Intersection {
    implicit val intersectionEq: Eq[Intersection] = Eq.fromUniversalEquals
    implicit val intersectionShow: Show[Intersection] =
      Show.show(i => s"Avenue ${i.avenue} and Street ${i.street}")
  }

  case class RoadSegment(
      startingIntersection: Intersection,
      endingIntersection: Intersection
  )

  case class TrafficMeasurements(trafficMeasurements: List[TrafficMeasurement])

  object TrafficMeasurements {
    implicit val trafficMeasurementsDecoder: Decoder[TrafficMeasurements] =
      deriveDecoder[TrafficMeasurements]
  }

  case class TrafficMeasurement(
      measurementTime: BigDecimal,
      measurements: List[Measurement]
  )

  object TrafficMeasurement {
    implicit val trafficMeasurementDecoder: Decoder[TrafficMeasurement] =
      deriveDecoder[TrafficMeasurement]
  }

  case class Measurement(roadSegment: RoadSegment, transitTime: BigDecimal)

  object Measurement {
    implicit val measurementDecoder: Decoder[Measurement] =
      new Decoder[Measurement] {
        final def apply(c: HCursor): Decoder.Result[Measurement] =
          for {
            startAvenue <- c.downField("startAvenue").as[String]
            startStreet <- c.downField("startStreet").as[String]
            startIntersection = Intersection(startAvenue, startStreet)
            endAvenue <- c.downField("endAvenue").as[String]
            endStreet <- c.downField("endStreet").as[String]
            endIntersection = Intersection(endAvenue, endStreet)
            roadSegment = RoadSegment(startIntersection, endIntersection)
            transitTime <- c.downField("transitTime").as[BigDecimal]
          } yield {
            Measurement(roadSegment, transitTime)
          }
      }
  }

  case class Result(
      startingIntersection: Intersection,
      endingIntersection: Intersection,
      shortestPath: List[RoadSegment],
      totalTransitTime: BigDecimal
  )

  object Result {
    implicit val resultEncoder: Encoder[Result] = deriveEncoder[Result]
  }
}
