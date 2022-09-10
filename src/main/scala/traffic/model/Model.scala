package traffic.model

import io.circe._
import io.circe.generic.semiauto.deriveDecoder
import io.circe.Decoder

object Model {
  case class Intersection(avenue: String, street: String)
  case class RoadSegment(startingIntersection: Intersection, endingIntersection: Intersection)

  case class TrafficMeasurements(trafficMeasurements: List[TrafficMeasurement])
  implicit val trafficMeasurementsDecoder: Decoder[TrafficMeasurements] = deriveDecoder[TrafficMeasurements]

  case class TrafficMeasurement(measurementTime: BigDecimal, measurements: List[Measurement])
  implicit val trafficMeasurementDecoder: Decoder[TrafficMeasurement] = deriveDecoder[TrafficMeasurement]

  case class Measurement(roadSegment: RoadSegment, transitTime: BigDecimal)
  implicit val measurementDecoder: Decoder[Measurement] = new Decoder[Measurement] {
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
