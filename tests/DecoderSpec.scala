import Decoder.implicits.given
import FuelLevelChanged.FuelLevel
import VehicleEvent.*
import java.time.Instant

class DecoderSpec extends munit.FunSuite {

  val decoder = summon[Decoder[VehicleEvent]]

  test("Should decode all correct messages") {
    val correctMessages = List(
      "Started,EVENT-1,VEHICLE-1,2023-02-27T10:26:44.916865Z",
      "Stopped,EVENT-2,VEHICLE-1,2023-02-27T10:27:44.916865Z",
      "PositionChanged,EVENT-3,VEHICLE-1,2023-02-27T10:28:44.916865Z,0,0",
      "FuelLevelChanged,EVENT-4,VEHICLE-1,2023-02-27T10:28:44.916865Z,99.0",
    )
    val parsingResults = List(
      Started(Id("EVENT-1"), VehicleId("VEHICLE-1"),Instant.parse("2023-02-27T10:26:44.916865Z")),
      Stopped(Id("EVENT-2"), VehicleId("VEHICLE-1"),Instant.parse("2023-02-27T10:27:44.916865Z")),
      PositionChanged(Id("EVENT-3"), VehicleId("VEHICLE-1"),Instant.parse("2023-02-27T10:28:44.916865Z"),0,0),
      FuelLevelChanged(Id("EVENT-4"), VehicleId("VEHICLE-1"),Instant.parse("2023-02-27T10:28:44.916865Z"), FuelLevel(99.0)),
    )

    correctMessages.zip(parsingResults).foreach{ (input, expectedOutput) =>
      assertEquals(decoder.decode(input), Right(expectedOutput))
    }
  }

  test("Should fail to decode empty message") {
    assert(decoder.decode("").isLeft)
  }

  test("Should fail to decode correct message with malformed instant") {
    val correctMessageMalformedInstant = "FuelLevelChanged,EVENT-4,VEHICLE-1,20230227T102844,99.0"
    assert(decoder.decode(correctMessageMalformedInstant).isLeft)
  }

  test("Should fail to decode correct message with incorrect subtype") {
    val messageWithIncorrectSubtype = "VehicleCrashed,EVENT-4,VEHICLE-1,2023-02-27T10:28:44.916865Z,99.0"
    assert(decoder.decode(messageWithIncorrectSubtype).isLeft)
  }

  test("Should fail when unable to parse number") {
    val fuelChangedMalformed = "FuelLevelChanged,EVENT-4,VEHICLE-1,2023-02-27T10:28:44.916865Z,HALf-EMPTY"
    assert(decoder.decode(fuelChangedMalformed).isLeft)
  }

}