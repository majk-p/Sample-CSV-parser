import Decoder.implicits.given

@main
def main() = 
  val decoder = summon[Decoder[VehicleEvent]]
  val correctMessages = List(
    "Started,EVENT-1,VEHICLE-1,2023-02-27T10:26:44.916865Z",
    "Stopped,EVENT-2,VEHICLE-1,2023-02-27T10:27:44.916865Z",
    "PositionChanged,EVENT-3,VEHICLE-1,2023-02-27T10:28:44.916865Z,0,0",
    "FuelLevelChanged,EVENT-4,VEHICLE-1,2023-02-27T10:28:44.916865Z,99.0",
  )
  println("Parsing results for correct events:")
  correctMessages.foreach(decoder.decode andThen println)
  