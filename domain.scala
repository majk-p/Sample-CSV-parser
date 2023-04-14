import java.time.Instant
import VehicleEvent.*
import FuelLevelChanged.*

enum VehicleEvent(id: Id, vehicleId: VehicleId, timestamp: Instant) {
  case Started(id: Id, vehicleId: VehicleId, timestamp: Instant) extends VehicleEvent(id, vehicleId, timestamp)
  case Stopped(id: Id, vehicleId: VehicleId, timestamp: Instant) extends VehicleEvent(id, vehicleId, timestamp)
  case PositionChanged(id: Id, vehicleId: VehicleId, timestamp: Instant, idx: Int, y: Int) extends VehicleEvent(id, vehicleId, timestamp)
  case FuelLevelChanged(id: Id, vehicleId: VehicleId, timestamp: Instant, fuelLevel: FuelLevel) extends VehicleEvent(id, vehicleId, timestamp)
}

object VehicleEvent {
  case class Id(value: String)
  case class VehicleId(value: String)
}

object FuelLevelChanged {
  case class FuelLevel(value: Double)
}