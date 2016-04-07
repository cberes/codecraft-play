package net.seabears.play.codecraft

import cwinter.codecraft.core.api._
import cwinter.codecraft.util.maths.Vector2
import scala.util.Random

class Harvester(mothership: DroneController, distance: Int) extends DroneController {
  private[this] var minerals = Set[MineralCrystal]()

  def enemiesInRange: Set[Drone] = enemiesInSight filter isInMissileRange

  def freshMinerals: Set[MineralCrystal] = minerals filterNot (_.harvested)

  def randomPosition: Vector2 = {
    val randomDirection = Vector2(2 * math.Pi * Random.nextDouble())
    position + distance * randomDirection
  }

  override def onMineralEntersVision(mineral: MineralCrystal) =
    if (!mineral.harvested) minerals += mineral

  override def onArrivesAtMineral(mineral: MineralCrystal) = {
    minerals -= mineral
    if (!mineral.harvested) harvest(mineral)
  }

  override def onArrivesAtDrone(drone: Drone) = giveResourcesTo(drone)

  override def onTick(): Unit = {
    enemiesInRange.headOption match {
      // return fire
      case Some(enemy) if missileBatteries > 0 => fireMissilesAt(enemy)
      case _ => enemiesInSight.headOption match {
        // flee
        case Some(enemy) if missileBatteries == 0 => moveTo(mothership)
        case _ => {
          // harvest as usual
          if (!isMoving && !isHarvesting) {
            if (availableStorage == 0) moveTo(mothership)
            else if (!freshMinerals.isEmpty) moveTo(freshMinerals.head)
            else moveTo(randomPosition)
          }
        }
      }
    }
  }
}
