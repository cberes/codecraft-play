package net.seabears.play.codecraft

import cwinter.codecraft.core.api._
import cwinter.codecraft.util.maths.Vector2
import scala.util.Random

class Harvester(mothership: Mothership, distance: Int) extends DroneController {
  private[this] var minerals = Set[MineralCrystal]()
  private[this] var claimed: Option[MineralCrystal] = None

  def enemiesInRange: Set[Drone] =
    (enemiesInSight filter isInMissileRange) filter (_.missileBatteries > 0)

  def freshMinerals: Set[MineralCrystal] = minerals filterNot (_.harvested)

  def randomPosition: Vector2 = {
    val randomDirection = Vector2(2 * math.Pi * Random.nextDouble())
    position + distance * randomDirection
  }

  override def onMineralEntersVision(mineral: MineralCrystal) =
    if (!mineral.harvested){
      minerals += mineral
      mothership.add(mineral)
    }

  override def onArrivesAtMineral(mineral: MineralCrystal) = {
    minerals -= mineral
    if (!mineral.harvested) harvest(mineral)
  }

  override def onDroneEntersVision(drone: Drone) =
    if (drone.isEnemy) mothership.add(drone)

  override def onArrivesAtDrone(drone: Drone) = giveResourcesTo(drone)

  def moveToHarvest(mineral: MineralCrystal): Unit = {
    mothership claim mineral
    claimed = Some(mineral)
    moveTo(mineral)
  }

  def releaseClaims: Unit = claimed foreach {
    claimed = None
    mothership.unclaim(_)
  }

  override def onDeath: Unit = releaseClaims

  override def onTick(): Unit = {
    enemiesInRange.headOption match {
      // return fire
      case Some(enemy) if missileBatteries > 0 => fireMissilesAt(enemy)
      case _ => enemiesInSight.headOption match {
        // flee
        case Some(enemy) if missileBatteries == 0 => moveTo(mothership)
        case _ => {
          if (!isMoving && !isHarvesting) {
            // harvest as usual
            releaseClaims
            if (availableStorage == 0) moveTo(mothership)
            else (freshMinerals -- mothership.getClaimed).headOption match {
              case Some(m) => moveToHarvest(m)
              case None => mothership.getMineral match {
                case Some(m) => moveToHarvest(m)
                case None => moveTo(randomPosition)
              }
            }
          }
        }
      }
    }
  }
}
