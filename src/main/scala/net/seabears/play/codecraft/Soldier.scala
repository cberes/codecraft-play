package net.seabears.play.codecraft

import cwinter.codecraft.core.api._
import cwinter.codecraft.util.maths.Vector2
import scala.util.Random

class Soldier(mothership: Mothership, distance: Int) extends DroneController {
  def enemiesInRange: Set[Drone] = enemiesInSight filter isInMissileRange

  def randomPosition: Vector2 = {
    val randomDirection = Vector2(2 * math.Pi * Random.nextDouble())
    position + distance * randomDirection
  }

  def randomOffset: Vector2 = {
    Vector2(20 * Random.nextDouble())
  }

  override def onDroneEntersVision(drone: Drone) =
    if (drone.isEnemy) mothership.add(drone)

  override def onArrivesAtPosition =
    if (mothership.isLeader(this)) mothership.removeEnemy

  override def onSpawn: Unit = mothership.add(this)

  override def onDeath: Unit = mothership.remove(this)

  override def onTick(): Unit = {
    enemiesInRange.headOption match {
      case Some(enemy) => fireMissilesAt(enemy)
      case None => enemiesInSight.headOption match {
        case Some(enemy) => if (!isMoving) moveTo(enemiesInSight.head)
        case None => if (!isMoving) {
          // attack if there are attackers, otherwise wait
          mothership.getEnemy match {
            case Some(e) if mothership.soldierCount > 4 => moveTo(e.lastKnownPosition)
            case _ => {
              // if I'm the leader, determine the next position
              if (mothership.isLeader(this)) moveTo(randomPosition)
              // else, follow the leader
              else mothership.getLeader match {
                case Some(leader) => moveTo(leader.lastKnownPosition)
                case None => moveTo(randomPosition)
              }
            }
          }
        }
      }
    }
  }
}
