package net.seabears.play.codecraft

import cwinter.codecraft.core.api._
import cwinter.codecraft.util.maths.Vector2
import scala.util.Random

class Guard(mothership: Mothership, subject: Option[DroneController], distance: Int) extends DroneController {
  def enemiesInRange: Set[Drone] = enemiesInSight filter isInMissileRange

  def randomPosition: Vector2 = {
    val randomDirection = Vector2(2 * math.Pi * Random.nextDouble())
    position + distance * randomDirection
  }

  def randomOffset: Vector2 = {
    Vector2(20 * Random.nextDouble())
  }

  def isAssigned: Boolean = !subject.isEmpty && !subject.get.isDead

  override def onDroneEntersVision(drone: Drone) =
    if (drone.isEnemy) mothership.add(drone)

  override def onSpawn: Unit = mothership.add(this)

  override def onDeath: Unit = mothership.remove(this)

  override def onTick(): Unit = {
    enemiesInRange.headOption match {
      case Some(enemy) => fireMissilesAt(enemy)
      case None => {
        // no enemies in range: follow subject
        if (!isMoving) {
          // follow subject or follow the leader
          subject match {
            case Some(drone) if !drone.isDead => moveTo(drone)
            case _ => mothership.getEnemy match {
              // pursue enemies or follow the leader
              case Some(e) if mothership.soldierCount > 4 => moveTo(e.lastKnownPosition)
              case _ => mothership.getLeader match {
                // follow the leader
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
