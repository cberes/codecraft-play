package net.seabears.play.codecraft

import cwinter.codecraft.core.api._
import cwinter.codecraft.util.maths.Vector2
import scala.util.Random

class Guard(subject: Option[DroneController], distance: Int) extends DroneController {
  def enemiesInRange: Set[Drone] = enemiesInSight filter isInMissileRange

  def randomPosition: Vector2 = {
    val randomDirection = Vector2(2 * math.Pi * Random.nextDouble())
    position + distance * randomDirection
  }

  def isAssigned: Boolean = !subject.isEmpty && !subject.get.isDead

  override def onTick(): Unit = {
    enemiesInRange.headOption match {
      case Some(enemy) => fireMissilesAt(enemy)
      case None => {
        // no enemies in range: follow subject
        if (!isMoving) {
          subject match {
            case Some(drone) if !drone.isDead => moveTo(drone)
            case _ => moveTo(randomPosition)
          }
        }
      }
    }
  }
}
