package net.seabears.play.codecraft

import cwinter.codecraft.core.api._
import cwinter.codecraft.util.maths.Vector2
import scala.util.Random

class Soldier(distance: Int) extends DroneController {
  def enemiesInRange: Set[Drone] = enemiesInSight filter isInMissileRange

  def randomPosition: Vector2 = {
    val randomDirection = Vector2(2 * math.Pi * Random.nextDouble())
    position + distance * randomDirection
  }

  override def onTick(): Unit = {
    enemiesInRange.headOption match {
      case Some(enemy) => fireMissilesAt(enemy)
      case None => enemiesInSight.headOption match {
        case Some(enemy) => if (!isMoving) moveTo(enemiesInSight.head)
        case None => if (!isMoving) moveTo(randomPosition)
      }
    }
  }
}
