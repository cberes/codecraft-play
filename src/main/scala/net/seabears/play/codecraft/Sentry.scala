package net.seabears.play.codecraft

import cwinter.codecraft.core.api._
import cwinter.codecraft.util.maths.Vector2
import scala.util.Random

class Sentry(mothership: DroneController) extends DroneController {
  override def onTick(): Unit = {
      if (enemiesInSight.isEmpty) {
        // no enemies in sight
        if (!isMoving) {
          val randomDirection = Vector2(2 * math.Pi * Random.nextDouble())
          val targetPosition = position + 500 * randomDirection
          moveTo(targetPosition)
        }
      } else {
        // enemies in sight
        val enemiesInRange = enemiesInSight filter isInMissileRange
        if (enemiesInRange.isEmpty) {
          // no enemiesin range
          if (!isMoving) {
            moveTo(enemiesInSight.head)
          }
        } else {
          // enemies in range: fire away
          fireMissilesAt(enemiesInRange.head)
        }
      }
  }
}
