package net.seabears.play.codecraft

import cwinter.codecraft.core.api._

class Mothership extends DroneController {
  var harvesters = 0
  var sentries = 0

  override def onTick(): Unit =
    if (!isConstructing) {
      // create twice as many harvesters as sentries
      if (harvesters > sentries + 1) {
        buildDrone(new Sentry(this), missileBatteries = 2, shieldGenerators = 1)
        sentries += 1
      } else {
        buildDrone(new Harvester(this), storageModules = 2)
        harvesters += 1
      }
    }
}
