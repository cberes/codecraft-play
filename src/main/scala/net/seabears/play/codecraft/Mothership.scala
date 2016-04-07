package net.seabears.play.codecraft

import cwinter.codecraft.core.api._

class Mothership extends DroneController {
  private[this] var harvesters = List[Harvester]()
  private[this] var guardedHarvesters = Set[Harvester]()
  private[this] var sentries = List[Sentry]()
  private[this] var guards = List[Guard]()

  def livingHarvesters: List[Harvester] = harvesters filterNot (_.isDead)
  def harvestersToGuard: List[Harvester] = livingHarvesters filterNot guardedHarvesters.contains
  def livingGuards: List[Guard] = guards filterNot (_.isDead)

  def enemiesInRange: Set[Drone] = enemiesInSight filter isInMissileRange

  def build: Unit = {
    val harvestersAlive = livingHarvesters
    val guardsAlive = livingGuards
    if (harvestersAlive.size < 2) {
      // make sure there are at least 2 harvesters
      harvesters = new Harvester(this, 1000) :: harvestersAlive
      buildDrone(harvesters.head, storageModules = 1)
    } else if (harvestersAlive.size > guardsAlive.size) {
      // create guard if there are unguarded harvesters
      // assign at most one guard to a harvester
      val harvester: Option[Harvester] = harvestersToGuard.headOption
      harvester map (guardedHarvesters += _)
      guards = new Guard(harvester, 1000) :: guardsAlive
      buildDrone(guards.head, missileBatteries = 1, engines = 1)
    } else if (harvestersAlive.size < 6) {
      // create at most 6 harvesters
      harvesters = new Harvester(this, 1500) :: harvestersAlive
      buildDrone(harvesters.head, storageModules = 2)
    } else {
      // create soldiers for attack
      sentries = new Sentry(1500) :: sentries
      buildDrone(sentries.head, missileBatteries = 3, shieldGenerators = 1)
      // TODO when there are enough, send them to attack together
    }
  }

  override def onTick(): Unit = enemiesInRange.headOption match {
    case Some(enemy) if missileBatteries > 0 => fireMissilesAt(enemy)
    case _ => if (!isConstructing) build
  }
}
