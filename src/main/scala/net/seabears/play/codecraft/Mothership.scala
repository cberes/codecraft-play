package net.seabears.play.codecraft

import cwinter.codecraft.core.api._

class Mothership extends DroneController {
  private[this] var harvesters = List[Harvester]()
  private[this] var guardedHarvesters = Set[Harvester]()
  private[this] var soldiers = List[Soldier]()
  private[this] var guards = List[Guard]()
  private[this] var minerals = Set[MineralCrystal]()
  private[this] var enemies = Set[Drone]()
  private[this] var claimed = Set[MineralCrystal]()

  def livingHarvesters: List[Harvester] = harvesters filterNot (_.isDead)
  def harvestersToGuard: List[Harvester] = livingHarvesters filterNot guardedHarvesters.contains
  def livingSoldiers: List[Soldier] = soldiers filterNot (_.isDead)
  def livingGuards: List[Guard] = guards filterNot (_.isDead)

  def enemiesInRange: Set[Drone] = enemiesInSight filter isInMissileRange

  def add(mineral: MineralCrystal): Unit =
    minerals = minerals.filterNot(_.harvested) + mineral

  def add(enemy: Drone): Unit = enemies += enemy

  def getMineral: Option[MineralCrystal] = {
    val liveMinerals = (minerals -- claimed) filterNot (_.harvested)
    if (liveMinerals.isEmpty) None else Some(liveMinerals maxBy (_.size))
  }

  def getClaimed = claimed

  def claim(mineral: MineralCrystal): Unit = claimed += mineral

  def unclaim(mineral: MineralCrystal): Unit = claimed -= mineral

  def getEnemy: Option[Drone] = {
    val liveEnemies = enemies filterNot (_.isDead)
    if (liveEnemies.isEmpty) None else Some(liveEnemies maxBy (_.missileBatteries))
  }

  def build: Unit = {
    val harvestersAlive = livingHarvesters
    val guardsAlive = livingGuards
    if (harvestersAlive.size < 2) {
      // make sure there are at least 2 harvesters
      harvesters = new Harvester(this, 1000) :: harvestersAlive
      buildDrone(harvesters.head, storageModules = 2)
    } else if (harvestersAlive.size > guardsAlive.size) {
      // create guard if there are unguarded harvesters
      // assign at most one guard to a harvester
      val harvester: Option[Harvester] = harvestersToGuard.headOption
      harvester map (guardedHarvesters += _)
      guards = new Guard(harvester, 1000) :: guardsAlive
      buildDrone(guards.head, missileBatteries = 2)
    } else if (harvestersAlive.size < 6) {
      // TODO also check if there are minerals?
      // create at most 6 harvesters
      harvesters = new Harvester(this, 1500) :: harvestersAlive
      buildDrone(harvesters.head, storageModules = 2)
    } else {
      // if there are already a few soldiers, send them to attack
      val soldiersAlive = livingSoldiers
      if (soldiersAlive.size > 4) {
        // send all soldiers and free guards to the enemy's position
        getEnemy map (e => {
          val enemyPosition = e.lastKnownPosition
          soldiersAlive foreach (_.moveTo(enemyPosition))
          (livingGuards filterNot (_.isAssigned)) foreach (_.moveTo(enemyPosition))
        })
      }
      // create soldiers for attack
      val soldier = new Soldier(500)
      soldiers = soldier :: soldiersAlive
      buildDrone(soldiers.head, missileBatteries = 3, shieldGenerators = 1)
    }
  }

  override def onTick(): Unit = enemiesInRange.headOption match {
    case Some(enemy) => {
      // call soldiers to attack
      livingSoldiers foreach (_.moveTo(position))
      // return fire
      if (missileBatteries > 0) fireMissilesAt(enemy)
    }
    case _ => if (!isConstructing) build
  }
}
