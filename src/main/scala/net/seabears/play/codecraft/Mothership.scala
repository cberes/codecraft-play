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
  private[this] var livingGuards = Set[Guard]()
  private[this] var livingSoldiers = Set[Soldier]()
  private[this] var leader: Option[Soldier] = None

  def livingHarvesters: List[Harvester] = harvesters filterNot (_.isDead)
  def harvestersToGuard: List[Harvester] = livingHarvesters filterNot guardedHarvesters.contains

  def enemiesInRange: Set[Drone] = enemiesInSight filter isInMissileRange

  def add(guard: Guard): Unit = livingGuards += guard

  def remove(guard: Guard): Unit = livingGuards -= guard

  def add(soldier: Soldier): Unit = {
    livingSoldiers += soldier
    if (leader.isEmpty) leader = Some(soldier)
  }

  def remove(soldier: Soldier): Unit = {
    livingSoldiers -= soldier
    if (!leader.isEmpty && leader.get == soldier) {
      leader = livingSoldiers.headOption
    }
  }

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

  def removeEnemy: Unit = getEnemy map (enemies -= _)

  def getLeader: Option[Soldier] = leader

  def isLeader(soldier: Soldier): Boolean = leader match {
    case Some(s) => s == soldier
    case _ => false
  }

  def soldierCount: Int = livingSoldiers.size

  def build: Unit = {
    val harvestersAlive = livingHarvesters
    if (harvestersAlive.size < 2) {
      // make sure there are at least 2 harvesters
      val harvester = new Harvester(this, 1000)
      buildDrone(harvester, storageModules = 2)
      harvesters = harvester :: harvestersAlive
    } else if (harvestersAlive.size > livingGuards.size) {
      // create guard if there are unguarded harvesters
      // assign at most one guard to a harvester
      val harvester: Option[Harvester] = harvestersToGuard.headOption
      harvester map (guardedHarvesters += _)
      val guard = new Guard(this, harvester, 1000)
      buildDrone(guard, missileBatteries = 2)
      guards = guard :: guards
    } else if (harvestersAlive.size < 6) {
      // TODO also check if there are minerals?
      // create at most 6 harvesters
      val harvester = new Harvester(this, 1500)
      buildDrone(harvester, storageModules = 2)
      harvesters = harvester :: harvestersAlive
    } else {
      // create soldiers for attack
      val soldier = new Soldier(this, 1500)
      buildDrone(soldier, missileBatteries = 3, shieldGenerators = 1)
      soldiers = soldier :: soldiers
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
