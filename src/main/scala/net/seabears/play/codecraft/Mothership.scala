package net.seabears.play.codecraft

import cwinter.codecraft.core.api._
import cwinter.codecraft.util.maths.Vector2
import scala.util.Random

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

  def randomPosition: Vector2 = {
    val randomDirection = Vector2(2 * math.Pi * Random.nextDouble())
    position + 500 * randomDirection
  }

  def moveToAttack: Unit = {
    val fallback = randomPosition
    val attackers: List[DroneController] = (livingGuards filterNot (_.isAssigned)) ::: livingSoldiers
    getEnemy match {
      case Some(e) => attackers foreach (_.moveTo(e.lastKnownPosition))
      case None => attackers foreach (_.moveTo(fallback))
    }
  }

  def build: Unit = {
    // TODO move this where it will be called more frequently
    if (soldiers.size > 4) {
      moveToAttack
    }

    val harvestersAlive = livingHarvesters
    val guardsAlive = livingGuards

    if (harvestersAlive.size < 2) {
      // make sure there are at least 2 harvesters
      val harvester = new Harvester(this, 1000)
      buildDrone(harvester, storageModules = 2)
      harvesters = harvester :: harvestersAlive
    } else if (harvestersAlive.size > guardsAlive.size) {
      // create guard if there are unguarded harvesters
      // assign at most one guard to a harvester
      val harvester: Option[Harvester] = harvestersToGuard.headOption
      harvester map (guardedHarvesters += _)
      val guard = new Guard(harvester, 1000)
      buildDrone(guard, missileBatteries = 2)
      guards = guard :: guardsAlive
    } else if (harvestersAlive.size < 6) {
      // TODO also check if there are minerals?
      // create at most 6 harvesters
      val harvester = new Harvester(this, 1500)
      buildDrone(harvester, storageModules = 2)
      harvesters = harvester :: harvestersAlive
    } else {
      // create soldiers for attack
      val soldier = new Soldier(500)
      buildDrone(soldier, missileBatteries = 3, shieldGenerators = 1)
      soldiers = soldier :: livingSoldiers
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
