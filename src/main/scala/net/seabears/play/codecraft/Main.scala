package net.seabears.play.codecraft

import cwinter.codecraft.core.api._

object Main {
  def main(args: Array[String]): Unit = {
    TheGameMaster.runLevel1(new Mothership)
  }
}
