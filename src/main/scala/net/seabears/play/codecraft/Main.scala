package net.seabears.play.codecraft

import cwinter.codecraft.core.api._

object Main {
  def main(args: Array[String]): Unit = {
    TheGameMaster.runLevel2(new Mothership)
  }
}
