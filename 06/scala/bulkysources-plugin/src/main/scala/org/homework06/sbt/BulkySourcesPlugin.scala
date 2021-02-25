package org.homework06.sbt

import sbt.Keys.sources
import sbt.{Def, _}
import complete.DefaultParsers._

object BulkySourcesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  lazy val bulkyThresholdInLines = settingKey[Int]("Defines threshold in lines for bulkySources task")
  lazy val bulkySources = inputKey[Seq[(Int, File)]]("Returns the source files which have no less lines than defined in bulkyThresholdInLines or specified by argument")

  val threshold = (Space ~> Digit.+map(_.mkString.toInt)).?.failOnException !!! s"Expected threshold not bigger than ${Int.MaxValue} or invalid number"

  lazy val baseBulkySources = Seq(
    bulkyThresholdInLines := 100,
    bulkySources := {
      val linesThreshold = threshold.parsed.getOrElse(bulkyThresholdInLines.value)
      (sources in bulkySources).value.flatMap { file =>
        val linesCount = IO.readLines(file).length
        if (linesCount >= linesThreshold) Some(linesCount, file) else None
      }.sorted.reverse
    }
  )

  object autoImport {
    def bulkyThresholdInLines: SettingKey[Int] = BulkySourcesPlugin.bulkyThresholdInLines
    def bulkySources: InputKey[Seq[(Int, File)]] = BulkySourcesPlugin.bulkySources
  }

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(baseBulkySources) ++ inConfig(Test)(baseBulkySources)
}
