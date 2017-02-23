package spider

import java.util.Date

case class FarmRecord(
  name: String,
  highestPrice: Double,
  lowestPrice: Double,
  market: String,
  region: String,
  infoSource: String,
  date: Date)