package spider.database

import slick.jdbc.MySQLProfile.api._
import slick.lifted.Tag
import java.sql.Date

import spider.spider3w3n.Record3w3n

class FarmTable(tag: Tag)
  extends Table[(String, Option[String], Double, String, Date)](
    tag, Some("Farm"), "Farm") {
  def name = column[String]("NAME")
  def subtype = column[Option[String]]("SUBTYPE")
  def price = column[Double]("PRICE")
  def market = column[String]("MARKET")
  def date = column[Date]("DATE")
  def * = (name, subtype, price, market, date)
}

object FarmTable extends TableQuery(new FarmTable(_)) {
  def insert(name: String, record: Record3w3n) = {
    this += ((
      name,
      Some(record.name),
      record.price,
      record.market,
      new Date(record.date.getMillis)))
  }
}