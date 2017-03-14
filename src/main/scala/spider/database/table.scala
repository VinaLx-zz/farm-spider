package spider.database

import slick.jdbc.MySQLProfile.api._
import slick.lifted.Tag
import slick.dbio.DBIO

import com.github.nscala_time.time.Imports._

import java.sql.Date

import spider.spider3w3n.Record3w3n
import spider.Util.DateTimeUtil.{ toSQLDate, stripHourMinSec }

class FarmTable(tag: Tag)
  extends Table[(String, Option[String], Double, String, Date)](
    tag, Some("farm"), "Farm") {
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
      toSQLDate(record.date)))
  }

  def clearRecordsAction(dates: Seq[DateTime]) = {
    DBIO.sequence(
      dates map { date ⇒
        (this filter (_.date === toSQLDate(stripHourMinSec(date)))).delete
      })
  }
}