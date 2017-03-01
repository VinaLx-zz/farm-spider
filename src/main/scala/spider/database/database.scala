package spider
package database

import slick.jdbc.JdbcBackend._
import slick.jdbc.MySQLProfile.api.{ Database ⇒ _, _ }
import scala.concurrent.Await
import scala.concurrent.duration.Duration.Inf

object FarmDB {
  private def getURL(
    host: String, port: Int, db: Option[String] = None): String = {
    val url = s"jdbc:mysql://$host:$port"
    db match {
      case None ⇒ url
      case Some(name) ⇒ s"${url}/$name"
    }
  }
  def connect(
    username: String = null,
    password: String = null,
    db: String = "farm",
    host: String = "localhost",
    port: Int = 3306): Database = {
    Await.result(Database.forURL(
      getURL(host, port),
      user = username,
      password = password,
      driver = "com.mysql.jdbc.Driver")
      .run(createDatabase(db)), Inf)
    Database.forURL(
      s"jdbc:mysql://$host:$port/$db",
      user = username,
      password = password,
      driver = "com.mysql.jdbc.Driver")
  }
  private def createDatabase(db: String) = {
    sqlu"CREATE DATABASE IF NOT EXISTS #$db"
  }

  def createProductTable = FarmTable.schema.create
}