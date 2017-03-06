package spider
package database

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

import scala.util.{ Try, Success, Failure }

case class DBConfig(
  username: String = "root",
  password: Option[String] = None,
  host: String = "localhost",
  port: Int = 3306,
  db: Option[String] = None,
  properties: Seq[(String, String)] = Seq.empty)

object DBConfig {
  def default: DBConfig = DBConfig()

  def loadJson(s: String): Try[DBConfig] = {
    Try(JsonMethods.parse(s)) match {
      case Success(json) ⇒ Success(fromJsonValue(json))
      case Failure(e) ⇒ Failure(e)
    }
  }

  private def fromJsonValue(json: JValue): DBConfig = {
    // a better way ? :(
    var config = default
    config = json \ "username" match {
      case JString(s) ⇒ config.copy(username = s)
      case _ ⇒ config
    }
    config = json \ "password" match {
      case JString(s) ⇒ config.copy(password = Some(s))
      case _ ⇒ config
    }
    config = json \ "db" match {
      case JString(s) ⇒ config.copy(db = Some(s))
      case _ ⇒ config
    }
    config = json \ "host" match {
      case JString(s) ⇒ config.copy(host = s)
      case _ ⇒ config
    }
    config = json \ "port" match {
      case JInt(i) ⇒ config.copy(port = i.intValue)
      case _ ⇒ config
    }
    config = json \ "properties" match {
      case JObject(fields) ⇒
        val kvPairs = for {
          JField(key, JString(value)) ← fields
        } yield (key, value)
        config.copy(properties = config.properties ++ kvPairs.toIndexedSeq)
      case _ ⇒ config
    }
    config
  }
}