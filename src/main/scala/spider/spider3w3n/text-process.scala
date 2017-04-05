package spider
package spider3w3n

import spider.Util.md5Hash
import spider.Util.DateTimeUtil.fromFormatString

// json parsing
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods
// html parsing
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model._

import Combinators._
import TextProcessing._
import Sinker._

object TextProcessing {
  private[spider3w3n] def extractHash(page: String): Option[String] = {
    val extractorRegex = """enc\('(\w+?)'\)""".r
    extractorRegex findFirstMatchIn page map (_.group(1)) map (md5Hash(_))
  }

  private[spider3w3n] def extractCategoryIds(
    page: String): IndexedSeq[(Int, String)] = {
    val categories =
      JsoupBrowser().parseString(page) >> elementList(".category_item")
    (categories map { span ⇒
      val id = ("""\d+""".r findFirstIn span.attr("onclick")).get.toInt
      (id, span.text)
    }).toIndexedSeq
    // val extractorRegex = """getProductListByPid\((\d+),""".r
    // (extractorRegex findAllMatchIn page map (_.group(1).toInt)).toIndexedSeq
  }

  private[spider3w3n] def parseTypeListJson(
    j: String): IndexedSeq[(Int, String)] = {
    (for {
      JObject(fields) ← JsonMethods.parse(j)
      JField("id", JInt(id)) ← fields
      JField("type", JString(name)) ← fields
    } yield (id, name))
      .map { case (bigint, name) ⇒ (bigint.intValue, name) }
      .toIndexedSeq
  }

  private[spider3w3n] def parseProductTable(
    page: String): IndexedSeq[Record3w3n] = {
    val table = JsoupBrowser().parseString(page) >> elementList("tr")
    if (table.isEmpty) return IndexedSeq.empty
    val recordList =
      table.tail map (_ >> texts("td")) map {
        (fields: Iterable[String]) ⇒
          val cols = fields.toIndexedSeq
          val price = cols(1).trim.split("""[^\d\.]""", 2)(0).toDouble
          val date = fromFormatString(cols(3).trim, "yyyy-MM-dd")
          Record3w3n(
            name = cols(0),
            price = price,
            market = cols(2),
            // infoSource = cols(3),
            date = date)
      }
    recordList.toIndexedSeq
  }
}