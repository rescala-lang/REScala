package replication.fbdc

import kofre.base.Bottom

import java.nio.file.{Files, Path}
import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Statement
import java.util.Properties
import scala.collection.mutable.ListBuffer
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps
import kofre.base.Bottom
import kofre.datatypes.LastWriterWins

object Northwind {

  val dbProperties = new Properties().tap(_.setProperty("immutable", "1"))

  def enableConditional(exampleData: FbdcExampleData, path: Path) = {
    if !Files.isRegularFile(path)
    then println(s"northwind $path not found")
    else
      println(s"opening northwind database")
      val connection = DriverManager.getConnection(s"jdbc:sqlite:${path.toString}", dbProperties)

      def query(q: String): List[Map[String, String]] =
        val st      = connection.createStatement()
        try
          val res     = st.executeQuery(q)
          val meta    = res.getMetaData
          val columns = (1 to meta.getColumnCount).map(meta.getColumnName)
          val lb      = ListBuffer.empty[Map[String, String]]
          while res.next()
          do
            lb.append(columns.map(c => c -> res.getString(c)).toMap)
          lb.toList
        catch
          case e: Exception => List(Map("error" -> e.toString))
        finally
          st.close()

      import exampleData.dataManager
      exampleData.addCapability("northwind")
      exampleData.requestsOf[Req.Northwind].observe { queries =>
        dataManager.transform { current =>
          current.modRes { reqq =>
            queries.foreach{ q =>
              val resp = Res.Northwind(q.value, query(q.value.query))
              reqq.observeRemoveMap.insert("northwind", Some(LastWriterWins.now(resp, exampleData.replicaId)))
            }
          }
        }
      }

  }
}
