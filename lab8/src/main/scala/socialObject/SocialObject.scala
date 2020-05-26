package socialObject

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration

trait ISocial[T] {
  def getUser(accessToken: String, id: String): Future[T]

  def extendFile(path: String, user1: String, user2: String): Unit

  def userStatsString(user: T): String

  def usersComparisonString(user1: T, user2: T): String
}

class SocialObject[T](adapter: ISocial[T], accessToken: String) {
  private def handleFailure[U](f: U): Unit = f match {
    case ex: Exception => throw ex
    case _ => ()
  }

  def compareLikes(
    logFile: String,
    user1: String, user2: String): Unit = {

    val user1Future = adapter.getUser(accessToken, user1)
    val user2Future = adapter.getUser(accessToken, user2)

    val results = Await.result(user1Future zip user2Future, Duration.Inf)
    val user1Value = results._1
    val user2Value = results._2

    handleFailure(user1Value)
    handleFailure(user2Value)

    adapter.extendFile(logFile, user1, user2)
    println(adapter.usersComparisonString(user1Value, user2Value))
  }
}
