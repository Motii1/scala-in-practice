package facebookAdapter

import socialObject.ISocial
import java.io.FileWriter
import java.util.Calendar

import com.restfb.types.{ Likes, User }
import com.restfb.{ DefaultFacebookClient, Version }

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object FacebookAdapter extends ISocial[User] {
  private val myAppSecret = sys.env.get("APP_SECRET") match {
    case Some(value) => value
    case None => throw new Exception("APP_SECRET is not defined")
  }

  class MyFacebookClient(currentAccessToken: String)
    extends DefaultFacebookClient(
      currentAccessToken,
      myAppSecret,
      Version.VERSION_5_0) {}

  def getUser(accessToken: String, id: String): Future[User] = Future {
    val client = new MyFacebookClient(accessToken)
    val user = client.fetchObject(id, classOf[User])
    user
  }

  def extendFile(path: String, user1: String, user2: String): Unit = {
    val fw = new FileWriter(path, true)
    try {
      fw.write(s"${Calendar.getInstance.getTime} $user1 $user2\n")
    } finally fw.close()
  }

  def userStatsString(user: User): String = {
    val name = user.getName
    val likes = user.getLikes match {
      case l: Likes => l.getTotalCount
      case _ => "no likes"
    }

    s"$name, likes: $likes"
  }

  def usersComparisonString(user1: User, user2: User): String = {
    userStatsString(user1) + " vs. " + userStatsString(user2)
  }
}
