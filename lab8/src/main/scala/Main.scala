import com.restfb.types.User
import facebookAdapter.FacebookAdapter
import socialObject.SocialObject

object Main extends App {
  val accessToken = sys.env.get("ACCESS_TOKEN") match {
    case Some(value) => value
    case None => throw new Exception("ACCESS_TOKEN is not defined")
  }
  val testUser1Id = "111330467243454"
  val testUser2Id = "111330467243454"

  val social = new SocialObject[User](FacebookAdapter, accessToken)

  social.compareLikes("logs.txt", testUser1Id, testUser2Id)
}
