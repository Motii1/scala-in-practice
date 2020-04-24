import plugins._
import actions._

object Main extends App {
  val rev = new Pluginable with Reverting
  val lowerCase = new Pluginable with LowerCasing
  val singleSpace = new Pluginable with SingleSpacing
  val noSpace = new Pluginable with NoSpacing
  val duplicateRemove = new Pluginable with DuplicateRemoval
  val rotate = new Pluginable with Rotating
  val doubling = new Pluginable with Doubling
  val shortening = new Pluginable with Shortening 

  require(rev.plugin("dawid motak ").getOrElse("unknown") == " katom diwad");
  require(lowerCase.plugin("FirsT NaMe").getOrElse("unknown") == "first name")
  require(singleSpace.plugin("ala ma  kota").getOrElse("unknown") == "ala ma kota")
  require(noSpace.plugin("ala ma  kota").getOrElse("unknown") == "alamakota")
  require(duplicateRemove.plugin("alzaa  cda").getOrElse("unknown") == "lzcd")
  require(rotate.plugin("abc").getOrElse("unknown") == "cab")
  require(doubling.plugin("abcd").getOrElse("unknown") == "abbcdd")
  require(shortening.plugin("ab cd").getOrElse("unknown") == "a d")


  require(Actions.actionA.plugin("ab   cd       e").getOrElse("unknown") == "abcd ")
  require(Actions.actionB.plugin("Fir  sT  NaM  e   ").getOrElse("unknown") == "FrrTaae")
  require(Actions.actionC.plugin("FirsT NaMe").getOrElse("unknown") == "fiirsst  naamee")
  require(Actions.actionD.plugin("XiX;sd;qqqq; ;;;;;QQQ;no;Tg;uQQta").getOrElse("unknown") == "aisd noTgut")
  require(Actions.actionE.plugin("a  b    c  d").getOrElse("unknown") == "cca")
  require(Actions.actionF.plugin("abcde").getOrElse("unknown") == "abcde")
  require(Actions.actionG.plugin("ab   cd       e").getOrElse("unknown") == "acc")
}