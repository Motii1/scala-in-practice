package actions

import plugins._

object Actions {
  val actionA: Pluginable = new Pluginable with Shortening with Doubling with SingleSpacing
  val actionB: Pluginable = new Pluginable with Doubling with Shortening with NoSpacing 
  val actionC: Pluginable = new Pluginable with Doubling with LowerCasing
  val actionD: Pluginable = new Pluginable with Rotating with DuplicateRemoval
  val actionE: Pluginable = new Pluginable with Reverting with Doubling with Shortening with NoSpacing

  val actionF: Pluginable = new Pluginable {
    private def pluginAndUnpackOption(text: String): String = ((new Pluginable with Rotating).plugin(text)).getOrElse("undefinded")  // will always return a valid non null String

    override def plugin(text: String): Option[String] = Option(text) match {
      case Some(x) => {
        def helper(acc: String, n: Int): String = n match {
          case 0 => acc
          case n => helper(pluginAndUnpackOption(acc), n - 1)
        }

        super.plugin(helper(x, 5))
      }
      case None => None
    }
  }

  val actionG: Pluginable = new Pluginable {
    override def plugin(text: String): Option[String] = actionA.plugin(text) match {
      case Some(x) => actionB.plugin(x)  
      case None => None
    }
  }
}