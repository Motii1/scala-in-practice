package plugins

trait Pluginable { 
  def plugin(text: String): Option[String] = Option(text) 
}

trait Reverting extends Pluginable {
  abstract override def plugin(text: String): Option[String] = Option(text) match {
    case Some(x) => super.plugin(x.reverse)
    case None => None
  }
}

trait LowerCasing extends Pluginable {
  abstract override def plugin(text: String): Option[String] = Option(text) match {
    case Some(x) => super.plugin(x.toLowerCase)
    case None => None
  }
}

trait SingleSpacing extends Pluginable {
  abstract override def plugin(text: String): Option[String] = Option(text) match {
    case Some(x) => super.plugin(x.replaceAll("  +", " "))
    case None => None
  } 
}

trait NoSpacing extends Pluginable {
  abstract override def plugin(text: String): Option[String] = Option(text) match {
    case Some(x) => super.plugin(x.replaceAll(" +", ""))
    case None => None
  }
}

trait DuplicateRemoval extends Pluginable {
  abstract override def plugin(text: String): Option[String] = {
    def helper(text: List[Char], index: Int): List[Char] = index match {
      case _ if index == text.length => text
      case i if text.filter(_ == text(i)).length > 1 => helper(text.filter(_ != text(i)), 0)
      case _ => helper(text, index + 1)
    }

    Option(text) match {
      case Some(x) => super.plugin(helper(x.toList, 0).mkString)
      case None => None
    }
  }
}

trait Rotating extends Pluginable {
  abstract override def plugin(text: String): Option[String] = Option(text) match {
    case Some(x) => super.plugin(x(x.length() - 1) + x.substring(0, x.length() - 1))
    case None => None
  }
}

trait Doubling extends Pluginable {
  abstract override def plugin(text: String): Option[String] = {
    def helper(index: Int, acc: String): String = index match {
      case _ if index == text.length() => acc
      case i if i % 2 == 1 => helper(i + 1, acc + text(i) + text(i))
      case c => helper(c + 1, acc + text(c))
    }

    Option(text) match {
      case Some(_) => super.plugin(helper(0, ""))
      case None => None 
    }
  }
}

trait Shortening extends Pluginable {
  abstract override def plugin(text: String): Option[String] = {
    def helper(index: Int, acc: String): String = index match {
      case _ if index == text.length() => acc
      case i if index % 2 == 1 => helper(i + 1, acc + "")
      case c => helper(c + 1, acc + text(c))
    }

    Option(text) match {
      case Some(_) => super.plugin(helper(0, ""))
      case None => None 
    }
  }
}