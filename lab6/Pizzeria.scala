package pizzeria

sealed abstract class Size
case object Small extends Size
case object Regular extends Size
case object Large extends Size

sealed abstract class Topping(val price: Double)
case object Ketchup extends Topping(0.5)
case object Garlic extends Topping(0.5)

sealed abstract class Meat(val price: Double)
case object Salami extends Meat(1)

sealed abstract class CrustType
case object Thin extends CrustType
case object Thick extends CrustType

sealed abstract class PizzaType(val price: Double)
case object Margarita extends PizzaType(5)
case object Pepperoni extends PizzaType(6.5)
case object Funghi extends PizzaType(7)

sealed abstract class Discount
case object Student extends Discount
case object Senior extends Discount

sealed abstract class Drink(val price: Double)
case object Lemonade extends Drink(2)

case class Pizza(
  pizzaType: PizzaType,
  size: Size,
  crustType: CrustType,
  extraMeat: Option[Seq[Meat]] = None,
  extraTopping: Option[Seq[Topping]] = None
) {
  override def toString() = {
    val separator = "----------------------------\n"
    s"Pizza type: ${pizzaType}\n" +
    separator +
    s"Size: ${size}\n" +
    separator +
    s"Crust: ${crustType}\n" +
    separator +
    s"Extra meat: ${extraMeat.getOrElse("None")}\n" +
    separator +
    s"Extra topping: ${extraTopping.getOrElse("None")}\n"
  }

  val extraMeatPrice: Double = extraMeat match {
    case Some(value) => value.map(_.price).sum
    case None => 0 
  }

  val extraToppingPrice: Double = extraTopping match {
    case Some(value) =>  value.map(_.price).sum
    case None => 0
  }

  private val percentage = size match {
    case Small => 0.9
    case Regular => 1
    case Large => 1.5
  }

  val price: Double = percentage * (extraMeatPrice + extraToppingPrice + pizzaType.price)
}