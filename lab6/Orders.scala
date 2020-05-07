package orders

import pizzeria._

class Order(
  name: String,
  address: String,
  phone: String,
  pizzas: Option[Seq[Pizza]],
  drinks: Option[Seq[Drink]],
  discount: Option[Discount] = None,
  specialInfo: Option[String] = None
) {
  override def toString() = {
    val separator = "-------------------------------------------------------------------\n"
    s"Name: ${name}\n" +
    separator +
    s"Address: ${address}\n" +
    separator +
    s"Phone: ${phone}\n" +
    separator +
    s"Pizzas: ${pizzas}\n" +
    separator +
    s"Drinks: ${drinks.getOrElse("None")}\n" +
    separator +
    s"Discount: ${discount.getOrElse("None")}\n" +
    separator +
    s"Message: ${specialInfo.getOrElse("None")}\n"
  }
  def extraMeatPrice: Option[Double] = pizzas match {
    case Some(value) => Option(value.map(_.extraMeatPrice).sum) 
    case None => None
  }

  def extraToppingPrice: Option[Double] = pizzas match {
    case Some(value) => Option(value.map(_.extraToppingPrice).sum) 
    case None => None
  }

  def pizzasPrice: Option[Double] = pizzas match {
    case Some(value) => Option(value.map(_.price).sum) 
    case None => None
  }

  def drinksPrice: Option[Double] = drinks match {
    case Some(value) => Option(value.map(_.price).sum) 
    case None => None
  }

  private val drinksPriceValue: Double = drinksPrice.getOrElse(0)
  private val pizzasPriceValue: Double = pizzasPrice.getOrElse(0)

  def priceByType(pizzaType: PizzaType): Option[Double] = pizzas match {
    case Some(value) => Option(value.filter(_.pizzaType == pizzaType).map(_.price).sum)
    case None => None
  }

  private val studentDiscount = 0.95
  private val seniorDiscount = 0.93
  
  private val priceWithoutDiscount: Double = drinksPriceValue + pizzasPriceValue 

  val price: Double = discount match {
    case Some(value) => value match {
      case Senior => priceWithoutDiscount * seniorDiscount
      case Student => priceWithoutDiscount * studentDiscount
    }
    case None => priceWithoutDiscount
  }
}

object Order {
  def apply(
    name: String,
    address: String,
    phone: String,
    pizzas: Option[Seq[Pizza]],
    drinks: Option[Seq[Drink]],
    discount: Option[Discount] = None,
    specialInfo: Option[String] = None
  ) = {
    require(phone.matches("[0-9]{9}") && (drinks != None || pizzas != None))
    new Order(name, address, phone, pizzas, drinks, discount, specialInfo)   
  }
}