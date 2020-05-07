import pizzeria._
import orders._

object Main extends App {
  val pizza1 = Pizza(
    Margarita,
    Large,
    Thin,
    Option(List(Salami)),
    Option(List(Ketchup, Garlic))
  )

  val pizza2 = Pizza(
    Funghi,
    Small,
    Thin,
    Option(List(Salami))
  )

  require(pizza1.price == 10.5)
  require(pizza2.price == 7.2)

  //println(pizza1)

  val order1 = Order(
    "Dawid",
    "Plonsk",
    "792143248",
    Option(List(pizza1)),
    Option(List(Lemonade)),
    Option(Student),
    Option("please call when you will be under my doors")
  )

  val order2 = Order(
    "Sandra",
    "Naruszewo",
    "792143248",
    Option(List(pizza1, pizza2, pizza1)),
    Option(List(Lemonade, Lemonade)),
    Option(Senior)
  )

  //println(order2)
  require(order1.extraMeatPrice.getOrElse(0) == 1)
  require(order1.extraToppingPrice.getOrElse(0) == 1)
  require(order1.pizzasPrice.getOrElse(0) == 10.5)
  require(order1.drinksPrice.getOrElse(0) == 2)
  require(order1.priceByType(Margarita).getOrElse(0) == 10.5)
  require(order1.price == 11.875)

  require(order2.extraMeatPrice.getOrElse(0) == 3)
  require(order2.extraToppingPrice.getOrElse(0) == 2)
  require(order2.pizzasPrice.getOrElse(0) == 28.2)
  require(order2.drinksPrice.getOrElse(0) == 4)
  require(order2.priceByType(Margarita).getOrElse(0) == 21)
  require(order2.priceByType(Funghi).getOrElse(0) == 7.2)
  require(order2.price == 29.946000000000005)
}