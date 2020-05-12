object Main extends App {
  import money._
  import money.Type._

  val sum1: Money = 100.01(USD) + 200(EUR)
  require(sum1.amount == 318.714)

  val sum2: Money = 100.01(zl) + 200($)
  require(sum2.amount == 935.138)

  val sum3: Money = 5(zl) + 3(PLN) + 20.5(USD)
  require(sum3.amount == 93.60062)

  val sub: Money = 300.01(USD) - 200(EUR)
  require(sub.amount == 81.306)

  val mult1: Money = 30(zl) * 20
  require(mult1.amount == 600)

  val mult2: Money = 20($) * 11
  require(mult2.amount == 220)

  val conv1: Money = 150.01(USD) as PLN
  require(conv1.amount == 626.3877564)

  val conv2: Money = 120.01(USD) as E
  require(conv2.amount == 109.74614475)

  val compare1 = 300.30(USD) > 200(E)
  require(compare1)

  val compare2 = 300.30($) < 200(EUR)
  require(!compare2)

  val compare3 = 200(PLN) < 100(EUR)
  require(compare3)
}