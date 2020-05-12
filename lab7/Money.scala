package money

sealed trait Currency
case object USD extends Currency
case object PLN extends Currency
case object EUR extends Currency

sealed trait Shortcut
case object $ extends Shortcut
case object zl extends Shortcut
case object E  extends Shortcut

object Type {
  import scala.language.implicitConversions

  val conversion: Map[(Currency, Currency), BigDecimal] = Map(
    (EUR, PLN) -> 4.5683,
    (EUR, USD) -> 1.09352,
    (USD, EUR) -> 0.914475,
    (USD, PLN) -> 4.17564,
    (PLN, USD) -> 0.239484,
    (PLN, EUR) -> 0.2189
  )

  val shortcutConversion: Map[Shortcut, Currency] = Map(
    $ -> USD,
    zl -> PLN,
    E -> EUR
  )

  implicit def shortcut2Currency(short: Shortcut) = short match {
    case s: Shortcut => shortcutConversion(short)
    case _ => throw new Exception("Unkown argument!")
  }

  case class CurrencyConverter(
    conversion: Map[(Currency, Currency), BigDecimal]) {
      def convert(from: Currency, to: Currency): BigDecimal = conversion((from, to))
    }

  case class Money(amount: BigDecimal, currency: Currency)(implicit
    currencyConverter: CurrencyConverter) {
      override def toString() = s"${amount}(${currency})"
      //SIP nice usage of String Interpolation

      def as(currency: Currency): Money = new Money(currencyConverter.convert(this.currency, currency) * amount, currency)

      private def changeCurrency(currency: Currency) = currency match {
        case this.currency => this
        case _ => this.as(currency)
      }
      
      private def converted(m: Money) = m.changeCurrency(currency).amount

      def +(other: Money): Money = new Money(amount + converted(other), currency)

      def -(other: Money): Money = new Money(amount - converted(other), currency)

      def *(mult: Double): Money = new Money(amount * mult, currency)

      def >(other: Money): Boolean = amount > converted(other)

      def <(other: Money): Boolean = amount < converted(other)

      def ==(other: Money): Boolean = amount == converted(other)

      def !=(other: Money): Boolean = amount != converted(other)

      def >=(other: Money): Boolean = amount >= converted(other)

      def <=(other: Money): Boolean = amount <= converted(other)
    }

  class NumberWrapper(n: BigDecimal) {
    def apply(currency: Currency) = Money(n, currency)(new CurrencyConverter(conversion))
  }

  implicit def number2Wrapper(n: Double) = new NumberWrapper(n)
  implicit def big2Wrapper[A <: BigDecimal](n: A) = new NumberWrapper(n)
}

