val x = 52

"Hello world!".toUpperCase

val t = 2

class Cat(name: String, colour: String, food: String, carateristic)
class Carateristics(...)

new Cat(name = Oswald,Black,Milk, New Carateristics(....))

object ChipShop {
  def willServe(cat:Cat) = cat.food == "Chips"
}

class Counter(number: Int){
  def inc(counter: Counter): Counter = new Counter(number + 1)
  def dec: Counter = new Counter(number -1)
  def count = number
}
new Counter(10).inc.dec.inc.inc.count

class Person(name: String, surname: String)

object Person{
  def apply(fullName: String) = {
    val nameArray = "John Doe".split(" ")
    new Person(nameArray(0), nameArray(1))
  }
}