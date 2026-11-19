import scala.collection.parallel.CollectionConverters._

object Workshop:

  // =========================================================================
  // ЗАВДАННЯ 2: РЕФАКТОРИНГ В ЧИСТЕ ФП (EXPRESSION-ORIENTED)
  // =========================================================================
  
  sealed trait RiskLevel
  case object HighRisk extends RiskLevel
  case object MediumRisk extends RiskLevel
  case object LowRisk extends RiskLevel

  def categorize(amount: Double): RiskLevel =
    
    if (amount > 80.0) HighRisk
    else if (amount > 50.0) MediumRisk
    else LowRisk

  def getMultiplier(level: RiskLevel): Double =
    level match {
      case HighRisk => 1.5
      case MediumRisk => 1.2
      case LowRisk => 1.0
    }

  @main def runWorkshop(): Unit =
    println("=== Практика 00: Вмикаємо мозок ===")

    // Генеруємо 100 транзакцій для швидких тестів (від 1 до 100)
    val data: Vector[Double] = (1 to 100).toVector.map(_.toDouble)


    // 1. ДЕМО RACE CONDITION: кілька потоків одночасно змінюють спільну змінну var.
    // Операція `totalRisk += x` не атомарна (читання -> додавання -> запис),
    // тому частина оновлень губиться, і результат різний щоразу.
    var totalRisk = 0.0
    data.par.foreach(amount => totalRisk += amount)
    println(s"[Race condition] Некоректна сума (var + par.foreach): $totalRisk")
    println(s"[Правильна сума] ${data.sum}")

    // 4. Побудуйте чистий конвеєр обчислень. 
    // Відфільтруйте транзакції (наприклад, залишіть лише > 50.0), 
    // розрахуйте для них фінальний ризик (transactionId * multiplier) і знайдіть суму.
    
    
    val finalRiskSum = data.par
      .filter(_ > 50.0)
      .map(amount => amount * getMultiplier(categorize(amount)))
      .sum

    println(s"Сума фінального ризику: $finalRiskSum")
    

  def main(args: Array[String]): Unit = runWorkshop()
