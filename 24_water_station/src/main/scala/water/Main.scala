package water

import Event.*

@main def run(): Unit =
  val (s, errs) = Station.replay(
    Station.initial(1000),
    List(Filtered(400), Backwash(40), SaltAdded(10), Filtered(500), Backwash(60), Filtered(200), ModuleReplaced(2000), Filtered(100))
  )
  println(s)
  println(f"Залишок: ${s.remainingResource} л, чиста: ${s.cleanDelivered} л, втрати: ${s.lossPercent}%.2f%%, стоп: ${s.mustStop}")
  println(s"Відхилено: $errs")
