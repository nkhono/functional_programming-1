package water

@main def run(args: String*): Unit =
  val path = args.headOption.getOrElse("testdata/variant_24_water_filtration.csv")
  val (errors, events) = Parser.parseFile(path)
  val (s, rejected)    = Station.replay(Station.initial(), events)

  println(s"CSV: $path")
  println(s"Valid events: ${events.size}, parse errors (Left): ${errors.size}")
  println(errors.groupBy(_.getClass.getSimpleName).view.mapValues(_.size).toMap)
  println(s"Rejected by domain rules: ${rejected.size}")
  println(s"Remaining resource: ${s.remainingResource} L")
  println(s"Clean water delivered: ${s.cleanDelivered} L")
  println(s"Loss percent: ${s.lossPercent}%")
  println(s"Salt added: ${s.saltKg} kg, cartridge replacements: ${s.replacements}")
  println(s"Must stop filtration: ${s.mustStop}")
  println(s"Invariant holds: ${s.invariantHolds}")
