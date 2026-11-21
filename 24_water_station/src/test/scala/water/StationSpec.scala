package water

import org.scalatest.funsuite.AnyFunSuite
import Event.*, Rejection.*, ParseError.*

class StationSpec extends AnyFunSuite:

  private val csvPath = "testdata/variant_24_water_filtration.csv"

  private def ok(line: String): Event = Parser.parseLine(1, line).toOption.get

  test("scenario: manual rows -> filter, flush, regenerate, replace -> totals"):
    val rows = List("FILTER_BATCH,400.00,,,", "FLUSH_DRAIN,40.00,,,", "REGENERATE,5.50,,,",
                    "FILTER_BATCH,100.00,,,", "FLUSH_DRAIN,10.00,,,", "CARTRIDGE_REPLACE,,,,", "FILTER_BATCH,50.00,,,")
    val (s, rej) = Station.replay(Station.initial(), rows.map(ok))
    assert(rej.isEmpty)
    assert(s.totalRaw == BigDecimal(550))
    assert(s.cleanDelivered == BigDecimal(500))
    assert(s.lossPercent == BigDecimal("9.09"))
    assert(s.saltKg == BigDecimal("5.5"))
    assert(s.replacements == 1)
    assert(s.remainingResource == BigDecimal(4950))
    assert(!s.mustStop && s.invariantHolds)

  test("invariant: filtration refused when cartridge resource is depleted"):
    val s0 = Station.initial(BigDecimal(100))
    val s1 = Station.step(s0, ok("FILTER_BATCH,100.00,,,")).toOption.get
    assert(s1.remainingResource == 0 && s1.mustStop)
    assert(Station.step(s1, ok("FILTER_BATCH,15.00,,,")) == Left(ResourceExhausted(BigDecimal(15), BigDecimal(0))))
    val s2 = Station.step(s1, CartridgeReplace).toOption.get
    assert(!s2.mustStop)

  test("flush cannot exceed processed water; state unchanged on rejection"):
    val (s, rej) = Station.replay(Station.initial(), List(ok("FILTER_BATCH,20.00,,,"), ok("FLUSH_DRAIN,30.00,,,")))
    assert(s.totalRaw == BigDecimal(20) && s.totalFlushed == 0 && rej.size == 1)

  test("parser: invalid rows become Left without exceptions"):
    assert(Parser.parseLine(1, "NOT_AN_EVENT,10.00,,,") == Left(UnknownEvent(1, "NOT_AN_EVENT")))
    assert(Parser.parseLine(2, ",10.00,,,") == Left(EmptyEvent(2)))
    assert(Parser.parseLine(3, "FILTER_BATCH,20.00") == Left(MalformedRow(3, 2)))
    assert(Parser.parseLine(4, "FILTER_BATCH,20.00,,,,EXTRA") == Left(MalformedRow(4, 6)))
    assert(Parser.parseLine(5, "FILTER_BATCH,abc,,,") == Left(InvalidQuantity(5, "liters", "abc")))
    assert(Parser.parseLine(6, "FILTER_BATCH,1,5,,,").isLeft)
    assert(Parser.parseLine(7, "FILTER_BATCH,-1.000,,,") == Left(InvalidQuantity(7, "liters", "-1.000")))
    assert(Parser.parseLine(8, "FILTER_BATCH,0,,,") == Left(OutOfRange(8, "liters", "0")))
    assert(Parser.parseLine(9, "FLUSH_DRAIN,-0.01,,,") == Left(OutOfRange(9, "liters", "-0.01")))
    assert(Parser.parseLine(10, "FLUSH_DRAIN,500.00,,,").isLeft)
    assert(Parser.parseLine(11, "REGENERATE,,,,") == Left(InvalidQuantity(11, "salt_kg", "")))

  test("real CSV fragment: first 2000 lines of variant_24_water_filtration.csv"):
    val src   = scala.io.Source.fromFile(csvPath, "UTF-8")
    val lines = try src.getLines().take(2000).toList finally src.close()
    val (errors, events) = Parser.parseAll(lines.iterator)
    assert(errors.nonEmpty && events.nonEmpty)
    assert(errors.size + events.size == 1999)
    val (s, _) = Station.replay(Station.initial(), events)
    assert(s.invariantHolds)

  test("full CSV (500000 rows): ~5% invalid, invariant holds after fold"):
    val (errors, events) = Parser.parseFile(csvPath)
    assert(errors.size + events.size == 500000)
    val rate = errors.size.toDouble / 500000
    assert(rate > 0.03 && rate < 0.07)
    val (s, _) = Station.replay(Station.initial(), events)
    assert(s.invariantHolds)
