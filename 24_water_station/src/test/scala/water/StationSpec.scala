package water

import org.scalatest.funsuite.AnyFunSuite
import Event.*, Rejection.*

class StationSpec extends AnyFunSuite:

  test("scenario: start -> filtering -> backwash -> salt -> module replacement -> totals"):
    val (s, errs) = Station.replay(
      Station.initial(1000),
      List(Filtered(400), Backwash(40), SaltAdded(10), Filtered(500), Backwash(60),
           ModuleReplaced(2000), Filtered(100))
    )
    assert(errs.isEmpty)
    assert(s.totalRaw == 1000)
    assert(s.cleanDelivered == 900)
    assert(s.lossPercent == 10.0)
    assert(s.saltKg == 10)
    assert(s.replacements == 1)
    assert(s.remainingResource == 1900)
    assert(!s.mustStop)

  test("resource exhausted: stop signal is raised and filtering is refused"):
    val s = Station.apply(Station.initial(500), Filtered(500)).toOption.get
    assert(s.remainingResource == 0 && s.mustStop)
    assert(Station(s, Filtered(1)) == Left(ResourceExhausted(1, 0)))

  test("edge case: filtering beyond remaining resource is rejected and state is unchanged"):
    val s0 = Station.initial(100)
    assert(Station(s0, Filtered(101)) == Left(ResourceExhausted(101, 100)))
    val (s1, errs) = Station.replay(s0, List(Filtered(101)))
    assert(s1 == s0 && errs.size == 1)

  test("backwash cannot exceed filtered water; zero or negative amounts are rejected"):
    val s = Station.apply(Station.initial(100), Filtered(50)).toOption.get
    assert(Station(s, Backwash(51)) == Left(BackwashExceedsProcessed(51, 50)))
    assert(Station(s, Filtered(0)) == Left(NonPositiveAmount(0)))
    assert(Station(s, Backwash(-5)) == Left(NonPositiveAmount(-5)))

  test("salt tank overflow is rejected"):
    val s = Station.initial(100, saltCapacityKg = 20)
    assert(Station(s, SaltAdded(21)) == Left(SaltTankOverflow(21, 20)))

  test("loss percent is zero when no water was filtered"):
    assert(Station.initial(100).lossPercent == 0.0)
