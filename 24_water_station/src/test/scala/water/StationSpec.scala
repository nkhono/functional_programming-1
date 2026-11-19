package water

import org.scalatest.funsuite.AnyFunSuite
import Event.*, Rejection.*

class StationSpec extends AnyFunSuite:

  test("сценарій: старт -> фільтрація -> промивка -> сіль -> заміна -> підсумки"):
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

  test("ресурс вичерпано: сигнал зупинки й відмова у фільтрації"):
    val s = Station.apply(Station.initial(500), Filtered(500)).toOption.get
    assert(s.remainingResource == 0 && s.mustStop)
    assert(Station(s, Filtered(1)) == Left(ResourceExhausted(1, 0)))

  test("гранична: фільтрація понад залишок ресурсу відхиляється, стан не змінюється"):
    val s0 = Station.initial(100)
    assert(Station(s0, Filtered(101)) == Left(ResourceExhausted(101, 100)))
    val (s1, errs) = Station.replay(s0, List(Filtered(101)))
    assert(s1 == s0 && errs.size == 1)

  test("промивка не може перевищити пропущену воду; від'ємні/нульові обсяги відхиляються"):
    val s = Station.apply(Station.initial(100), Filtered(50)).toOption.get
    assert(Station(s, Backwash(51)) == Left(BackwashExceedsProcessed(51, 50)))
    assert(Station(s, Filtered(0)) == Left(NonPositiveAmount(0)))
    assert(Station(s, Backwash(-5)) == Left(NonPositiveAmount(-5)))

  test("сіль: переповнення бака відхиляється"):
    val s = Station.initial(100, saltCapacityKg = 20)
    assert(Station(s, SaltAdded(21)) == Left(SaltTankOverflow(21, 20)))

  test("відсоток втрат без води дорівнює нулю"):
    assert(Station.initial(100).lossPercent == 0.0)
