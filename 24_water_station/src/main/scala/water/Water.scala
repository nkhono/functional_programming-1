package water

/** Variant 24: automatic water purification station (contract: water_filtration). */

// ---------- Domain events ----------

enum Event:
  case FilterBatch(liters: Liters)
  case FlushDrain(liters: Liters)
  case Regenerate(salt: SaltKg)
  case CartridgeReplace

// ---------- Errors (parse-layer + domain-layer) ----------

enum ParseError:
  case EmptyEvent(line: Int)
  case UnknownEvent(line: Int, code: String)
  case MalformedRow(line: Int, columns: Int)
  case InvalidQuantity(line: Int, field: String, raw: String)
  case OutOfRange(line: Int, field: String, raw: String)

enum Rejection:
  case ResourceExhausted(requested: BigDecimal, remaining: BigDecimal)
  case FlushExceedsProcessed(requested: BigDecimal, available: BigDecimal)

// ---------- System state ----------

final case class Station(
    cartridgeCapacity: BigDecimal,
    cartridgeUsed: BigDecimal,
    totalRaw: BigDecimal,
    totalFlushed: BigDecimal,
    saltKg: BigDecimal,
    replacements: Int
):

  /** Remaining filter resource (liters until replacement). */
  def remainingResource: BigDecimal = cartridgeCapacity - cartridgeUsed

  /** Total clean water delivered to consumers. */
  def cleanDelivered: BigDecimal = totalRaw - totalFlushed

  /** Percentage of technological water losses on flushing. */
  def lossPercent: BigDecimal =
    if totalRaw == 0 then BigDecimal(0)
    else (totalFlushed * 100 / totalRaw).setScale(2, BigDecimal.RoundingMode.HALF_UP)

  /** Invariant stop_filtration_when_cartridge_depleted: filtration must stop. */
  def mustStop: Boolean = remainingResource <= 0

  /** Invariant check: used resource never exceeds capacity, flushes never exceed raw. */
  def invariantHolds: Boolean =
    cartridgeUsed >= 0 && cartridgeUsed <= cartridgeCapacity &&
      totalFlushed <= totalRaw

object Station:

  import Event.*
  import Rejection.*

  /** Contract constant: cartridgeCapacityLiters. */
  val CartridgeCapacityLiters: BigDecimal = BigDecimal(5000)

  def initial(capacity: BigDecimal = CartridgeCapacityLiters): Station =
    Station(capacity, 0, 0, 0, 0, 0)

  def step(s: Station, e: Event): Either[Rejection, Station] =
    e match

      case FilterBatch(l) =>
        if l.value > s.remainingResource then
          Left(ResourceExhausted(l.value, s.remainingResource)) // forbidden: depleted cartridge
        else
          Right(s.copy(cartridgeUsed = s.cartridgeUsed + l.value, totalRaw = s.totalRaw + l.value))

      case FlushDrain(l) =>
        val available = s.totalRaw - s.totalFlushed
        if l.value > available then Left(FlushExceedsProcessed(l.value, available))
        else Right(s.copy(totalFlushed = s.totalFlushed + l.value))

      case Regenerate(kg) =>
        Right(s.copy(saltKg = s.saltKg + kg.value))

      case CartridgeReplace =>
        Right(s.copy(cartridgeUsed = 0, replacements = s.replacements + 1))

  /** Fold over valid events; forbidden operations are collected and leave the state unchanged. */
  def replay(start: Station, events: Iterable[Event]): (Station, List[(Event, Rejection)]) =
    val (st, rejected) = events.foldLeft((start, List.empty[(Event, Rejection)])) {
      case ((state, rej), ev) =>
        step(state, ev) match
          case Right(next) => (next, rej)
          case Left(r)     => (state, (ev -> r) :: rej)
    }
    (st, rejected.reverse)
