package water

enum Event:
  case Filtered(rawLiters: Long)
  case Backwash(liters: Long)
  case SaltAdded(kg: Long)
  case ModuleReplaced(newCapacity: Long)

enum Rejection:
  case NonPositiveAmount(amount: Long)
  case ResourceExhausted(requested: Long, remaining: Long)
  case BackwashExceedsProcessed(requested: Long, available: Long)
  case SaltTankOverflow(requested: Long, freeSpace: Long)

final case class Station(
    moduleCapacity: Long,
    moduleUsed: Long,
    totalRaw: Long,
    totalBackwash: Long,
    saltKg: Long,
    saltCapacityKg: Long,
    replacements: Int
):
  def remainingResource: Long = moduleCapacity - moduleUsed
  def cleanDelivered: Long = totalRaw - totalBackwash
  def lossPercent: Double =
    if totalRaw == 0 then 0.0 else totalBackwash.toDouble * 100 / totalRaw
  def mustStop: Boolean = remainingResource <= 0

object Station:
  def initial(moduleCapacity: Long, saltCapacityKg: Long = 100): Station =
    require(moduleCapacity > 0 && saltCapacityKg > 0, "ємності мають бути додатні")
    Station(moduleCapacity, 0, 0, 0, 0, saltCapacityKg, 0)

  import Event.*, Rejection.*

  def apply(s: Station, e: Event): Either[Rejection, Station] = e match
    case Filtered(n) =>
      if n <= 0 then Left(NonPositiveAmount(n))
      else if n > s.remainingResource then Left(ResourceExhausted(n, s.remainingResource))
      else Right(s.copy(moduleUsed = s.moduleUsed + n, totalRaw = s.totalRaw + n))
    case Backwash(n) =>
      val available = s.totalRaw - s.totalBackwash
      if n <= 0 then Left(NonPositiveAmount(n))
      else if n > available then Left(BackwashExceedsProcessed(n, available))
      else Right(s.copy(totalBackwash = s.totalBackwash + n))
    case SaltAdded(kg) =>
      val free = s.saltCapacityKg - s.saltKg
      if kg <= 0 then Left(NonPositiveAmount(kg))
      else if kg > free then Left(SaltTankOverflow(kg, free))
      else Right(s.copy(saltKg = s.saltKg + kg))
    case ModuleReplaced(cap) =>
      if cap <= 0 then Left(NonPositiveAmount(cap))
      else Right(s.copy(moduleCapacity = cap, moduleUsed = 0, replacements = s.replacements + 1))

  def replay(start: Station, events: List[Event]): (Station, List[(Event, Rejection)]) =
    events.foldLeft((start, List.empty[(Event, Rejection)])) { case ((st, errs), ev) =>
      apply(st, ev) match
        case Right(next) => (next, errs)
        case Left(r)     => (st, errs :+ (ev -> r))
    }
