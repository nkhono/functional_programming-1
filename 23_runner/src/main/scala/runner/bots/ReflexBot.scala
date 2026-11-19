package runner.bots

import runner.*
import scala.util.Random

/**
 * Рефлексивний бот (поки що на випадковій моделі моторики).
 * Студенти замінюють цю реалізацію на чисті патерн-матчинги в Лекції 2.
 */
class ReflexBot(seed: Long = 202L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "ReflexBot"

  private val SearchDepth = 4

  private val preference: List[Action] =
    List(Action.KeepRunning, Action.Duck, Action.Jump, Action.MoveLeft, Action.MoveRight)

  private def apply(lane: Lane, action: Action): Option[(Lane, Stance)] = action match
    case Action.KeepRunning => Some((lane, Stance.Running))
    case Action.Jump        => Some((lane, Stance.Jumping))
    case Action.Duck        => Some((lane, Stance.Ducking))
    case Action.MoveLeft    => lane.left.map(l => (l, Stance.Running))
    case Action.MoveRight   => lane.right.map(l => (l, Stance.Running))

  private def survives(slice: TunnelSlice, lane: Lane, stance: Stance): Boolean =
    val low  = slice.hasObstacleAt(lane, Height.Low)
    val mid  = slice.hasObstacleAt(lane, Height.Mid)
    val high = slice.hasObstacleAt(lane, Height.High)
    stance match
      case Stance.Running => !low && !mid
      case Stance.Ducking => !low
      case Stance.Jumping => !high

  private def canSurvive(lane: Lane, slices: List[TunnelSlice], depth: Int): Boolean =
    if depth == 0 then true
    else slices match
      case Nil => true
      case s :: rest =>
        preference.exists { a =>
          apply(lane, a).exists { case (l, st) =>
            survives(s, l, st) && canSurvive(l, rest, depth - 1)
          }
        }

  override def decide(observation: Observation): Action =
    val lane   = observation.hero.lane
    val slices = observation.upcoming

    slices match
      case Nil => Action.KeepRunning
      case head :: tail =>
        val safeFar = preference.filter { a =>
          apply(lane, a).exists { case (l, st) =>
            survives(head, l, st) && canSurvive(l, tail, SearchDepth - 1)
          }
        }
        val safeNear = preference.filter { a =>
          apply(lane, a).exists { case (l, st) => survives(head, l, st) }
        }
        safeFar.headOption.orElse(safeNear.headOption).getOrElse(Action.KeepRunning)
