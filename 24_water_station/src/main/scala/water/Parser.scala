package water

import Event.*
import ParseError.*

// ---------- Smart constructors for contract field types ----------

final case class Liters private[water] (value: BigDecimal)
final case class SaltKg private[water] (value: BigDecimal)

object Quantity:

  /** Decimal with scale <= 2 (contract: scale 2), strictly positive and within [min, max]. */
  private def parse(line: Int, field: String, raw: String, min: BigDecimal, max: BigDecimal): Either[ParseError, BigDecimal] =
    if !raw.matches("-?\\d+(\\.\\d{1,2})?") then Left(InvalidQuantity(line, field, raw))
    else
      val v = BigDecimal(raw)
      if v <= 0 || v < min || v > max then Left(OutOfRange(line, field, raw))
      else Right(v)

  def liters(line: Int, field: String, raw: String, min: BigDecimal, max: BigDecimal): Either[ParseError, Liters] =
    parse(line, field, raw, min, max).map(Liters(_))

  def salt(line: Int, field: String, raw: String, min: BigDecimal, max: BigDecimal): Either[ParseError, SaltKg] =
    parse(line, field, raw, min, max).map(SaltKg(_))

// ---------- Parser: CSV line -> Either[ParseError, Event] ----------

object Parser:

  // ranges from n04_variants.draft.json (variant 24)
  private val FilterRange = (BigDecimal("15.00"), BigDecimal("400.00"))
  private val FlushRange  = (BigDecimal("8.00"), BigDecimal("120.00"))
  private val SaltRange   = (BigDecimal("1.50"), BigDecimal("12.00"))

  def parseLine(lineNo: Int, line: String): Either[ParseError, Event] =
    val cols = line.split(",", -1)
    if cols.length != 5 then
      if cols.headOption.exists(_.isEmpty) then Left(EmptyEvent(lineNo))
      else Left(MalformedRow(lineNo, cols.length))
    else
      cols(0) match
        case ""                  => Left(EmptyEvent(lineNo))
        case "FILTER_BATCH"      => Quantity.liters(lineNo, "liters", cols(1), FilterRange._1, FilterRange._2).map(FilterBatch(_))
        case "FLUSH_DRAIN"       => Quantity.liters(lineNo, "liters", cols(1), FlushRange._1, FlushRange._2).map(FlushDrain(_))
        case "REGENERATE"        => Quantity.salt(lineNo, "salt_kg", cols(1), SaltRange._1, SaltRange._2).map(Regenerate(_))
        case "CARTRIDGE_REPLACE" => Right(CartridgeReplace)
        case other               => Left(UnknownEvent(lineNo, other))

  /** Batch processing: (errors, events). Header line is skipped. */
  def parseAll(lines: Iterator[String]): (List[ParseError], List[Event]) =
    lines.zipWithIndex
      .drop(1)
      .map((l, i) => parseLine(i + 1, l))
      .toList
      .partitionMap(identity)

  def parseFile(path: String): (List[ParseError], List[Event]) =
    val src = scala.io.Source.fromFile(path, "UTF-8")
    try parseAll(src.getLines())
    finally src.close()
