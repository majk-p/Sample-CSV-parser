import cats.implicits.*
import cats.kernel.Monoid
import cats.kernel.Semigroup
import magnolia1.AutoDerivation
import magnolia1.CaseClass.Param
import magnolia1.*

import java.time.Instant
import scala.util.Try

trait Decoder[A] {
  def decode(value: String): Either[Decoder.Failure, A]
}

object Decoder {

  def apply[A](implicit ev: Decoder[A]): Decoder[A] = ev

  case class Failure(reason: String)
 
  given Monoid[Failure]  with {
    def empty = Failure("")
    def combine(x: Failure, y: Failure): Failure = Failure(s"${x.reason}\n${y.reason}")
  }

  object implicits extends AutoDerivation[Decoder] {
    given decodeString: Decoder[String] = _.asRight

    given decodeNumber[A: Numeric]: Decoder[A] =
      value => 
        Numeric[A]
          .parseString(value)
          .toRight(Failure(s"Failed to parse number $value"))

    given decodeInstant: Decoder[Instant] =
      value =>
        Try(Instant.parse(value))
          .toEither
          .leftMap(error => Failure(error.getMessage))

    def join[T](
      ctx: CaseClass[Decoder, T]
    ): Decoder[T] = value => {
      val result = extractSubtype(ctx.typeInfo.short, value)
      validateNumberOfParameters(ctx, result) *>
        ctx
          .constructEither { param =>
            validateParameterIndex(param, result) *> param.typeclass.decode(result.values(param.index))
          }
          .leftMap(_.combineAll)
    }

    def split[T](
      ctx: SealedTrait[Decoder, T]
    ): Decoder[T] = param => 
      extractSubtype(ctx)(param)
        .toRight(Failure(s"No subtype of ${ctx.typeInfo.short} found in $param"))
        .flatMap { result => 
          val subtype = ctx.subtypes.find(_.typeInfo.short == result.subtype).get
          subtype.typeclass.decode(param)
        }

    private final case class ExtractionResult(
      subtype: String,
      values: List[String]
    )

    private def validateNumberOfParameters(
      ctx: CaseClass[Decoder, ?],
      result: ExtractionResult
    ) =
      Either.cond(
        result.values.length == ctx.parameters.length,
        right = (),
        left = Failure(s"too many parameters for ${ctx.typeInfo.short}, got ${result.values.length} expected ${ctx.parameters.length}")
      )

    private def validateParameterIndex(
      param: Param[Decoder, ?],
      result: ExtractionResult
    ) = Either
      .cond(
        result.values.length > param.index,
        right = (),
        left = Failure(s"Failed to parse ${result.subtype} due to missing value for ${param.label}")
      )
    
    private def extractSubtype[T](
      ctx: SealedTrait[Decoder, T]
    )(
      value: String
    ): Option[ExtractionResult] = {
      val subtypeNames = ctx.subtypes.map(_.typeInfo.short)
      val parts = value.split(separator).toList
      val subtype = parts.find(subtypeNames.contains(_))
      subtype.map{ subtypeName =>
        ExtractionResult(subtypeName, parts.filterNot(_ == subtypeName))
      }
      
    }
 
    private def extractSubtype(
      simpleName: String,
      value: String
    ): ExtractionResult =
      ExtractionResult(simpleName, value.split(separator).toList.filterNot(_ == simpleName))

  }

  private val separator = ","

  
}