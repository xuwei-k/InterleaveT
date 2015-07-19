package scalaz

import scalaprops._
import scalaz.std.anyVal._
import scalaz.std.stream._

object InterleaveTTest extends Scalaprops {

  val either = {
    type G[a] = Int \/ a
    type F[a] = InterleaveT[G, a]
    scalazlaws.applicative.all[F]
  }

  val eitherTMaybe = {
    type G[a] = EitherT[Maybe, Int, a]
    type F[a] = InterleaveT[G, a]
    scalazlaws.applicative.all[F]
  }

  val iList = scalazlaws.applicative.all[
    ({type l[a] = InterleaveT[IList, a]})#l
  ].andThenParam(Param.maxSize(5))

  val maybe = scalazlaws.applicative.all[
    ({type l[a] = InterleaveT[Maybe, a]})#l
  ]

  val maybeTMaybe = {
    type G[a] = MaybeT[Maybe, a]
    type F[a] = InterleaveT[G, a]
    scalazlaws.applicative.all[F]
  }

  val nel = scalazlaws.applicative.all[
    ({type l[a] = InterleaveT[NonEmptyList, a]})#l
  ].andThenParam(Param.maxSize(5))

  val oneAndMaybe = {
    type G[a] = OneAnd[Maybe, a]
    type F[a] = InterleaveT[G, a]
    scalazlaws.applicative.all[F]
  }

  val streamTMaybe = {
    type G[a] = StreamT[Maybe, a]
    type F[a] = InterleaveT[G, a]
    scalazlaws.applicative.all[F].andThenParam(Param.maxSize(5))
  }

}
