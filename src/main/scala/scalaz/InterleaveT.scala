package scalaz

import scalaprops.Gen
import scalaz.InterleaveT._

object InterleaveT extends InterleaveTInstances {

  implicit def interleaveTApplicative[F[_]](implicit F0: Functor[F], F1: Plus[F]): Applicative[({type l[a] = InterleaveT[F, a]})#l] =
    new Applicative[({type l[a] = InterleaveT[F, a]})#l] with InterleaveTFunctor[F] {
      import scalaz.syntax.std.function2._

      override def F = F0

      override def point[A](a: => A) =
        NoMore(a)

      override def ap[A, B](fa: => InterleaveT[F, A])(f: => InterleaveT[F, A => B]) =
        apply2(fa, f)((x, y) => y(x))

      override def apply2[A, B, C](aa: => InterleaveT[F, A], bb: => InterleaveT[F, B])(f: (A, B) => C) =
        (aa, bb) match {
          case (oa @ Continue(a, fa), ob @ Continue(b, fb)) =>
            Continue(
              f(a, b),
              F1.plus(
                F.map(fa)(apply2(ob, _)(f.flip)),
                F.map(fb)(apply2(oa, _)(f))
              )
            )
          case (NoMore(a), ob) =>
            map(ob)(f(a, _))
          case (oa, NoMore(b)) =>
            map(oa)(f(_, b))
          case (oa, ob) =>
            Terminate(f(oa.result, ob.result))
        }
    }

  implicit def interleaveTEqual[F[_], A](implicit
    E1: Equal[A],
    E2: shapeless.Lazy[Equal[F[InterleaveT[F, A]]]]
  ): Equal[InterleaveT[F, A]] = Equal.equal{
    case (Continue(a, fa), Continue(b, fb)) =>
      E1.equal(a, b) && E2.value.equal(fa, fb)
    case (NoMore(a), NoMore(b)) =>
      E1.equal(a, b)
    case (Terminate(a), Terminate(b)) =>
      E1.equal(a, b)
    case (_, _) =>
      false
  }

  implicit def interleaveTGen[F[_], A](implicit
    G1: Gen[A],
    G2: shapeless.Lazy[Gen[F[InterleaveT[F, A]]]]
  ): Gen[InterleaveT[F, A]] =
    Gen.oneOf(
      G1.map(Terminate(_)),
      G1.map(NoMore(_)),
      Apply[Gen].apply2(G1, G2.value)(Continue(_, _))
    )

  final case class Terminate[F[_], A](result: A) extends InterleaveT[F, A]
  final case class NoMore[F[_], A](result: A) extends InterleaveT[F, A]
  final case class Continue[F[_], A](result: A, rest: F[InterleaveT[F, A]]) extends InterleaveT[F, A]
}

sealed abstract class InterleaveTInstances {
  implicit final def interleaveTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type l[a] = InterleaveT[F, a]})#l] =
    new InterleaveTFunctor[F]{
      def F = F0
    }
}

sealed abstract class InterleaveT[F[_], A] extends Product with Serializable {

  def result: A

  final def map[B](f: A => B)(implicit F: Functor[F]): InterleaveT[F, B] =
    this match {
      case Terminate(a) =>
        Terminate(f(a))
      case NoMore(a) =>
        NoMore(f(a))
      case Continue(a, rest) =>
        Continue(f(a), F.map(rest)(_.map(f)))
    }

}

private trait InterleaveTFunctor[F[_]] extends Functor[({type l[a] = InterleaveT[F, a]})#l] {
  protected[this] implicit def F: Functor[F]

  override final def map[A, B](fa: InterleaveT[F, A])(f: A => B) =
    fa map f
}
