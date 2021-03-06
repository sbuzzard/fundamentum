package fundamentum

import simulacrum.typeclass

/**
 * Describes type constructors that support the `map` method such that:
 *  - `map(fa)(identity) == fa`
 *  - `map(map(fa)(f))(g) == xmap(fa)(f andThen g)`
 *
 * The name is short for "covariant functor".
 *
 * Note that every functor is an exponential functor, where `xmap` is implemented in
 * terms of `map` by ignoring the `B => A` function.
 */
@typeclass trait Functor[F[_]] extends Exponential[F] {

  /**
   * Converts the supplied `F[A]` in to an `F[B]` using the supplied `A => B`.
   */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  override def xmap[A, B](fa: F[A])(f: A => B, g: B => A): F[B] =
    map(fa)(f)

  /** Lifts the supplied function in to the `F` type constructor. */
  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  /** Replaces the `A` value in `F[A]` with the supplied value. */
  def as[A, B](fa: F[A], b: B): F[B] =
    map(fa)(_ => b)
}
