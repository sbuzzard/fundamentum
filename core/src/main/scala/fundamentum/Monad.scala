package fundamentum

import simulacrum.{ typeclass, op }

@typeclass trait Monad[F[_]] extends Applicative[F] {

  @op(">>=", alias = true)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](fa: F[A])(f: F[A => B]): F[B] =
    flatMap(f)(map(fa))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => insert(f(a)))
}
