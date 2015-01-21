package fundamentum

import simulacrum.typeclass

@typeclass trait Foldable[F[_]] extends Functor[F] {

  def foldRight[A, B](fa: F[A], initial: B)(f: (A, B) => B): B

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldRight(fa, mb.id)((a, b) => mb.append(f(a), b))

  def fold[A: Monoid](fa: F[A]): A =
    foldMap(fa)(x => x)
}
