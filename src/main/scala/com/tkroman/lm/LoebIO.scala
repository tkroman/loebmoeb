package com.tkroman.lm

import cats.effect.IO
import cats.implicits._
import cats.kernel.Semigroup
import cats.{Apply, Functor}

object LoebIO {
  // fa.fmap(f => f(loeb(fa)))
  // making lazy:
  // IO is an obvious choice (or rather eval but w/e)
  // f.fmap(fn => fn(loeb(f)))
  // now we want loeb to return io, so:
  // loeb: f[f[a]=>a]=>f[a]
  // changes to
  // loeb: f[f[a]=>a]=>io[f[a]]
  // but now impl doesn't satisfy types:
  // f.fmap(fn => fn(loeb(f): ERROR: io[a] instead of [a]))
  // so fn has to have a return type of io[a]
  // AND accept an io[f[a]]:
  // loeb: f[io[f[a]]=>io[a]]=>io[f[a]]
  // BUT then since we feed io[f[a]] into loeb itself,
  // loeb has to become a function of io[f[io[f[a]]=>io[a]]] into io[f[a]]
  // BUT then, since the only thing we can return in fmap now is io[?],
  // return type becomes io[f[io[a]]:
  // loeb: io[f[io[f[a]]=>io[a]]=>io[f[io[a]]
  // BUT NOW since we have this new return type
  // that we want/can onyl pass to loeb
  // we have to retrofit the argument fn type once again:
  // io[f[io[f[io[a]]]=>io[a]]=>io[f[io[a]]
  // now that we have everything in io, we basically got haskell :)
  def loeb[F[_]: Functor, A](
      f: IO[F[IO[F[IO[A]]] => IO[A]]]
  ): IO[F[IO[A]]] = {
    f.map(fn => fn.fmap(x => loeb(f).flatMap(l => x(IO.pure(l)))))
  }

  def moeb[A, B, C](f: ((A => IO[B]) => IO[B]) => C => IO[A])(c: IO[C]): IO[A] = {
    c.flatMap { _c =>
      f(g => moeb(f)(c).flatMap(a => g(a)))(_c)
    }
  }

  // yeah...
  // the idea is the same, retrofitting types backwards
  // note that this may also be done by simply saying
  // "let's substitute each type A with IO[A] now'
  // and then fucking around until it compiles
  def loebAsMoeb[F[_], A](fa: IO[F[IO[F[IO[A]]] => IO[A]]])(implicit F: Functor[F]): IO[F[IO[A]]] = {
    moeb[F[IO[A]], A, F[IO[F[IO[A]]] => IO[A]]](
      f => fs => IO(F.fmap(fs)(fn => f(_f => fn(IO(_f)))))
    )(fa)
  }

  def liftSemigroup[F[_]: Apply, A: Semigroup]: Semigroup[F[A]] =
    (x: F[A], y: F[A]) => Apply[F].map2(x, y)(Semigroup[A].combine)

  def op[A](xs: List[IO[A]])(implicit S: Semigroup[A]): IO[A] = {
    val sg: Semigroup[IO[A]] = liftSemigroup[IO, A]
    xs.tail.foldLeft(xs.head)(sg.combine)
  }

  def sum[A: Numeric](xs: List[IO[A]]) = op[A](xs)(Numeric[A].plus(_, _))
  def max[A: Numeric](xs: List[IO[A]]) = op[A](xs)(Ordering[A].max(_, _))

  def main(args: Array[String]): Unit = {
    // let's try:
    val xs: IO[List[IO[List[IO[Int]]] => IO[Int]]] = IO(
      List(
        (xs: IO[List[IO[Int]]]) => xs.flatMap(xs => IO(xs.length)),
        (xs: IO[List[IO[Int]]]) => xs.flatMap(xs => xs.last.map(_ + 1)),
        (xs: IO[List[IO[Int]]]) => IO(100),
        (xs: IO[List[IO[Int]]]) => xs.flatMap(xs => sum(xs.takeRight(2))),
        (xs: IO[List[IO[Int]]]) => xs.flatMap(xs => max(xs.take(3))),
        (xs: IO[List[IO[Int]]]) => xs.flatMap(xs => xs.head.map(_ + 1))
      )
    )
    println(loeb[List, Int](xs).flatMap(_.traverse(identity)).unsafeRunSync())
    println(loebAsMoeb[List, Int](xs).flatMap(_.traverse(identity)).unsafeRunSync())
  }
}
