package com.tkroman.lm

import cats.Functor
import cats.instances.list._
import cats.syntax.functor._

object LoebNaive {
  // stmt: if (x is provable then x) is provable, then x is provable
  // i.e. p(p(x)->x)->p(x)
  // replacing p with A[_]:
  // A[A[X]=>X]=>A[X]
  // simplest way to get into A[_] is fmap from Functor:
  def loeb[F[_]: Functor, A](fa: F[F[A] => A]): F[A] = {
    // impl: follow the types:
    // loeb: A[A[X]=>X]=>A[X]
    // the only thing we have is `A[X]=>X` from inside A (via fmap)
    // and fmap needs a return to match A[X], so
    // fa.fmap(`a[x]->x` => f(`a[x]->x`))
    // where f: (A[X]=>X)=>X
    // what do we have of type A[X] in the scope?
    // loeb itself:
    // fa.fmap(a => a(loeb(g)))
    // where g has to have original argument type,
    // and since the only such value in scope is fa,
    // let's just do that
    fa.fmap(f => f(loeb(fa)))
  }

  // abstracting over fmap:
  //
  // remember:
  // loeb(fa) = fa.fmap(f => f(loeb(fa)))
  //
  // loeb :: Functor f => f (f a -> a) -> f a
  // loeb x = fmap (\a -> a (loeb x)) x
  // loeb x = go where go = fmap ($ go) x
  // loeb x = go where go = fmap (\f -> f go) x
  //
  // now:
  // moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
  // moeb f x = go where go = f ($ go) x
  // moeb f x = f (\g -> g (moeb f x)) x
  // f :: (((a -> b) -> b) -> c -> a)
  //
  // (moeb fmap) :: f (f b -> b) -> f b
  // fmap:: (a -> b) -> f a => f b
  //
  // moeb fmap = fmap (\g -> g (moeb fmap x))
  //
  def moeb[A, B, C](f: ((A => B) => B) => (C => A))(c: C): A = {
    f { (g: A => B) =>
      val a: A = moeb(f)(c)
      val b: B = g(a)
      b
    }(c)
  }

  def loebAsMoeb[F[_], A](fa: F[F[A] => A])(implicit F: Functor[F]): F[A] = {
    moeb[F[A], A, F[F[A] => A]] {
      (fn: (F[A] => A) => A) =>
        (fns: F[F[A] => A]) =>
          F.fmap(fns)(fn)
    }(fa)
  }

  def main(args: Array[String]): Unit = {
    // let's try:
    def xs[A] = List(
      (xs: List[A]) => xs.length,
      (xs: List[A]) => xs.head
    )
    //Exception in thread "main" java.lang.StackOverflowError
    //	at com.tkroman.lm.LoebNaive$.$anonfun$loeb$1(LoebNaive.scala:28)
    println(loebAsMoeb(xs))
  }
}
