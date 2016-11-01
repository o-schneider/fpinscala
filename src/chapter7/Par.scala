package chapter7

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(List()))((p, acc) => map2(p, acc)((a, l) => a :: l))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldRight[Par[List[A]]](unit(List()))((a, acc) => map2(asyncF(f)(a), acc)((keep, l) => if (keep) a :: l else l))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond)(b => if (b) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(choices(_))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = flatMap(key)(choices(_))

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = es => f(run(es)(a).get)(es)

  def flatMapWithJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(a).get()(es)

  def joinWithFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(a => a)

  private case class Map2Future[A, B, C](f1: Future[A], f2: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    override def isDone: Boolean = cache.isDefined

    override def isCancelled: Boolean = f1.isCancelled || f2.isCancelled

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = f1.cancel(mayInterruptIfRunning) || f2.cancel(mayInterruptIfRunning)

    def compute(timeout: Long): C = {
      val start = System.nanoTime()
      val a = f1.get(timeout, TimeUnit.NANOSECONDS)
      val end = System.nanoTime()
      val timeLeft = timeout - (end - start)
      val b = f2.get(timeLeft, TimeUnit.NANOSECONDS)
      val c = f(a, b)
      cache = Some(c)
      c
    }
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }


  def main(args: Array[String]) {
    val a = lazyUnit(42 + 1)
    val executor = Executors.newSingleThreadExecutor()

  }
}