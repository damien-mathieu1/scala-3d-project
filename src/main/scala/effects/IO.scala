package effects

// Minimal IO monad — wraps a lazy side-effectful computation
opaque type IO[+A] = () => A

object IO:
  def apply[A](a: => A): IO[A]  = () => a
  def pure[A](a: A): IO[A]      = () => a

  extension [A](io: IO[A])
    def map[B](f: A => B): IO[B]          = IO(f(io()))
    def flatMap[B](f: A => IO[B]): IO[B]  = IO(f(io())())
    def unsafeRun(): A                    = io()
