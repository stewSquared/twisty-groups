package blog
package object ycomm {

  locally {
    def fact(n: Int): Int =
      if (n <= 1) 1
      else n * fact(n - 1)

    def fact2(n: Int, f: Int => Int = fact): Int =
      if (n <= 1) 1
      else n * f(n - 1)

    def fact3(f: => Int => Int): Int => Int = {
      n => if (n <= 1) 1 else n * f(n - 1)
    }



    // def Y[A](f: (A => A) => (A => A)): (A => A) = f(Y(f))
    
    def factorial = {
      {(f: Int => Int) => (n: Int) =>
        if (n <= 1) 1 else n * f(n - 1)
        (f: Int => Int) => (n: Int) =>
        if (n <= 1) 1 else n * f(n - 1)
        (f: Int => Int) => (n: Int) =>
        if (n <= 1) 1 else n * f(n - 1)
        (f: Int => Int) => (n: Int) =>
        if (n <= 1) 1 else n * f(n - 1)
        (f: Int => Int) => (n: Int) =>
        if (n <= 1) 1 else n * f(n - 1)
      }.apply((n: Int) => 1)
    }


  }


}
