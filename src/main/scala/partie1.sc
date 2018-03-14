
/*
 * Addition
 */
def add(x: Int, y: Int) = x + y

/*
 * Soustraction
 */
def substract(x: Int, y: Int) = x - y

/*
 * Puissance carrée
 */
def square(x: Double) = x * x

/*
 * Puissance n
 */
def power(x: Double, n: Int): Double = {
  if (n == 0) 1.0
  else if (n % 2 == 0) square(power(x, n / 2))
  else x * power(x, n - 1)
}

/*
 * Fonction factoriel récursive terminale
 */
def factorial(n: Int): Int = {
  def loop(acc: Int, n: Int) : Int = {
    if (n == 0) acc
    else loop(acc * n, n - 1)
  }

  loop(1, n)
}


/*
 * Le plus grand diviseur commun
 */
def gcd(a: Int, b: Int) : Int =
  if (b == 0) a else gcd(b, a % b)