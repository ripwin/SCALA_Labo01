
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

/**
  * Valeur absolue
  */
def abs(x: Double) ={
  if(x < 0 ) -x
  else x
}

/*
 * Racine carré
 */
def sqrt(n: Double) = {
  val epsilon = 0.0001
  def calcSqrt(n: Double, x : Double): Double = {
    if((abs(square(x) -n )/n) < epsilon) x
    else calcSqrt(n, ((x+ n/x)/2))
  }
  calcSqrt(n, 1)
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


/*
 * Mémoire de la machine
 * utiliser une map var memory: Map(String, double) = Map()
 */

/*
 * Equation du second degré
 */

/*
 * Nombre premier
 */
def primeNumber(x: Int) : String ={
  def isPrimeNumber(a: Int, b:Int) : String = {
    if(b >= sqrt(a)) a + "is a prime number"
    else if(a % b == 0) "Not a prime number"
    else isPrimeNumber(a, b + 1)
  }

  if (x < 2) "Not a prime number"
  else isPrimeNumber(x, 2)
}



/*
 * Algorithme d'euclide étendu
 */

/*
 * Inverse moduclaire
 */