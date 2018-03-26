
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
 * (Les valeurs < 2 ne sont pas considérées comme nombre premier)
 */
def primeNumber(x: Int) : String ={
  def isPrimeNumber(a: Int, b:Int) : String = {
    if(b >= sqrt(a)) a + " is a prime number"
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

/*
 * Equations 2eme degré
 */

def solve(a: Double, b: Double, c: Double): Any = {
  def delta(a: Double, b: Double, c: Double) = square(b) - 4 * a * c

  // Si a = 0 alors ce n'est pas une equation du 2ème degré
  if (a == 0) throw new Error("Syntax Error")

  val d = delta(a, b, c)
  println(d)
  d match {
    // delta = 0 : solution unique
    case 0 => List() :+ (-b) / (2 * a)
    // delta > 0 : 1ère solution et 2ème solution
    case x if (d > 0) => {
      List() :+ ((-b + sqrt(d)) / 2 * a) :+ ((-b - sqrt(d)) / 2 * a)
    }
    // delta < 0 :  réelle et tuple(1ère complete, 2ème complexe)
    case _ => {
      List() :+ (-b) / (2 * a) :+ (sqrt((-d)) / (2 * a), (-sqrt((-d))) / (2 * a))
    }
  }
}
/* Exemples de test
solve(1,1,-2)// (delta > 0)
solve(4,4,1) // delta = 0
solve(2,1,5) // delta < 0
*/