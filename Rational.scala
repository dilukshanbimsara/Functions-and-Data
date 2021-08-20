object MyApp extends App{
    val w = new Rational(3)
    val x = new Rational(3,4)
    val y = new Rational(5,8)
    val z = new Rational(2,7)
    val t = x - y - z
    println(t)
} 

class Rational(n:Int, d:Int){

    require(d>0, "The given value d greater than 0")
    // def numer = n/gcd(n,d)
    // def denom = d/gcd(n,d)
    def numer = n/gcd(Math.abs(n),d)
    def denom = d/gcd(Math.abs(n),d)
    
//constructore
    // def this(x:Int) = this(x,1)
    //originala constructore 
    def this(n:Int) = this(n,1)

//Add function
    // def add(r:Rational) = new Rational(this.numer * r.denom + this.denom * r.numer , this.denom*r.denom)
    //we can create add function using "+" also
    def +(r:Rational) = new Rational(this.numer * r.denom + this.denom * r.numer , this.denom*r.denom)
    // 2/3 + 4/5 = (2*5 +4*3)/3*5

     private def gcd( a:Int , b:Int ):Int = {
        if ( b == 0 ) a
        else if ( b > a ) gcd( b , a )
        else gcd( b, a%b )
    }


//Negative
    def neg = new Rational(-this.numer, this.denom)

//Substraction = addition of negative and positive
    def -(r:Rational) = this+r.neg

//less than
    def less(that:Rational) = this.numer*that.denom < this.denom*that.numer

//greater than
    def max(that:Rational) = this.numer*that.denom > this.denom*that.numer
    
    def *( that : Rational ) : Rational = new Rational( this.numer * that.numer , this.denom * that.denom )

    def /( that : Rational ) : Rational = new Rational( this.numer * that.denom , this.denom * that.numer )


    override def toString= numer + "/" + denom
}