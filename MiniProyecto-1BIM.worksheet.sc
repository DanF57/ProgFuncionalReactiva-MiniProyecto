

def simpson(a:Int, b:Int, f:Double => Double) : Double = {
    (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}

def simpsonCompuesta(a:Int, b:Int, n:Int, f:Double => Double) : Double = {
	val h = (b-a)/n

	f(x(2*j-2))+4*f(x(2*j-1))+f(x(2*j))
}

val f = (x : Double) => -Math.pow(x,2)+(8*x)-12
simpson(3, 5, f)

val g = (x : Double) => 3*Math.pow(x,2)
simpson(0,2,g)

val h = (x : Double) => x+2*Math.pow(x,2)-Math.pow(x,3)+5*Math.pow(x,4)
simpson(-1,1,h)

val i = (x : Double) => ((2*x+1)/Math.pow(x,2)+x)
simpson(1,2,i)

val j = (x : Double) => Math.pow(Math.E,x)
simpson(0,1,j)

val k = (x : Double) => (1/Math.sqrt(x-1))
simpson(2,3,k)

val l = (x : Double) => (1/1+Math.pow(x,2))
simpson(0,2,l)

