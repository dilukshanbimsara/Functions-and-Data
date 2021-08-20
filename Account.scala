object Account extends App{
    // val A = new Account("199936013026" , "045200180083654" , 15)
    // val B = new Account("9825364126v" , "045200188003564" , 30)
    // println(A)
    // println(B)
    // println(A.transfer(5,B))
    // println("transfer 5 A to B")
    // println(A)
    // println(B)


    var bank : List[Account] = List( new Account("199936013026" , "045200180083654" , -1500) , new Account("9825364126v" , "045200188003564" , 2000) )

    val find = ( n : Int , b : List[Account]) => b.filter( x => x.account_number.equals( n ))

    val overdraft = ( b : List[Account] ) => b.filter( x => x.balance < 0)

    val balance = ( b: List[Account] ) => b.map( x => (x,x.balance) ).reduce( (a , c) => ( c._1 , a._2 + c._2) )

    val interest = ( b : List[Account] ) => b.map( x => {
        x.balance match {
            case a if a >= 0 => x deposit x.balance * 0.05
            case _ => x withdraw Math.abs(x.balance) * 0.01

        }
        x
    })


    println("bank " + bank )
    println()
    println("find " + find( 2 , bank) )
    println()
    println("overdraft " + overdraft( bank ) )
    println()
    println("balance " + balance( bank )._2 )
    println()

    bank = interest( bank )

    println("bank " + bank )
    println()
    println("balance " + balance( bank )._2 )
    println()


}

class Account( id:String , a:String , b:Double){
    val NIC : String = id
    val account_number : String = a
    var balance : Double = b

    def withdraw( amount : Double ) = this.balance = this.balance - amount

    def deposit ( amount : Double ) = this.balance = this.balance + amount

    def transfer( amount : Double , to_account : Account ): Unit = {
        this.balance = this.balance - amount
        to_account.balance = to_account.balance + balance
    }

    @Override
    override def toString() : String = "NIC : " +this.NIC + "\nAccount Number : " + this.account_number + "\nBalance : " + this.balance
}