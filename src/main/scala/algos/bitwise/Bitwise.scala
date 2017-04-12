package algos.bitwise

object Bitwise {
	

	
	/*
		Given a char convert it to case 
		Toggle 5th bit to change the case 
	*/

	def convertCase(i:Char):Char = (i ^ 32).toChar

	def checkEven(i:Int):Boolean = if((i & 1) == 1) false else true

	def isNthBitSet(x:Int,n:Int) = if( (x & (1 << n)) == 0 ) false else true

	def setNthBit(x:Int,n:Int) = (x | (1 << n)) 

	def unSetNthBit(x:Int,n:Int) = (x & ~(1 << n)) 

	def toggleNthBit(x:Int,n:Int) = (x ^ (1 << n)) 

	def turnOffRightMost1Bit(x:Int) = (x & x-1) 

	def turnOnRightMost0Bit(x:Int) = (x | x+1) 
}