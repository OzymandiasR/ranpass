fun randRandom x y = Random.rand(x,y);

fun randRange lo hi = Random.randRange(lo,hi);

fun intList(a:int,b:int) =
  let
      fun mkIL (c:int,d:int list)=
	case (c<=b)
	 of true => mkIL(c+1,c::d)
	  | _ => rev(d)
  in
      mkIL (a,[])
  end;

val asciiIntPrint = intList(32,126);

fun asciiItoC (a:int list) =
  let
      fun aItoC (b:int list,c:char list) =
	case b
	 of nil => rev(c)
	  | _ => aItoC (tl(b),chr(hd b)::c)
  in
      aItoC (a,[])
  end;

val printCharList = asciiItoC (asciiIntPrint);

val arrPrintChars = Array.fromList printCharList;


fun GenPW(seedLo,seedHi,lengthPW) =
  	if lengthPW<=0
	then print("Password length must be greater than zero! \n")
	else
	    let
		val myRand = randRandom seedLo seedHi
		val myGen = randRange 0 94
		val arrPntChars = arrPrintChars
		fun genRand() = myGen myRand
		fun genRanPW (lenPW:int,pwChars:char list) =
		  case lenPW
		   of 0 => print(implode(rev(pwChars))^"\n")
		    | _ => genRanPW(lenPW-1,(Array.sub(arrPntChars,genRand())::pwChars))
	    in
		genRanPW(lengthPW,[])
	    end;
