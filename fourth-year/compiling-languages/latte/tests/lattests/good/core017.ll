; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v20 = private   constant [5 x i8] c"true\00"


@_v23 = private   constant [6 x i8] c"false\00"


define external ccc  i32 @main()    {
entry:
   call ccc  void  @printBool(i1  1)  
   call ccc  void  @printBool(i1  1)  
   call ccc  void  @printBool(i1  0)  
   call ccc  void  @printBool(i1  1)  
  %_v11 =  call ccc  i1  @implies(i1  0, i1  0)  
   call ccc  void  @printBool(i1  %_v11)  
  %_v13 =  call ccc  i1  @implies(i1  0, i1  1)  
   call ccc  void  @printBool(i1  %_v13)  
  %_v15 =  call ccc  i1  @implies(i1  1, i1  0)  
   call ccc  void  @printBool(i1  %_v15)  
  %_v17 =  call ccc  i1  @implies(i1  1, i1  1)  
   call ccc  void  @printBool(i1  %_v17)  
  ret i32 0 
}


define external ccc  i1 @dontCallMe(i32  %x)    {
entry:
   call ccc  void  @printInt(i32  %x)  
  ret i1 1 
}


define external ccc  void @printBool(i1  %b)    {
entry:
  br i1 %b, label %_B1, label %_B2 
_B1:
  %_v21 = bitcast [5 x i8]* @_v20 to i8* 
   call ccc  void  @printString(i8*  %_v21)  
  br label %_B3 
_B2:
  %_v24 = bitcast [6 x i8]* @_v23 to i8* 
   call ccc  void  @printString(i8*  %_v24)  
  br label %_B3 
_B3:
  ret void 
}


define external ccc  i1 @implies(i1  %x, i1  %y)    {
entry:
  %_v26 = xor i1 1, %x 
  br i1 %_v26, label %_B5, label %_B4 
_B4:
  %_v27 = icmp eq i1 %x, %y 
  br label %_B5 
_B5:
  %_v28 = phi i1 [1, %entry], [%_v27, %_B4] 
  ret i1 %_v28 
}