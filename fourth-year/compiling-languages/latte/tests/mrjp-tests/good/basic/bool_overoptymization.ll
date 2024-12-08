; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v3 = private   constant [5 x i8] c"ahoj\00"


define external ccc  i32 @main()    {
entry:
  %_v1 =  call ccc  i1  @print()  
  br i1 %_v1, label %_B1, label %_B2 
_B1:
  br label %_B2 
_B2:
  %_v2 = phi i1 [0, %entry], [0, %_B1] 
  ret i32 0 
}


define external ccc  i1 @print()    {
entry:
  %_v4 = bitcast [5 x i8]* @_v3 to i8* 
   call ccc  void  @printString(i8*  %_v4)  
  ret i1 1 
}