; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v5 = private   constant [4 x i8] c"yes\00"


@_v8 = private   constant [5 x i8] c"NOOO\00"


define external ccc  i32 @main()    {
entry:
   call ccc  void  @f(i32  1, i32  2)  
  ret i32 0 
}


define external ccc  void @f(i32  %x, i32  %y)    {
entry:
  %_v2 = icmp sgt i32 %y, %x 
  br i1 %_v2, label %_B2, label %_B1 
_B1:
  %_v3 =  call ccc  i1  @e()  
  br label %_B2 
_B2:
  %_v4 = phi i1 [1, %entry], [%_v3, %_B1] 
  br i1 %_v4, label %_B3, label %_B4 
_B3:
  %_v6 = bitcast [4 x i8]* @_v5 to i8* 
   call ccc  void  @printString(i8*  %_v6)  
  br label %_B4 
_B4:
  ret void 
}


define external ccc  i1 @e()    {
entry:
  %_v9 = bitcast [5 x i8]* @_v8 to i8* 
   call ccc  void  @printString(i8*  %_v9)  
  ret i1 0 
}