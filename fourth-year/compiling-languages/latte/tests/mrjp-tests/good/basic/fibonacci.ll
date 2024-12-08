; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v13 = private   constant [42 x i8] c"Expected a non-negative integer, but got:\00"


define external ccc  i32 @fibonacci(i32  %n)    {
entry:
  %_v1 = icmp sle i32 %n, 1 
  br i1 %_v1, label %_B1, label %_B2 
_B1:
  ret i32 %n 
_B2:
  br label %_B4 
_B3:
  %_v6 = add   i32 %_v3, %_v2 
  %_v7 = add   i32 %_v4, 1 
  br label %_B4 
_B4:
  %_v2 = phi i32 [0, %_B2], [%_v3, %_B3] 
  %_v3 = phi i32 [1, %_B2], [%_v6, %_B3] 
  %_v4 = phi i32 [2, %_B2], [%_v7, %_B3] 
  %_v5 = phi i32 [0, %_B2], [%_v6, %_B3] 
  %_v8 = icmp sle i32 %_v4, %n 
  br i1 %_v8, label %_B3, label %_B5 
_B5:
  ret i32 %_v3 
}


define external ccc  i32 @main()    {
entry:
  %_v9 =  call ccc  i32  @readInt()  
  %_v10 = icmp sge i32 %_v9, 0 
  br i1 %_v10, label %_B6, label %_B7 
_B6:
  %_v11 =  call ccc  i32  @fibonacci(i32  %_v9)  
   call ccc  void  @printInt(i32  %_v11)  
  ret i32 0 
_B7:
  %_v14 = bitcast [42 x i8]* @_v13 to i8* 
   call ccc  void  @printString(i8*  %_v14)  
   call ccc  void  @printInt(i32  %_v9)  
  ret i32 1 
}