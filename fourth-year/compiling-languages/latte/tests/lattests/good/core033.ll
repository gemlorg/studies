; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


define external ccc  i32 @funkcja_ifbooltrudny(i1  %f, i1  %t)    {
entry:
  %_v1 = xor i1 1, %f 
  br i1 %_v1, label %_B1, label %_B2 
_B1:
  br label %_B2 
_B2:
  %_v2 = phi i1 [0, %entry], [%f, %_B1] 
  br i1 %_v2, label %_B4, label %_B3 
_B3:
  br i1 %f, label %_B6, label %_B5 
_B5:
  %_v3 = xor i1 1, %t 
  br label %_B6 
_B6:
  %_v4 = phi i1 [1, %_B3], [%_v3, %_B5] 
  %_v5 = xor i1 1, %_v4 
  br label %_B4 
_B4:
  %_v6 = phi i1 [1, %_B2], [%_v5, %_B6] 
  br i1 %_v6, label %_B7, label %_B8 
_B7:
   call ccc  void  @printInt(i32  1042)  
  br label %_B9 
_B8:
   call ccc  void  @printInt(i32  2042)  
  br label %_B9 
_B9:
  ret i32 0 
}


define external ccc  i32 @main()    {
entry:
  %_v9 =  call ccc  i32  @funkcja_ifbooltrudny(i1  0, i1  1)  
  ret i32 0 
}