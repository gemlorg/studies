; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


define external ccc  i32 @main()    {
entry:
  %_v1 =  call ccc  i32  @f(i32  1, i32  -1)  
   call ccc  void  @printInt(i32  %_v1)  
  ret i32 0 
}


define external ccc  i32 @f(i32  %a, i32  %b)    {
entry:
  %_v3 = icmp sgt i32 %a, 0 
  br i1 %_v3, label %_B1, label %_B2 
_B1:
  %_v4 = icmp sgt i32 %b, 0 
  br label %_B2 
_B2:
  %_v5 = phi i1 [0, %entry], [%_v4, %_B1] 
  br i1 %_v5, label %_B4, label %_B3 
_B3:
  %_v6 = icmp slt i32 %a, 0 
  br i1 %_v6, label %_B5, label %_B6 
_B5:
  %_v7 = icmp slt i32 %b, 0 
  br label %_B6 
_B6:
  %_v8 = phi i1 [0, %_B3], [%_v7, %_B5] 
  br label %_B4 
_B4:
  %_v9 = phi i1 [1, %_B2], [%_v8, %_B6] 
  br i1 %_v9, label %_B7, label %_B8 
_B7:
  ret i32 7 
_B8:
  ret i32 42 
}