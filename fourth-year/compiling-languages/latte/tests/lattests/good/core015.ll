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
  %_v1 =  call ccc  i32  @ev(i32  17)  
   call ccc  void  @printInt(i32  %_v1)  
  ret i32 0 
}


define external ccc  i32 @ev(i32  %y)    {
entry:
  %_v3 = icmp sgt i32 %y, 0 
  br i1 %_v3, label %_B1, label %_B2 
_B1:
  %_v4 = sub   i32 %y, 2 
  %_v5 =  call ccc  i32  @ev(i32  %_v4)  
  ret i32 %_v5 
_B2:
  %_v6 = icmp slt i32 %y, 0 
  br i1 %_v6, label %_B4, label %_B5 
_B4:
  ret i32 0 
_B5:
  ret i32 1 
}