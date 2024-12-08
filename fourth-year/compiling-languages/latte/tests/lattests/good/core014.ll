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
   call ccc  void  @printInt(i32  1)  
  br label %_B2 
_B1:
   call ccc  void  @printInt(i32  %_v2)  
  %_v5 = add   i32 %_v3, %_v2 
  %_v6 = sub   i32 %_v5, %_v3 
  br label %_B2 
_B2:
  %_v2 = phi i32 [1, %entry], [%_v5, %_B1] 
  %_v3 = phi i32 [1, %entry], [%_v6, %_B1] 
  %_v7 = icmp slt i32 %_v2, 5000000 
  br i1 %_v7, label %_B1, label %_B3 
_B3:
  ret i32 0 
}