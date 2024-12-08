; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v12 = private   constant [4 x i8] c"foo\00"


define external ccc  i32 @main()    {
entry:
   call ccc  void  @printInt(i32  1)  
   call ccc  void  @printInt(i32  78)  
  br label %_B2 
_B1:
  %_v4 = sub   i32 %_v3, 1 
   call ccc  void  @printInt(i32  %_v4)  
  %_v6 = add   i32 %_v4, 7 
   call ccc  void  @printInt(i32  %_v6)  
  br label %_B2 
_B2:
  %_v3 = phi i32 [78, %entry], [%_v4, %_B1] 
  %_v8 = icmp sgt i32 %_v3, 76 
  br i1 %_v8, label %_B1, label %_B3 
_B3:
   call ccc  void  @printInt(i32  %_v3)  
  %_v10 = icmp sgt i32 %_v3, 4 
  br i1 %_v10, label %_B4, label %_B5 
_B4:
   call ccc  void  @printInt(i32  4)  
  br label %_B6 
_B5:
  %_v13 = bitcast [4 x i8]* @_v12 to i8* 
   call ccc  void  @printString(i8*  %_v13)  
  br label %_B6 
_B6:
   call ccc  void  @printInt(i32  %_v3)  
  ret i32 0 
}