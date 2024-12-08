; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v3 = private   constant [12 x i8] c"jeszcze raz\00"


define external ccc  i32 @main()    {
entry:
  br label %_B2 
_B1:
  %_v1 =  call ccc  i32  @readInt()  
  %_v2 = icmp eq i32 %_v1, 1 
  br i1 %_v2, label %_B4, label %_B5 
_B4:
  ret i32 0 
_B5:
  %_v4 = bitcast [12 x i8]* @_v3 to i8* 
   call ccc  void  @printString(i8*  %_v4)  
  br label %_B6 
_B6:
  br label %_B2 
_B2:
  br label %_B1 
_B3:
  ret i32 0 
}