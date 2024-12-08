; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v2 = private   constant [2 x i8] c"4\00"


@_v6 = private   constant [2 x i8] c"4\00"


@_v12 = private   constant [2 x i8] c"6\00"


@_v16 = private   constant [2 x i8] c"6\00"


define external ccc  i32 @main()    {
entry:
  %_v3 = bitcast [2 x i8]* @_v2 to i8* 
   call ccc  void  @printString(i8*  %_v3)  
  %_v7 = bitcast [2 x i8]* @_v6 to i8* 
   call ccc  void  @printString(i8*  %_v7)  
  %_v13 = bitcast [2 x i8]* @_v12 to i8* 
   call ccc  void  @printString(i8*  %_v13)  
  %_v17 = bitcast [2 x i8]* @_v16 to i8* 
   call ccc  void  @printString(i8*  %_v17)  
  ret i32 0 
}