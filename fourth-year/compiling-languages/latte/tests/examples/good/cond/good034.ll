; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v1 = private   constant [4 x i8] c"aaa\00"


@_v3 = private   constant [4 x i8] c"aaa\00"


@_v8 = private   constant [8 x i8] c"failure\00"


@_v11 = private   constant [9 x i8] c"it works\00"


define external ccc  i32 @main()    {
entry:
  %_v2 = bitcast [4 x i8]* @_v1 to i8* 
  %_v4 = bitcast [4 x i8]* @_v3 to i8* 
  %_v5 =  call ccc  i32  @_compareStrings(i8*  %_v2, i8*  %_v4)  
  %_v6 = trunc i32 %_v5 to i1  
  %_v7 = xor i1 %_v6, 1 
  br i1 %_v7, label %_B1, label %_B2 
_B1:
  %_v9 = bitcast [8 x i8]* @_v8 to i8* 
   call ccc  void  @printString(i8*  %_v9)  
  br label %_B3 
_B2:
  %_v12 = bitcast [9 x i8]* @_v11 to i8* 
   call ccc  void  @printString(i8*  %_v12)  
  br label %_B3 
_B3:
  ret i32 0 
}