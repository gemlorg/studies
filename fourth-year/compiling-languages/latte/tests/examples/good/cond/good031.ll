; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


declare external ccc  i8* @_malloc(i32)    


declare external ccc  i32 @_count_arr_length(i8*, i32)    


@_g0 =    constant [4 x i8] c"abc\00"


@_g1 =    constant [4 x i8] c"abc\00"


@_g2 =    constant [9 x i8] c"it works\00"


@_g3 =    constant [8 x i8] c"failure\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


define external ccc  i32 @main()    {
entry:
  %_v0 = bitcast [4 x i8]* @_g0 to i8* 
  %_v1 = bitcast [4 x i8]* @_g1 to i8* 
  %_v2 =  call ccc  i32  @_compareStrings(i8*  %_v0, i8*  %_v1)  
  %_v3 = trunc i32 %_v2 to i1  
  br i1 %_v3, label %_B1, label %_B2 
_B1:
  %_v4 = bitcast [9 x i8]* @_g2 to i8* 
   call ccc  void  @printString(i8*  %_v4)  
  br label %_B3 
_B2:
  %_v6 = bitcast [8 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_v6)  
  br label %_B3 
_B3:
  ret i32 0 
}