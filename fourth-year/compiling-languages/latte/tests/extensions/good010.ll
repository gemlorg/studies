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


@_g0 =    constant [5 x i8] c"test\00"


@_g1 =    constant [6 x i8] c"test2\00"


@_g2 =    constant [5 x i8] c"test\00"


@_g3 =    constant [6 x i8] c"test2\00"


@_g4 =    constant [5 x i8] c"test\00"


@_g5 =    constant [6 x i8] c"test2\00"


@_g6 =    constant [5 x i8] c"test\00"


@_g7 =    constant [6 x i8] c"test2\00"


@_g8 =    constant [5 x i8] c"test\00"


@_g9 =    constant [6 x i8] c"test2\00"


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
  %_v0 = bitcast [5 x i8]* @_g0 to i8* 
  %_v1 = bitcast [6 x i8]* @_g1 to i8* 
  %_v2 =  call ccc  i8*  @_concatStrings(i8*  %_v0, i8*  %_v1)  
  %_v3 = bitcast [5 x i8]* @_g2 to i8* 
  %_v4 = bitcast [6 x i8]* @_g3 to i8* 
  %_v5 =  call ccc  i8*  @_concatStrings(i8*  %_v3, i8*  %_v4)  
  %_v6 = bitcast [5 x i8]* @_g4 to i8* 
  %_v7 = bitcast [6 x i8]* @_g5 to i8* 
  %_v8 =  call ccc  i8*  @_concatStrings(i8*  %_v6, i8*  %_v7)  
  %_v9 = bitcast [5 x i8]* @_g6 to i8* 
  %_v10 = bitcast [6 x i8]* @_g7 to i8* 
  %_v11 =  call ccc  i8*  @_concatStrings(i8*  %_v9, i8*  %_v10)  
  %_v12 = bitcast [5 x i8]* @_g8 to i8* 
  %_v13 = bitcast [6 x i8]* @_g9 to i8* 
  %_v14 =  call ccc  i8*  @_concatStrings(i8*  %_v12, i8*  %_v13)  
  ret i32 0 
}