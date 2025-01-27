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


@_g0 =    constant [2 x i8] c"a\00"


@_g1 =    constant [2 x i8] c"a\00"


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
  %_v0 = bitcast [2 x i8]* @_g0 to i8* 
  br label %_B2 
_B1:
  %_v3 = bitcast [2 x i8]* @_g1 to i8* 
  %_v4 =  call ccc  i8*  @_concatStrings(i8*  %_v2, i8*  %_v3)  
  %_v5 = add   i32 %_v1, 1 
  br label %_B2 
_B2:
  %_v1 = phi i32 [0, %entry], [%_v5, %_B1] 
  %_v2 = phi i8* [%_v0, %entry], [%_v4, %_B1] 
  %_v6 = icmp slt i32 %_v1, 10 
  br i1 %_v6, label %_B1, label %_B3 
_B3:
   call ccc  void  @printString(i8*  %_v2)  
  ret i32 0 
}