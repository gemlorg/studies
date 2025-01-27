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


@_g0 =    constant [5 x i8] c"ahoj\00"


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
  br label %IB_1 
IB_1:
  %_iv1 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv1)  
  br label %IB_2 
IB_2:
  %_v0 = bitcast i1 1 to i1 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  br label %_B2 
_B2:
  %_v1 = phi i1 [0, %IB_2], [0, %_B1] 
  ret i32 0 
}


define external ccc  i1 @print()    {
entry:
  %_v0 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_v0)  
  ret i1 1 
}