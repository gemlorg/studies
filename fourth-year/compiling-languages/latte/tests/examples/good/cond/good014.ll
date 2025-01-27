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
  %_iv1 = icmp eq i32 0, 0 
  br i1 %_iv1, label %IB_2, label %IB_3 
IB_2:
  br label %IB_4 
IB_3:
  br label %IB_4 
IB_4:
  %_v0 = phi i32 [0, %IB_2], [1, %IB_3] 
  ret i32 0 
}


define external ccc  i32 @f(i32  %a)    {
entry:
  %_v0 = icmp eq i32 %a, 0 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  ret i32 0 
_B2:
  ret i32 1 
}