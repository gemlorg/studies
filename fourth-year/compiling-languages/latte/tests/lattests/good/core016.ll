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
  br label %_B2 
_B1:
  %_v1 = sub   i32 %_v0, 2 
  br label %_B2 
_B2:
  %_v0 = phi i32 [17, %entry], [%_v1, %_B1] 
  %_v2 = icmp sgt i32 %_v0, 0 
  br i1 %_v2, label %_B1, label %_B3 
_B3:
  %_v3 = icmp slt i32 %_v0, 0 
  br i1 %_v3, label %_B4, label %_B5 
_B4:
   call ccc  void  @printInt(i32  0)  
  ret i32 0 
_B5:
   call ccc  void  @printInt(i32  1)  
  ret i32 0 
}