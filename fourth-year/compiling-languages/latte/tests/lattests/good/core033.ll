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


define external ccc  i32 @funkcja_ifbooltrudny(i1  %f, i1  %t)    {
entry:
  %_v0 = xor i1 1, %f 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  br label %_B2 
_B2:
  %_v1 = phi i1 [0, %entry], [%f, %_B1] 
  br i1 %_v1, label %_B4, label %_B3 
_B3:
  br i1 %f, label %_B6, label %_B5 
_B5:
  %_v2 = xor i1 1, %t 
  br label %_B6 
_B6:
  %_v3 = phi i1 [1, %_B3], [%_v2, %_B5] 
  %_v4 = xor i1 1, %_v3 
  br label %_B4 
_B4:
  %_v5 = phi i1 [1, %_B2], [%_v4, %_B6] 
  br i1 %_v5, label %_B7, label %_B8 
_B7:
   call ccc  void  @printInt(i32  1042)  
  br label %_B9 
_B8:
   call ccc  void  @printInt(i32  2042)  
  br label %_B9 
_B9:
  ret i32 0 
}


define external ccc  i32 @main()    {
entry:
  br label %IB_1 
IB_1:
  %_iv1 = xor i1 1, 0 
  br i1 %_iv1, label %IB_2, label %IB_3 
IB_2:
  br label %IB_3 
IB_3:
  %_iv2 = phi i1 [0, %IB_1], [0, %IB_2] 
  br i1 %_iv2, label %IB_7, label %IB_4 
IB_4:
  br i1 0, label %IB_6, label %IB_5 
IB_5:
  %_iv3 = xor i1 1, 1 
  br label %IB_6 
IB_6:
  %_iv4 = phi i1 [1, %IB_4], [%_iv3, %IB_5] 
  %_iv5 = xor i1 1, %_iv4 
  br label %IB_7 
IB_7:
  %_iv6 = phi i1 [1, %IB_3], [%_iv5, %IB_6] 
  br i1 %_iv6, label %IB_8, label %IB_9 
IB_8:
   call ccc  void  @printInt(i32  1042)  
  br label %IB_10 
IB_9:
   call ccc  void  @printInt(i32  2042)  
  br label %IB_10 
IB_10:
  br label %IB_11 
IB_11:
  %_v0 = bitcast i32 0 to i32 
  ret i32 0 
}