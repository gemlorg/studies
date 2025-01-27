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
  %_iv1 = icmp sgt i32 1, 0 
  br i1 %_iv1, label %IB_2, label %IB_3 
IB_2:
  %_iv2 = icmp sgt i32 -1, 0 
  br label %IB_3 
IB_3:
  %_iv3 = phi i1 [0, %IB_1], [%_iv2, %IB_2] 
  br i1 %_iv3, label %IB_7, label %IB_4 
IB_4:
  %_iv4 = icmp slt i32 1, 0 
  br i1 %_iv4, label %IB_5, label %IB_6 
IB_5:
  %_iv5 = icmp slt i32 -1, 0 
  br label %IB_6 
IB_6:
  %_iv6 = phi i1 [0, %IB_4], [%_iv5, %IB_5] 
  br label %IB_7 
IB_7:
  %_iv7 = phi i1 [1, %IB_3], [%_iv6, %IB_6] 
  br i1 %_iv7, label %IB_8, label %IB_9 
IB_8:
  br label %IB_10 
IB_9:
  br label %IB_10 
IB_10:
  %_v0 = phi i32 [7, %IB_8], [42, %IB_9] 
   call ccc  void  @printInt(i32  %_v0)  
  ret i32 0 
}


define external ccc  i32 @f(i32  %a, i32  %b)    {
entry:
  %_v0 = icmp sgt i32 %a, 0 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  %_v1 = icmp sgt i32 %b, 0 
  br label %_B2 
_B2:
  %_v2 = phi i1 [0, %entry], [%_v1, %_B1] 
  br i1 %_v2, label %_B4, label %_B3 
_B3:
  %_v3 = icmp slt i32 %a, 0 
  br i1 %_v3, label %_B5, label %_B6 
_B5:
  %_v4 = icmp slt i32 %b, 0 
  br label %_B6 
_B6:
  %_v5 = phi i1 [0, %_B3], [%_v4, %_B5] 
  br label %_B4 
_B4:
  %_v6 = phi i1 [1, %_B2], [%_v5, %_B6] 
  br i1 %_v6, label %_B7, label %_B8 
_B7:
  ret i32 7 
_B8:
  ret i32 42 
}