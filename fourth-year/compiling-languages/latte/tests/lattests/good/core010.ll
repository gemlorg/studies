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
  br label %IB_3 
IB_2:
  %_iv1 = mul   i32 %_iv4, %_iv3 
  %_iv2 = sub   i32 %_iv3, 1 
  br label %IB_3 
IB_3:
  %_iv3 = phi i32 [5, %IB_1], [%_iv2, %IB_2] 
  %_iv4 = phi i32 [1, %IB_1], [%_iv1, %IB_2] 
  %_iv5 = icmp sgt i32 %_iv3, 0 
  br i1 %_iv5, label %IB_2, label %IB_4 
IB_4:
  br label %IB_5 
IB_5:
  %_v0 = bitcast i32 %_iv4 to i32 
   call ccc  void  @printInt(i32  %_v0)  
  ret i32 0 
}


define external ccc  i32 @fac(i32  %a)    {
entry:
  br label %_B2 
_B1:
  %_v2 = mul   i32 %_v1, %_v0 
  %_v3 = sub   i32 %_v0, 1 
  br label %_B2 
_B2:
  %_v0 = phi i32 [%a, %entry], [%_v3, %_B1] 
  %_v1 = phi i32 [1, %entry], [%_v2, %_B1] 
  %_v4 = icmp sgt i32 %_v0, 0 
  br i1 %_v4, label %_B1, label %_B3 
_B3:
  ret i32 %_v1 
}