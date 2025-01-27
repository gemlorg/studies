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


@_g0 =    constant [5 x i8] c"true\00"


@_g1 =    constant [6 x i8] c"false\00"


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
  %_v0 =  call ccc  i1  @t(i32  1)  
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  br label %IB_1 
IB_1:
   call ccc  void  @printInt(i32  2)  
  br label %IB_2 
IB_2:
  %_v1 = bitcast i1 0 to i1 
  br label %_B2 
_B2:
  %_v2 = phi i1 [0, %entry], [%_v1, %IB_2] 
  br label %IB_3 
IB_3:
  br i1 %_v2, label %IB_4, label %IB_5 
IB_4:
  %_iv1 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv1)  
  br label %IB_6 
IB_5:
  %_iv2 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv2)  
  br label %IB_6 
IB_6:
  br label %IB_7 
IB_7:
  %_v4 =  call ccc  i1  @t(i32  3)  
  br i1 %_v4, label %_B3, label %_B4 
_B3:
  %_v5 =  call ccc  i1  @t(i32  4)  
  br label %_B4 
_B4:
  %_v6 = phi i1 [0, %IB_7], [%_v5, %_B3] 
  br label %IB_8 
IB_8:
  br i1 %_v6, label %IB_9, label %IB_10 
IB_9:
  %_iv3 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv3)  
  br label %IB_11 
IB_10:
  %_iv4 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv4)  
  br label %IB_11 
IB_11:
  br label %IB_12 
IB_12:
  %_v8 =  call ccc  i1  @t(i32  5)  
  br i1 %_v8, label %_B6, label %_B5 
_B5:
  %_v9 =  call ccc  i1  @t(i32  6)  
  br label %_B6 
_B6:
  %_v10 = phi i1 [1, %IB_12], [%_v9, %_B5] 
  br label %IB_13 
IB_13:
  br i1 %_v10, label %IB_14, label %IB_15 
IB_14:
  %_iv5 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv5)  
  br label %IB_16 
IB_15:
  %_iv6 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv6)  
  br label %IB_16 
IB_16:
  br label %IB_17 
IB_17:
  br label %IB_18 
IB_18:
   call ccc  void  @printInt(i32  7)  
  br label %IB_19 
IB_19:
  %_v12 = bitcast i1 0 to i1 
  br i1 %_v12, label %_B7, label %_B8 
_B7:
  %_v13 =  call ccc  i1  @t(i32  8)  
  br label %_B8 
_B8:
  %_v14 = phi i1 [0, %IB_19], [%_v13, %_B7] 
  br label %IB_20 
IB_20:
  br i1 %_v14, label %IB_21, label %IB_22 
IB_21:
  %_iv7 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv7)  
  br label %IB_23 
IB_22:
  %_iv8 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv8)  
  br label %IB_23 
IB_23:
  br label %IB_24 
IB_24:
  %_v16 =  call ccc  i1  @t(i32  9)  
  br i1 %_v16, label %_B9, label %_B10 
_B9:
  %_v17 =  call ccc  i1  @t(i32  10)  
  br i1 %_v17, label %_B11, label %_B12 
_B11:
  %_v18 =  call ccc  i1  @t(i32  11)  
  br label %_B12 
_B12:
  %_v19 = phi i1 [0, %_B9], [%_v18, %_B11] 
  br label %_B10 
_B10:
  %_v20 = phi i1 [0, %IB_24], [%_v19, %_B12] 
  br label %IB_25 
IB_25:
  br i1 %_v20, label %IB_26, label %IB_27 
IB_26:
  %_iv9 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv9)  
  br label %IB_28 
IB_27:
  %_iv10 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv10)  
  br label %IB_28 
IB_28:
  br label %IB_29 
IB_29:
  br label %IB_30 
IB_30:
   call ccc  void  @printInt(i32  12)  
  br label %IB_31 
IB_31:
  %_v22 = bitcast i1 0 to i1 
  br i1 %_v22, label %_B14, label %_B13 
_B13:
  br label %IB_32 
IB_32:
   call ccc  void  @printInt(i32  13)  
  br label %IB_33 
IB_33:
  %_v23 = bitcast i1 0 to i1 
  br i1 %_v23, label %_B15, label %_B16 
_B15:
  %_v24 =  call ccc  i1  @t(i32  14)  
  br label %_B16 
_B16:
  %_v25 = phi i1 [0, %IB_33], [%_v24, %_B15] 
  br label %_B14 
_B14:
  %_v26 = phi i1 [1, %IB_31], [%_v25, %_B16] 
  br label %IB_34 
IB_34:
  br i1 %_v26, label %IB_35, label %IB_36 
IB_35:
  %_iv11 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv11)  
  br label %IB_37 
IB_36:
  %_iv12 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv12)  
  br label %IB_37 
IB_37:
  br label %IB_38 
IB_38:
  ret i32 0 
}


define external ccc  i1 @f(i32  %a)    {
entry:
   call ccc  void  @printInt(i32  %a)  
  ret i1 0 
}


define external ccc  i1 @t(i32  %a)    {
entry:
  br label %IB_39 
IB_39:
   call ccc  void  @printInt(i32  %a)  
  br label %IB_40 
IB_40:
  %_v0 = bitcast i1 0 to i1 
  %_v1 = xor i1 1, %_v0 
  ret i1 %_v1 
}


define external ccc  void @b(i1  %a)    {
entry:
  br i1 %a, label %_B1, label %_B2 
_B1:
  %_v0 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_v0)  
  br label %_B3 
_B2:
  %_v2 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_v2)  
  br label %_B3 
_B3:
  ret void 
}