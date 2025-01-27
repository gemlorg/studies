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
  br label %IB_1 
IB_1:
  br i1 1, label %IB_2, label %IB_3 
IB_2:
  %_iv1 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv1)  
  br label %IB_4 
IB_3:
  %_iv2 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv2)  
  br label %IB_4 
IB_4:
  br label %IB_5 
IB_5:
  br label %IB_6 
IB_6:
  br i1 1, label %IB_7, label %IB_8 
IB_7:
  %_iv3 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv3)  
  br label %IB_9 
IB_8:
  %_iv4 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv4)  
  br label %IB_9 
IB_9:
  br label %IB_10 
IB_10:
  br label %IB_11 
IB_11:
  br i1 0, label %IB_12, label %IB_13 
IB_12:
  %_iv5 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv5)  
  br label %IB_14 
IB_13:
  %_iv6 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv6)  
  br label %IB_14 
IB_14:
  br label %IB_15 
IB_15:
  br label %IB_16 
IB_16:
  br i1 1, label %IB_17, label %IB_18 
IB_17:
  %_iv7 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv7)  
  br label %IB_19 
IB_18:
  %_iv8 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv8)  
  br label %IB_19 
IB_19:
  br label %IB_20 
IB_20:
  br label %IB_21 
IB_21:
  %_iv9 = xor i1 1, 0 
  br i1 %_iv9, label %IB_23, label %IB_22 
IB_22:
  %_iv10 = icmp eq i1 0, 0 
  br label %IB_23 
IB_23:
  %_iv11 = phi i1 [1, %IB_21], [%_iv10, %IB_22] 
  br label %IB_24 
IB_24:
  %_v10 = bitcast i1 %_iv11 to i1 
  br label %IB_25 
IB_25:
  br i1 %_v10, label %IB_26, label %IB_27 
IB_26:
  %_iv12 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv12)  
  br label %IB_28 
IB_27:
  %_iv13 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv13)  
  br label %IB_28 
IB_28:
  br label %IB_29 
IB_29:
  br label %IB_30 
IB_30:
  %_iv14 = xor i1 1, 0 
  br i1 %_iv14, label %IB_32, label %IB_31 
IB_31:
  %_iv15 = icmp eq i1 0, 1 
  br label %IB_32 
IB_32:
  %_iv16 = phi i1 [1, %IB_30], [%_iv15, %IB_31] 
  br label %IB_33 
IB_33:
  %_v12 = bitcast i1 %_iv16 to i1 
  br label %IB_34 
IB_34:
  br i1 %_v12, label %IB_35, label %IB_36 
IB_35:
  %_iv17 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv17)  
  br label %IB_37 
IB_36:
  %_iv18 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv18)  
  br label %IB_37 
IB_37:
  br label %IB_38 
IB_38:
  br label %IB_39 
IB_39:
  %_iv19 = xor i1 1, 1 
  br i1 %_iv19, label %IB_41, label %IB_40 
IB_40:
  %_iv20 = icmp eq i1 1, 0 
  br label %IB_41 
IB_41:
  %_iv21 = phi i1 [1, %IB_39], [%_iv20, %IB_40] 
  br label %IB_42 
IB_42:
  %_v14 = bitcast i1 %_iv21 to i1 
  br label %IB_43 
IB_43:
  br i1 %_v14, label %IB_44, label %IB_45 
IB_44:
  %_iv22 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv22)  
  br label %IB_46 
IB_45:
  %_iv23 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv23)  
  br label %IB_46 
IB_46:
  br label %IB_47 
IB_47:
  br label %IB_48 
IB_48:
  %_iv24 = xor i1 1, 1 
  br i1 %_iv24, label %IB_50, label %IB_49 
IB_49:
  %_iv25 = icmp eq i1 1, 1 
  br label %IB_50 
IB_50:
  %_iv26 = phi i1 [1, %IB_48], [%_iv25, %IB_49] 
  br label %IB_51 
IB_51:
  %_v16 = bitcast i1 %_iv26 to i1 
  br label %IB_52 
IB_52:
  br i1 %_v16, label %IB_53, label %IB_54 
IB_53:
  %_iv27 = bitcast [5 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv27)  
  br label %IB_55 
IB_54:
  %_iv28 = bitcast [6 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_iv28)  
  br label %IB_55 
IB_55:
  br label %IB_56 
IB_56:
  ret i32 0 
}


define external ccc  i1 @dontCallMe(i32  %x)    {
entry:
   call ccc  void  @printInt(i32  %x)  
  ret i1 1 
}


define external ccc  void @printBool(i1  %b)    {
entry:
  br i1 %b, label %_B1, label %_B2 
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


define external ccc  i1 @implies(i1  %x, i1  %y)    {
entry:
  %_v0 = xor i1 1, %x 
  br i1 %_v0, label %_B2, label %_B1 
_B1:
  %_v1 = icmp eq i1 %x, %y 
  br label %_B2 
_B2:
  %_v2 = phi i1 [1, %entry], [%_v1, %_B1] 
  ret i1 %_v2 
}