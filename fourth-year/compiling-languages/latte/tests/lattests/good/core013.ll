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


@_g0 =    constant [3 x i8] c"&&\00"


@_g1 =    constant [3 x i8] c"||\00"


@_g2 =    constant [2 x i8] c"!\00"


@_g3 =    constant [6 x i8] c"false\00"


@_g4 =    constant [5 x i8] c"true\00"


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
  %_v0 = bitcast [3 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_v0)  
  br label %IB_1 
IB_1:
   call ccc  void  @printInt(i32  -1)  
  %_iv1 = icmp sgt i32 -1, 0 
  br label %IB_2 
IB_2:
  %_v2 = bitcast i1 %_iv1 to i1 
  br i1 %_v2, label %_B1, label %_B2 
_B1:
  br label %IB_3 
IB_3:
   call ccc  void  @printInt(i32  0)  
  %_iv2 = icmp sgt i32 0, 0 
  br label %IB_4 
IB_4:
  %_v3 = bitcast i1 %_iv2 to i1 
  br label %_B2 
_B2:
  %_v4 = phi i1 [0, %IB_2], [%_v3, %IB_4] 
  br label %IB_5 
IB_5:
  %_iv3 = xor i1 1, %_v4 
  br i1 %_iv3, label %IB_6, label %IB_7 
IB_6:
  %_iv4 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv4)  
  br label %IB_8 
IB_7:
  %_iv5 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv5)  
  br label %IB_8 
IB_8:
  br label %IB_9 
IB_9:
  br label %IB_10 
IB_10:
   call ccc  void  @printInt(i32  -2)  
  %_iv6 = icmp sgt i32 -2, 0 
  br label %IB_11 
IB_11:
  %_v6 = bitcast i1 %_iv6 to i1 
  br i1 %_v6, label %_B3, label %_B4 
_B3:
  br label %IB_12 
IB_12:
   call ccc  void  @printInt(i32  1)  
  %_iv7 = icmp sgt i32 1, 0 
  br label %IB_13 
IB_13:
  %_v7 = bitcast i1 %_iv7 to i1 
  br label %_B4 
_B4:
  %_v8 = phi i1 [0, %IB_11], [%_v7, %IB_13] 
  br label %IB_14 
IB_14:
  %_iv8 = xor i1 1, %_v8 
  br i1 %_iv8, label %IB_15, label %IB_16 
IB_15:
  %_iv9 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv9)  
  br label %IB_17 
IB_16:
  %_iv10 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv10)  
  br label %IB_17 
IB_17:
  br label %IB_18 
IB_18:
  br label %IB_19 
IB_19:
   call ccc  void  @printInt(i32  3)  
  %_iv11 = icmp sgt i32 3, 0 
  br label %IB_20 
IB_20:
  %_v10 = bitcast i1 %_iv11 to i1 
  br i1 %_v10, label %_B5, label %_B6 
_B5:
  br label %IB_21 
IB_21:
   call ccc  void  @printInt(i32  -5)  
  %_iv12 = icmp sgt i32 -5, 0 
  br label %IB_22 
IB_22:
  %_v11 = bitcast i1 %_iv12 to i1 
  br label %_B6 
_B6:
  %_v12 = phi i1 [0, %IB_20], [%_v11, %IB_22] 
  br label %IB_23 
IB_23:
  %_iv13 = xor i1 1, %_v12 
  br i1 %_iv13, label %IB_24, label %IB_25 
IB_24:
  %_iv14 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv14)  
  br label %IB_26 
IB_25:
  %_iv15 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv15)  
  br label %IB_26 
IB_26:
  br label %IB_27 
IB_27:
  br label %IB_28 
IB_28:
   call ccc  void  @printInt(i32  234234)  
  %_iv16 = icmp sgt i32 234234, 0 
  br label %IB_29 
IB_29:
  %_v14 = bitcast i1 %_iv16 to i1 
  br i1 %_v14, label %_B7, label %_B8 
_B7:
  br label %IB_30 
IB_30:
   call ccc  void  @printInt(i32  21321)  
  %_iv17 = icmp sgt i32 21321, 0 
  br label %IB_31 
IB_31:
  %_v15 = bitcast i1 %_iv17 to i1 
  br label %_B8 
_B8:
  %_v16 = phi i1 [0, %IB_29], [%_v15, %IB_31] 
  br label %IB_32 
IB_32:
  %_iv18 = xor i1 1, %_v16 
  br i1 %_iv18, label %IB_33, label %IB_34 
IB_33:
  %_iv19 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv19)  
  br label %IB_35 
IB_34:
  %_iv20 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv20)  
  br label %IB_35 
IB_35:
  br label %IB_36 
IB_36:
  %_v18 = bitcast [3 x i8]* @_g1 to i8* 
   call ccc  void  @printString(i8*  %_v18)  
  br label %IB_37 
IB_37:
   call ccc  void  @printInt(i32  -1)  
  %_iv21 = icmp sgt i32 -1, 0 
  br label %IB_38 
IB_38:
  %_v20 = bitcast i1 %_iv21 to i1 
  br i1 %_v20, label %_B10, label %_B9 
_B9:
  br label %IB_39 
IB_39:
   call ccc  void  @printInt(i32  0)  
  %_iv22 = icmp sgt i32 0, 0 
  br label %IB_40 
IB_40:
  %_v21 = bitcast i1 %_iv22 to i1 
  br label %_B10 
_B10:
  %_v22 = phi i1 [1, %IB_38], [%_v21, %IB_40] 
  br label %IB_41 
IB_41:
  %_iv23 = xor i1 1, %_v22 
  br i1 %_iv23, label %IB_42, label %IB_43 
IB_42:
  %_iv24 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv24)  
  br label %IB_44 
IB_43:
  %_iv25 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv25)  
  br label %IB_44 
IB_44:
  br label %IB_45 
IB_45:
  br label %IB_46 
IB_46:
   call ccc  void  @printInt(i32  -2)  
  %_iv26 = icmp sgt i32 -2, 0 
  br label %IB_47 
IB_47:
  %_v24 = bitcast i1 %_iv26 to i1 
  br i1 %_v24, label %_B12, label %_B11 
_B11:
  br label %IB_48 
IB_48:
   call ccc  void  @printInt(i32  1)  
  %_iv27 = icmp sgt i32 1, 0 
  br label %IB_49 
IB_49:
  %_v25 = bitcast i1 %_iv27 to i1 
  br label %_B12 
_B12:
  %_v26 = phi i1 [1, %IB_47], [%_v25, %IB_49] 
  br label %IB_50 
IB_50:
  %_iv28 = xor i1 1, %_v26 
  br i1 %_iv28, label %IB_51, label %IB_52 
IB_51:
  %_iv29 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv29)  
  br label %IB_53 
IB_52:
  %_iv30 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv30)  
  br label %IB_53 
IB_53:
  br label %IB_54 
IB_54:
  br label %IB_55 
IB_55:
   call ccc  void  @printInt(i32  3)  
  %_iv31 = icmp sgt i32 3, 0 
  br label %IB_56 
IB_56:
  %_v28 = bitcast i1 %_iv31 to i1 
  br i1 %_v28, label %_B14, label %_B13 
_B13:
  br label %IB_57 
IB_57:
   call ccc  void  @printInt(i32  -5)  
  %_iv32 = icmp sgt i32 -5, 0 
  br label %IB_58 
IB_58:
  %_v29 = bitcast i1 %_iv32 to i1 
  br label %_B14 
_B14:
  %_v30 = phi i1 [1, %IB_56], [%_v29, %IB_58] 
  br label %IB_59 
IB_59:
  %_iv33 = xor i1 1, %_v30 
  br i1 %_iv33, label %IB_60, label %IB_61 
IB_60:
  %_iv34 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv34)  
  br label %IB_62 
IB_61:
  %_iv35 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv35)  
  br label %IB_62 
IB_62:
  br label %IB_63 
IB_63:
  br label %IB_64 
IB_64:
   call ccc  void  @printInt(i32  234234)  
  %_iv36 = icmp sgt i32 234234, 0 
  br label %IB_65 
IB_65:
  %_v32 = bitcast i1 %_iv36 to i1 
  br i1 %_v32, label %_B16, label %_B15 
_B15:
  br label %IB_66 
IB_66:
   call ccc  void  @printInt(i32  21321)  
  %_iv37 = icmp sgt i32 21321, 0 
  br label %IB_67 
IB_67:
  %_v33 = bitcast i1 %_iv37 to i1 
  br label %_B16 
_B16:
  %_v34 = phi i1 [1, %IB_65], [%_v33, %IB_67] 
  br label %IB_68 
IB_68:
  %_iv38 = xor i1 1, %_v34 
  br i1 %_iv38, label %IB_69, label %IB_70 
IB_69:
  %_iv39 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv39)  
  br label %IB_71 
IB_70:
  %_iv40 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv40)  
  br label %IB_71 
IB_71:
  br label %IB_72 
IB_72:
  %_v36 = bitcast [2 x i8]* @_g2 to i8* 
   call ccc  void  @printString(i8*  %_v36)  
  br label %IB_73 
IB_73:
  %_iv41 = xor i1 1, 1 
  br i1 %_iv41, label %IB_74, label %IB_75 
IB_74:
  %_iv42 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv42)  
  br label %IB_76 
IB_75:
  %_iv43 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv43)  
  br label %IB_76 
IB_76:
  br label %IB_77 
IB_77:
  br label %IB_78 
IB_78:
  %_iv44 = xor i1 1, 0 
  br i1 %_iv44, label %IB_79, label %IB_80 
IB_79:
  %_iv45 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_iv45)  
  br label %IB_81 
IB_80:
  %_iv46 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_iv46)  
  br label %IB_81 
IB_81:
  br label %IB_82 
IB_82:
  ret i32 0 
}


define external ccc  void @printBool(i1  %b)    {
entry:
  %_v0 = xor i1 1, %b 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  %_v1 = bitcast [6 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_v1)  
  br label %_B3 
_B2:
  %_v3 = bitcast [5 x i8]* @_g4 to i8* 
   call ccc  void  @printString(i8*  %_v3)  
  br label %_B3 
_B3:
  ret void 
}


define external ccc  i1 @test(i32  %i)    {
entry:
   call ccc  void  @printInt(i32  %i)  
  %_v1 = icmp sgt i32 %i, 0 
  ret i1 %_v1 
}