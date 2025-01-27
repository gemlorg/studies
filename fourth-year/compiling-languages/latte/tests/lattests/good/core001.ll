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


@_g0 =    constant [1 x i8] c"\00"


@_g1 =    constant [2 x i8] c"=\00"


@_g2 =    constant [9 x i8] c"hello */\00"


@_g3 =    constant [9 x i8] c"/* world\00"


@_g4 =    constant [1 x i8] c"\00"


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
  %_iv3 = phi i32 [10, %IB_1], [%_iv2, %IB_2] 
  %_iv4 = phi i32 [1, %IB_1], [%_iv1, %IB_2] 
  %_iv5 = icmp sgt i32 %_iv3, 0 
  br i1 %_iv5, label %IB_2, label %IB_4 
IB_4:
  br label %IB_5 
IB_5:
  %_v0 = bitcast i32 %_iv4 to i32 
   call ccc  void  @printInt(i32  %_v0)  
  %_v2 =  call ccc  i32  @rfac(i32  10)  
   call ccc  void  @printInt(i32  %_v2)  
  %_v4 =  call ccc  i32  @mfac(i32  10)  
   call ccc  void  @printInt(i32  %_v4)  
  %_v6 =  call ccc  i32  @ifac(i32  10)  
   call ccc  void  @printInt(i32  %_v6)  
  %_v8 = bitcast [1 x i8]* @_g0 to i8* 
  br label %_B2 
_B1:
  %_v11 = mul   i32 %_v10, %_v9 
  %_v12 = sub   i32 %_v9, 1 
  br label %_B2 
_B2:
  %_v9 = phi i32 [10, %IB_5], [%_v12, %_B1] 
  %_v10 = phi i32 [1, %IB_5], [%_v11, %_B1] 
  %_v13 = icmp sgt i32 %_v9, 0 
  br i1 %_v13, label %_B1, label %_B3 
_B3:
   call ccc  void  @printInt(i32  %_v10)  
  %_v15 = bitcast [2 x i8]* @_g1 to i8* 
  br label %IB_6 
IB_6:
  %_iv6 = bitcast [1 x i8]* @_g4 to i8* 
  br label %IB_8 
IB_7:
  %_iv7 =  call ccc  i8*  @_concatStrings(i8*  %_iv10, i8*  %_v15)  
  %_iv8 = add   i32 %_iv9, 1 
  br label %IB_8 
IB_8:
  %_iv9 = phi i32 [0, %IB_6], [%_iv8, %IB_7] 
  %_iv10 = phi i8* [%_iv6, %IB_6], [%_iv7, %IB_7] 
  %_iv11 = icmp slt i32 %_iv9, 60 
  br i1 %_iv11, label %IB_7, label %IB_9 
IB_9:
  br label %IB_10 
IB_10:
  %_v16 = bitcast i8* %_iv10 to i8* 
   call ccc  void  @printString(i8*  %_v16)  
  %_v18 = bitcast [9 x i8]* @_g2 to i8* 
   call ccc  void  @printString(i8*  %_v18)  
  %_v20 = bitcast [9 x i8]* @_g3 to i8* 
   call ccc  void  @printString(i8*  %_v20)  
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


define external ccc  i32 @rfac(i32  %n)    {
entry:
  %_v0 = icmp eq i32 %n, 0 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  ret i32 1 
_B2:
  %_v1 = sub   i32 %n, 1 
  %_v2 =  call ccc  i32  @rfac(i32  %_v1)  
  %_v3 = mul   i32 %n, %_v2 
  ret i32 %_v3 
}


define external ccc  i32 @mfac(i32  %n)    {
entry:
  %_v0 = icmp eq i32 %n, 0 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  ret i32 1 
_B2:
  %_v1 = sub   i32 %n, 1 
  %_v2 =  call ccc  i32  @nfac(i32  %_v1)  
  %_v3 = mul   i32 %n, %_v2 
  ret i32 %_v3 
}


define external ccc  i32 @nfac(i32  %n)    {
entry:
  %_v0 = icmp ne i32 %n, 0 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  %_v1 = sub   i32 %n, 1 
  %_v2 =  call ccc  i32  @mfac(i32  %_v1)  
  %_v3 = mul   i32 %_v2, %n 
  ret i32 %_v3 
_B2:
  ret i32 1 
}


define external ccc  i32 @ifac(i32  %n)    {
entry:
  %_v0 =  call ccc  i32  @ifac2f(i32  1, i32  %n)  
  ret i32 %_v0 
}


define external ccc  i32 @ifac2f(i32  %l, i32  %h)    {
entry:
  %_v0 = icmp eq i32 %l, %h 
  br i1 %_v0, label %_B1, label %_B2 
_B1:
  ret i32 %l 
_B2:
  %_v1 = icmp sgt i32 %l, %h 
  br i1 %_v1, label %_B3, label %_B4 
_B3:
  ret i32 1 
_B4:
  %_v2 = add   i32 %l, %h 
  %_v3 = sdiv  i32 %_v2, 2 
  %_v4 =  call ccc  i32  @ifac2f(i32  %l, i32  %_v3)  
  %_v5 = add   i32 %_v3, 1 
  %_v6 =  call ccc  i32  @ifac2f(i32  %_v5, i32  %h)  
  %_v7 = mul   i32 %_v4, %_v6 
  ret i32 %_v7 
}


define external ccc  i8* @repStr(i8*  %s, i32  %n)    {
entry:
  %_v0 = bitcast [1 x i8]* @_g4 to i8* 
  br label %_B2 
_B1:
  %_v3 =  call ccc  i8*  @_concatStrings(i8*  %_v2, i8*  %s)  
  %_v4 = add   i32 %_v1, 1 
  br label %_B2 
_B2:
  %_v1 = phi i32 [0, %entry], [%_v4, %_B1] 
  %_v2 = phi i8* [%_v0, %entry], [%_v3, %_B1] 
  %_v5 = icmp slt i32 %_v1, %n 
  br i1 %_v5, label %_B1, label %_B3 
_B3:
  ret i8* %_v2 
}