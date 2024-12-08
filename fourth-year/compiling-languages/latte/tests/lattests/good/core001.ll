; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v10 = private   constant [1 x i8] c"\00"


@_v17 = private   constant [2 x i8] c"=\00"


@_v21 = private   constant [9 x i8] c"hello */\00"


@_v24 = private   constant [9 x i8] c"/* world\00"


@_v53 = private   constant [1 x i8] c"\00"


define external ccc  i32 @main()    {
entry:
  %_v1 =  call ccc  i32  @fac(i32  10)  
   call ccc  void  @printInt(i32  %_v1)  
  %_v3 =  call ccc  i32  @rfac(i32  10)  
   call ccc  void  @printInt(i32  %_v3)  
  %_v5 =  call ccc  i32  @mfac(i32  10)  
   call ccc  void  @printInt(i32  %_v5)  
  %_v7 =  call ccc  i32  @ifac(i32  10)  
   call ccc  void  @printInt(i32  %_v7)  
  %_v9 = bitcast [1 x i8]* @_v10 to i8* 
  br label %_B2 
_B1:
  %_v13 = mul   i32 %_v12, %_v11 
  %_v14 = sub   i32 %_v11, 1 
  br label %_B2 
_B2:
  %_v11 = phi i32 [10, %entry], [%_v14, %_B1] 
  %_v12 = phi i32 [1, %entry], [%_v13, %_B1] 
  %_v15 = icmp sgt i32 %_v11, 0 
  br i1 %_v15, label %_B1, label %_B3 
_B3:
   call ccc  void  @printInt(i32  %_v12)  
  %_v18 = bitcast [2 x i8]* @_v17 to i8* 
  %_v19 =  call ccc  i8*  @repStr(i8*  %_v18, i32  60)  
   call ccc  void  @printString(i8*  %_v19)  
  %_v22 = bitcast [9 x i8]* @_v21 to i8* 
   call ccc  void  @printString(i8*  %_v22)  
  %_v25 = bitcast [9 x i8]* @_v24 to i8* 
   call ccc  void  @printString(i8*  %_v25)  
  ret i32 0 
}


define external ccc  i32 @fac(i32  %a)    {
entry:
  br label %_B5 
_B4:
  %_v29 = mul   i32 %_v28, %_v27 
  %_v30 = sub   i32 %_v27, 1 
  br label %_B5 
_B5:
  %_v27 = phi i32 [%a, %entry], [%_v30, %_B4] 
  %_v28 = phi i32 [1, %entry], [%_v29, %_B4] 
  %_v31 = icmp sgt i32 %_v27, 0 
  br i1 %_v31, label %_B4, label %_B6 
_B6:
  ret i32 %_v28 
}


define external ccc  i32 @rfac(i32  %n)    {
entry:
  %_v32 = icmp eq i32 %n, 0 
  br i1 %_v32, label %_B7, label %_B8 
_B7:
  ret i32 1 
_B8:
  %_v33 = sub   i32 %n, 1 
  %_v34 =  call ccc  i32  @rfac(i32  %_v33)  
  %_v35 = mul   i32 %n, %_v34 
  ret i32 %_v35 
}


define external ccc  i32 @mfac(i32  %n)    {
entry:
  %_v36 = icmp eq i32 %n, 0 
  br i1 %_v36, label %_B10, label %_B11 
_B10:
  ret i32 1 
_B11:
  %_v37 = sub   i32 %n, 1 
  %_v38 =  call ccc  i32  @nfac(i32  %_v37)  
  %_v39 = mul   i32 %n, %_v38 
  ret i32 %_v39 
}


define external ccc  i32 @nfac(i32  %n)    {
entry:
  %_v40 = icmp ne i32 %n, 0 
  br i1 %_v40, label %_B13, label %_B14 
_B13:
  %_v41 = sub   i32 %n, 1 
  %_v42 =  call ccc  i32  @mfac(i32  %_v41)  
  %_v43 = mul   i32 %_v42, %n 
  ret i32 %_v43 
_B14:
  ret i32 1 
}


define external ccc  i32 @ifac(i32  %n)    {
entry:
  %_v44 =  call ccc  i32  @ifac2f(i32  1, i32  %n)  
  ret i32 %_v44 
}


define external ccc  i32 @ifac2f(i32  %l, i32  %h)    {
entry:
  %_v45 = icmp eq i32 %l, %h 
  br i1 %_v45, label %_B16, label %_B17 
_B16:
  ret i32 %l 
_B17:
  %_v46 = icmp sgt i32 %l, %h 
  br i1 %_v46, label %_B18, label %_B19 
_B18:
  ret i32 1 
_B19:
  %_v47 = add   i32 %l, %h 
  %_v48 = sdiv  i32 %_v47, 2 
  %_v49 =  call ccc  i32  @ifac2f(i32  %l, i32  %_v48)  
  %_v50 = add   i32 %_v48, 1 
  %_v51 =  call ccc  i32  @ifac2f(i32  %_v50, i32  %h)  
  %_v52 = mul   i32 %_v49, %_v51 
  ret i32 %_v52 
}


define external ccc  i8* @repStr(i8*  %s, i32  %n)    {
entry:
  %_v54 = bitcast [1 x i8]* @_v53 to i8* 
  br label %_B21 
_B20:
  %_v57 =  call ccc  i8*  @_concatStrings(i8*  %_v56, i8*  %s)  
  %_v58 = add   i32 %_v55, 1 
  br label %_B21 
_B21:
  %_v55 = phi i32 [0, %entry], [%_v58, %_B20] 
  %_v56 = phi i8* [%_v54, %entry], [%_v57, %_B20] 
  %_v59 = icmp slt i32 %_v55, %n 
  br i1 %_v59, label %_B20, label %_B22 
_B22:
  ret i8* %_v56 
}