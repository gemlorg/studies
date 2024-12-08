; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v32 = private   constant [5 x i8] c"true\00"


@_v35 = private   constant [6 x i8] c"false\00"


define external ccc  i32 @main()    {
entry:
  %_v1 =  call ccc  i1  @t(i32  1)  
  br i1 %_v1, label %_B1, label %_B2 
_B1:
  %_v2 =  call ccc  i1  @f(i32  2)  
  br label %_B2 
_B2:
  %_v3 = phi i1 [0, %entry], [%_v2, %_B1] 
   call ccc  void  @b(i1  %_v3)  
  %_v5 =  call ccc  i1  @t(i32  3)  
  br i1 %_v5, label %_B3, label %_B4 
_B3:
  %_v6 =  call ccc  i1  @t(i32  4)  
  br label %_B4 
_B4:
  %_v7 = phi i1 [0, %_B2], [%_v6, %_B3] 
   call ccc  void  @b(i1  %_v7)  
  %_v9 =  call ccc  i1  @t(i32  5)  
  br i1 %_v9, label %_B6, label %_B5 
_B5:
  %_v10 =  call ccc  i1  @t(i32  6)  
  br label %_B6 
_B6:
  %_v11 = phi i1 [1, %_B4], [%_v10, %_B5] 
   call ccc  void  @b(i1  %_v11)  
  %_v13 =  call ccc  i1  @f(i32  7)  
  br i1 %_v13, label %_B7, label %_B8 
_B7:
  %_v14 =  call ccc  i1  @t(i32  8)  
  br label %_B8 
_B8:
  %_v15 = phi i1 [0, %_B6], [%_v14, %_B7] 
   call ccc  void  @b(i1  %_v15)  
  %_v17 =  call ccc  i1  @t(i32  9)  
  br i1 %_v17, label %_B9, label %_B10 
_B9:
  %_v18 =  call ccc  i1  @t(i32  10)  
  br i1 %_v18, label %_B11, label %_B12 
_B11:
  %_v19 =  call ccc  i1  @t(i32  11)  
  br label %_B12 
_B12:
  %_v20 = phi i1 [0, %_B9], [%_v19, %_B11] 
  br label %_B10 
_B10:
  %_v21 = phi i1 [0, %_B8], [%_v20, %_B12] 
   call ccc  void  @b(i1  %_v21)  
  %_v23 =  call ccc  i1  @f(i32  12)  
  br i1 %_v23, label %_B14, label %_B13 
_B13:
  %_v24 =  call ccc  i1  @f(i32  13)  
  br i1 %_v24, label %_B15, label %_B16 
_B15:
  %_v25 =  call ccc  i1  @t(i32  14)  
  br label %_B16 
_B16:
  %_v26 = phi i1 [0, %_B13], [%_v25, %_B15] 
  br label %_B14 
_B14:
  %_v27 = phi i1 [1, %_B10], [%_v26, %_B16] 
   call ccc  void  @b(i1  %_v27)  
  ret i32 0 
}


define external ccc  i1 @f(i32  %a)    {
entry:
   call ccc  void  @printInt(i32  %a)  
  ret i1 0 
}


define external ccc  i1 @t(i32  %a)    {
entry:
  %_v30 =  call ccc  i1  @f(i32  %a)  
  %_v31 = xor i1 1, %_v30 
  ret i1 %_v31 
}


define external ccc  void @b(i1  %a)    {
entry:
  br i1 %a, label %_B17, label %_B18 
_B17:
  %_v33 = bitcast [5 x i8]* @_v32 to i8* 
   call ccc  void  @printString(i8*  %_v33)  
  br label %_B19 
_B18:
  %_v36 = bitcast [6 x i8]* @_v35 to i8* 
   call ccc  void  @printString(i8*  %_v36)  
  br label %_B19 
_B19:
  ret void 
}