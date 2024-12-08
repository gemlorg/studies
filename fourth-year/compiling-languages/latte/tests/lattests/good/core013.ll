; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v1 = private   constant [3 x i8] c"&&\00"


@_v20 = private   constant [3 x i8] c"||\00"


@_v39 = private   constant [2 x i8] c"!\00"


@_v45 = private   constant [6 x i8] c"false\00"


@_v48 = private   constant [5 x i8] c"true\00"


define external ccc  i32 @main()    {
entry:
  %_v2 = bitcast [3 x i8]* @_v1 to i8* 
   call ccc  void  @printString(i8*  %_v2)  
  %_v4 =  call ccc  i1  @test(i32  -1)  
  br i1 %_v4, label %_B1, label %_B2 
_B1:
  %_v5 =  call ccc  i1  @test(i32  0)  
  br label %_B2 
_B2:
  %_v6 = phi i1 [0, %entry], [%_v5, %_B1] 
   call ccc  void  @printBool(i1  %_v6)  
  %_v8 =  call ccc  i1  @test(i32  -2)  
  br i1 %_v8, label %_B3, label %_B4 
_B3:
  %_v9 =  call ccc  i1  @test(i32  1)  
  br label %_B4 
_B4:
  %_v10 = phi i1 [0, %_B2], [%_v9, %_B3] 
   call ccc  void  @printBool(i1  %_v10)  
  %_v12 =  call ccc  i1  @test(i32  3)  
  br i1 %_v12, label %_B5, label %_B6 
_B5:
  %_v13 =  call ccc  i1  @test(i32  -5)  
  br label %_B6 
_B6:
  %_v14 = phi i1 [0, %_B4], [%_v13, %_B5] 
   call ccc  void  @printBool(i1  %_v14)  
  %_v16 =  call ccc  i1  @test(i32  234234)  
  br i1 %_v16, label %_B7, label %_B8 
_B7:
  %_v17 =  call ccc  i1  @test(i32  21321)  
  br label %_B8 
_B8:
  %_v18 = phi i1 [0, %_B6], [%_v17, %_B7] 
   call ccc  void  @printBool(i1  %_v18)  
  %_v21 = bitcast [3 x i8]* @_v20 to i8* 
   call ccc  void  @printString(i8*  %_v21)  
  %_v23 =  call ccc  i1  @test(i32  -1)  
  br i1 %_v23, label %_B10, label %_B9 
_B9:
  %_v24 =  call ccc  i1  @test(i32  0)  
  br label %_B10 
_B10:
  %_v25 = phi i1 [1, %_B8], [%_v24, %_B9] 
   call ccc  void  @printBool(i1  %_v25)  
  %_v27 =  call ccc  i1  @test(i32  -2)  
  br i1 %_v27, label %_B12, label %_B11 
_B11:
  %_v28 =  call ccc  i1  @test(i32  1)  
  br label %_B12 
_B12:
  %_v29 = phi i1 [1, %_B10], [%_v28, %_B11] 
   call ccc  void  @printBool(i1  %_v29)  
  %_v31 =  call ccc  i1  @test(i32  3)  
  br i1 %_v31, label %_B14, label %_B13 
_B13:
  %_v32 =  call ccc  i1  @test(i32  -5)  
  br label %_B14 
_B14:
  %_v33 = phi i1 [1, %_B12], [%_v32, %_B13] 
   call ccc  void  @printBool(i1  %_v33)  
  %_v35 =  call ccc  i1  @test(i32  234234)  
  br i1 %_v35, label %_B16, label %_B15 
_B15:
  %_v36 =  call ccc  i1  @test(i32  21321)  
  br label %_B16 
_B16:
  %_v37 = phi i1 [1, %_B14], [%_v36, %_B15] 
   call ccc  void  @printBool(i1  %_v37)  
  %_v40 = bitcast [2 x i8]* @_v39 to i8* 
   call ccc  void  @printString(i8*  %_v40)  
   call ccc  void  @printBool(i1  1)  
   call ccc  void  @printBool(i1  0)  
  ret i32 0 
}


define external ccc  void @printBool(i1  %b)    {
entry:
  %_v44 = xor i1 1, %b 
  br i1 %_v44, label %_B17, label %_B18 
_B17:
  %_v46 = bitcast [6 x i8]* @_v45 to i8* 
   call ccc  void  @printString(i8*  %_v46)  
  br label %_B19 
_B18:
  %_v49 = bitcast [5 x i8]* @_v48 to i8* 
   call ccc  void  @printString(i8*  %_v49)  
  br label %_B19 
_B19:
  ret void 
}


define external ccc  i1 @test(i32  %i)    {
entry:
   call ccc  void  @printInt(i32  %i)  
  %_v52 = icmp sgt i32 %i, 0 
  ret i1 %_v52 
}