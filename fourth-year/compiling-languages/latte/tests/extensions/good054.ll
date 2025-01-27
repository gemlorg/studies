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


@_g0 =    constant [13 x i8] c"hello from f\00"


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
  %_v0 =  call ccc  %_arr*  @__arr_malloc()  
  %_v1 = mul   i32 4, 10 
  %_v2 =  call ccc  i8*  @_malloc(i32  %_v1)  
  %_v3 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  store   i8* %_v2, i8** %_v3  
  %_v4 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  store   i32 10, i32* %_v4  
  %_v5 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v6 = load   i8*, i8** %_v5  
  %_v7 = bitcast i8* %_v6 to i32* 
  %_v8 = getelementptr  i32, i32* %_v7, i32 0 
  store   i32 13, i32* %_v8  
  %_v9 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v10 = load   i8*, i8** %_v9  
  %_v11 = bitcast i8* %_v10 to i32* 
  %_v12 = getelementptr  i32, i32* %_v11, i32 1 
  store   i32 1, i32* %_v12  
  %_v13 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v14 = load   i8*, i8** %_v13  
  %_v15 = bitcast i8* %_v14 to i32* 
  %_v16 = getelementptr  i32, i32* %_v15, i32 4 
  store   i32 4, i32* %_v16  
  %_v17 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v18 = load   i8*, i8** %_v17  
  %_v19 = bitcast i8* %_v18 to i32* 
  %_v20 = getelementptr  i32, i32* %_v19, i32 9 
  store   i32 9, i32* %_v20  
  %_v21 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v22 = load   i8*, i8** %_v21  
  %_v23 = bitcast i8* %_v22 to i32* 
  %_v24 = getelementptr  i32, i32* %_v23, i32 0 
  %_v25 = load   i32, i32* %_v24  
   call ccc  void  @printInt(i32  %_v25)  
  %_v27 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v28 = load   i8*, i8** %_v27  
  %_v29 = bitcast i8* %_v28 to i32* 
  %_v30 = getelementptr  i32, i32* %_v29, i32 9 
  %_v31 = load   i32, i32* %_v30  
   call ccc  void  @printInt(i32  %_v31)  
  %_v33 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  %_v34 = load   i32, i32* %_v33  
   call ccc  void  @printInt(i32  %_v34)  
  br label %_B2 
_B1:
  %_v38 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v39 = load   i8*, i8** %_v38  
  %_v40 = bitcast i8* %_v39 to i32* 
  %_v41 = getelementptr  i32, i32* %_v40, i32 %_v37 
  %_v42 = load   i32, i32* %_v41  
   call ccc  void  @printInt(i32  %_v42)  
  br label %IB_1 
IB_1:
  %_iv1 = bitcast [13 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_iv1)  
  br label %IB_2 
IB_2:
  %_v44 = bitcast i32 %_v42 to i32 
  %_v45 = add   i32 %_v37, 1 
  br label %_B2 
_B2:
  %_v37 = phi i32 [0, %entry], [%_v45, %IB_2] 
  %_v46 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  %_v47 = load   i32, i32* %_v46  
  %_v48 = icmp slt i32 %_v37, %_v47 
  br i1 %_v48, label %_B1, label %_B3 
_B3:
  ret i32 0 
}


define external ccc  i32 @f(i32  %i)    {
entry:
  %_v0 = bitcast [13 x i8]* @_g0 to i8* 
   call ccc  void  @printString(i8*  %_v0)  
  ret i32 %i 
}