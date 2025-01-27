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


define external ccc  %_arr* @doubleArray(%_arr*  %a)    {
entry:
  %_v0 = getelementptr  %_arr, %_arr* %a, i32 0, i32 1 
  %_v1 = load   i32, i32* %_v0  
  %_v2 =  call ccc  %_arr*  @__arr_malloc()  
  %_v3 = mul   i32 4, %_v1 
  %_v4 =  call ccc  i8*  @_malloc(i32  %_v3)  
  %_v5 = getelementptr  %_arr, %_arr* %_v2, i32 0, i32 0 
  store   i8* %_v4, i8** %_v5  
  %_v6 = getelementptr  %_arr, %_arr* %_v2, i32 0, i32 1 
  store   i32 %_v1, i32* %_v6  
  br label %_B2 
_B1:
  %_v10 = getelementptr  %_arr, %_arr* %a, i32 0, i32 0 
  %_v11 = load   i8*, i8** %_v10  
  %_v12 = bitcast i8* %_v11 to i32* 
  %_v13 = getelementptr  i32, i32* %_v12, i32 %_v8 
  %_v14 = load   i32, i32* %_v13  
  %_v15 = mul   i32 2, %_v14 
  %_v16 = getelementptr  %_arr, %_arr* %_v2, i32 0, i32 0 
  %_v17 = load   i8*, i8** %_v16  
  %_v18 = bitcast i8* %_v17 to i32* 
  %_v19 = getelementptr  i32, i32* %_v18, i32 %_v9 
  store   i32 %_v15, i32* %_v19  
  %_v20 = add   i32 %_v9, 1 
  %_v21 = add   i32 %_v8, 1 
  br label %_B2 
_B2:
  %_v8 = phi i32 [0, %entry], [%_v21, %_B1] 
  %_v9 = phi i32 [0, %entry], [%_v20, %_B1] 
  %_v22 = getelementptr  %_arr, %_arr* %a, i32 0, i32 1 
  %_v23 = load   i32, i32* %_v22  
  %_v24 = icmp slt i32 %_v8, %_v23 
  br i1 %_v24, label %_B1, label %_B3 
_B3:
  ret %_arr* %_v2 
}


define external ccc  void @shiftLeft(%_arr*  %a)    {
entry:
  %_v0 = getelementptr  %_arr, %_arr* %a, i32 0, i32 0 
  %_v1 = load   i8*, i8** %_v0  
  %_v2 = bitcast i8* %_v1 to i32* 
  %_v3 = getelementptr  i32, i32* %_v2, i32 0 
  %_v4 = load   i32, i32* %_v3  
  br label %_B2 
_B1:
  %_v6 = add   i32 %_v5, 1 
  %_v7 = getelementptr  %_arr, %_arr* %a, i32 0, i32 0 
  %_v8 = load   i8*, i8** %_v7  
  %_v9 = bitcast i8* %_v8 to i32* 
  %_v10 = getelementptr  i32, i32* %_v9, i32 %_v6 
  %_v11 = load   i32, i32* %_v10  
  %_v12 = getelementptr  %_arr, %_arr* %a, i32 0, i32 0 
  %_v13 = load   i8*, i8** %_v12  
  %_v14 = bitcast i8* %_v13 to i32* 
  %_v15 = getelementptr  i32, i32* %_v14, i32 %_v5 
  store   i32 %_v11, i32* %_v15  
  %_v16 = add   i32 %_v5, 1 
  br label %_B2 
_B2:
  %_v5 = phi i32 [0, %entry], [%_v16, %_B1] 
  %_v17 = getelementptr  %_arr, %_arr* %a, i32 0, i32 1 
  %_v18 = load   i32, i32* %_v17  
  %_v19 = sub   i32 %_v18, 1 
  %_v20 = icmp slt i32 %_v5, %_v19 
  br i1 %_v20, label %_B1, label %_B3 
_B3:
  %_v21 = getelementptr  %_arr, %_arr* %a, i32 0, i32 1 
  %_v22 = load   i32, i32* %_v21  
  %_v23 = sub   i32 %_v22, 1 
  %_v24 = getelementptr  %_arr, %_arr* %a, i32 0, i32 0 
  %_v25 = load   i8*, i8** %_v24  
  %_v26 = bitcast i8* %_v25 to i32* 
  %_v27 = getelementptr  i32, i32* %_v26, i32 %_v23 
  store   i32 %_v4, i32* %_v27  
  ret void 
}


define external ccc  i32 @scalProd(%_arr*  %a, %_arr*  %b)    {
entry:
  br label %_B2 
_B1:
  %_v2 = getelementptr  %_arr, %_arr* %a, i32 0, i32 0 
  %_v3 = load   i8*, i8** %_v2  
  %_v4 = bitcast i8* %_v3 to i32* 
  %_v5 = getelementptr  i32, i32* %_v4, i32 %_v0 
  %_v6 = load   i32, i32* %_v5  
  %_v7 = getelementptr  %_arr, %_arr* %b, i32 0, i32 0 
  %_v8 = load   i8*, i8** %_v7  
  %_v9 = bitcast i8* %_v8 to i32* 
  %_v10 = getelementptr  i32, i32* %_v9, i32 %_v0 
  %_v11 = load   i32, i32* %_v10  
  %_v12 = mul   i32 %_v6, %_v11 
  %_v13 = add   i32 %_v1, %_v12 
  %_v14 = add   i32 %_v0, 1 
  br label %_B2 
_B2:
  %_v0 = phi i32 [0, %entry], [%_v14, %_B1] 
  %_v1 = phi i32 [0, %entry], [%_v13, %_B1] 
  %_v15 = getelementptr  %_arr, %_arr* %a, i32 0, i32 1 
  %_v16 = load   i32, i32* %_v15  
  %_v17 = icmp slt i32 %_v0, %_v16 
  br i1 %_v17, label %_B1, label %_B3 
_B3:
  ret i32 %_v1 
}


define external ccc  i32 @main()    {
entry:
  %_v0 =  call ccc  %_arr*  @__arr_malloc()  
  %_v1 = mul   i32 4, 5 
  %_v2 =  call ccc  i8*  @_malloc(i32  %_v1)  
  %_v3 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  store   i8* %_v2, i8** %_v3  
  %_v4 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  store   i32 5, i32* %_v4  
  br label %_B2 
_B1:
  %_v6 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v7 = load   i8*, i8** %_v6  
  %_v8 = bitcast i8* %_v7 to i32* 
  %_v9 = getelementptr  i32, i32* %_v8, i32 %_v5 
  store   i32 %_v5, i32* %_v9  
  %_v10 = add   i32 %_v5, 1 
  br label %_B2 
_B2:
  %_v5 = phi i32 [0, %entry], [%_v10, %_B1] 
  %_v11 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  %_v12 = load   i32, i32* %_v11  
  %_v13 = icmp slt i32 %_v5, %_v12 
  br i1 %_v13, label %_B1, label %_B3 
_B3:
   call ccc  void  @shiftLeft(%_arr*  %_v0)  
  %_v15 =  call ccc  %_arr*  @doubleArray(%_arr*  %_v0)  
  br label %_B5 
_B4:
  %_v18 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v19 = load   i8*, i8** %_v18  
  %_v20 = bitcast i8* %_v19 to i32* 
  %_v21 = getelementptr  i32, i32* %_v20, i32 %_v17 
  %_v22 = load   i32, i32* %_v21  
   call ccc  void  @printInt(i32  %_v22)  
  %_v24 = add   i32 %_v17, 1 
  br label %_B5 
_B5:
  %_v17 = phi i32 [0, %_B3], [%_v24, %_B4] 
  %_v25 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  %_v26 = load   i32, i32* %_v25  
  %_v27 = icmp slt i32 %_v17, %_v26 
  br i1 %_v27, label %_B4, label %_B6 
_B6:
  br label %_B8 
_B7:
  %_v30 = getelementptr  %_arr, %_arr* %_v15, i32 0, i32 0 
  %_v31 = load   i8*, i8** %_v30  
  %_v32 = bitcast i8* %_v31 to i32* 
  %_v33 = getelementptr  i32, i32* %_v32, i32 %_v29 
  %_v34 = load   i32, i32* %_v33  
   call ccc  void  @printInt(i32  %_v34)  
  %_v36 = add   i32 %_v29, 1 
  br label %_B8 
_B8:
  %_v29 = phi i32 [0, %_B6], [%_v36, %_B7] 
  %_v37 = getelementptr  %_arr, %_arr* %_v15, i32 0, i32 1 
  %_v38 = load   i32, i32* %_v37  
  %_v39 = icmp slt i32 %_v29, %_v38 
  br i1 %_v39, label %_B7, label %_B9 
_B9:
  %_v40 =  call ccc  i32  @scalProd(%_arr*  %_v0, %_arr*  %_v15)  
   call ccc  void  @printInt(i32  %_v40)  
  ret i32 0 
}