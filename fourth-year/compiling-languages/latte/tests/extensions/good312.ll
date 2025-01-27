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
  %_v8 = getelementptr  i32, i32* %_v7, i32 3 
  %_v9 = load   i32, i32* %_v8  
  %_v10 = add   i32 %_v9, 1 
  %_v11 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v12 = load   i8*, i8** %_v11  
  %_v13 = bitcast i8* %_v12 to i32* 
  %_v14 = getelementptr  i32, i32* %_v13, i32 3 
  store   i32 %_v10, i32* %_v14  
  %_v15 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v16 = load   i8*, i8** %_v15  
  %_v17 = bitcast i8* %_v16 to i32* 
  %_v18 = getelementptr  i32, i32* %_v17, i32 3 
  %_v19 = load   i32, i32* %_v18  
  %_v20 = add   i32 %_v19, 1 
  %_v21 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v22 = load   i8*, i8** %_v21  
  %_v23 = bitcast i8* %_v22 to i32* 
  %_v24 = getelementptr  i32, i32* %_v23, i32 3 
  store   i32 %_v20, i32* %_v24  
  %_v25 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v26 = load   i8*, i8** %_v25  
  %_v27 = bitcast i8* %_v26 to i32* 
  %_v28 = getelementptr  i32, i32* %_v27, i32 3 
  %_v29 = load   i32, i32* %_v28  
   call ccc  void  @printInt(i32  %_v29)  
  %_v31 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v32 = load   i8*, i8** %_v31  
  %_v33 = bitcast i8* %_v32 to i32* 
  %_v34 = getelementptr  i32, i32* %_v33, i32 0 
  %_v35 = load   i32, i32* %_v34  
   call ccc  void  @printInt(i32  %_v35)  
  ret i32 0 
}