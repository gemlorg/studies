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
  br label %_B5 
_B4:
  %_v16 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v17 = load   i8*, i8** %_v16  
  %_v18 = bitcast i8* %_v17 to i32* 
  %_v19 = getelementptr  i32, i32* %_v18, i32 %_v15 
  %_v20 = load   i32, i32* %_v19  
   call ccc  void  @printInt(i32  %_v20)  
  %_v22 = add   i32 %_v15, 1 
  br label %_B5 
_B5:
  %_v15 = phi i32 [0, %_B3], [%_v22, %_B4] 
  %_v23 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  %_v24 = load   i32, i32* %_v23  
  %_v25 = icmp slt i32 %_v15, %_v24 
  br i1 %_v25, label %_B4, label %_B6 
_B6:
   call ccc  void  @printInt(i32  45)  
  ret i32 0 
}