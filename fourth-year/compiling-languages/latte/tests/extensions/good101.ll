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


@_g0 =    constant [2 x i8] c"a\00"


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
  %_v1 = mul   i32 8, 10 
  %_v2 =  call ccc  i8*  @_malloc(i32  %_v1)  
  %_v3 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  store   i8* %_v2, i8** %_v3  
  %_v4 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 1 
  store   i32 10, i32* %_v4  
  %_v5 = bitcast [2 x i8]* @_g0 to i8* 
  br label %_B2 
_B1:
  %_v7 =  call ccc  i8*  @_concatStrings(i8*  %_v5, i8*  %_v5)  
  %_v8 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v9 = load   i8*, i8** %_v8  
  %_v10 = bitcast i8* %_v9 to i8** 
  %_v11 = getelementptr  i8*, i8** %_v10, i32 %_v6 
  store   i8* %_v7, i8** %_v11  
  %_v12 = getelementptr  %_arr, %_arr* %_v0, i32 0, i32 0 
  %_v13 = load   i8*, i8** %_v12  
  %_v14 = bitcast i8* %_v13 to i8** 
  %_v15 = getelementptr  i8*, i8** %_v14, i32 %_v6 
  %_v16 = load   i8*, i8** %_v15  
   call ccc  void  @printString(i8*  %_v16)  
  %_v18 = add   i32 %_v6, 1 
  br label %_B2 
_B2:
  %_v6 = phi i32 [0, %entry], [%_v18, %_B1] 
  %_v19 = icmp slt i32 %_v6, 10 
  br i1 %_v19, label %_B1, label %_B3 
_B3:
  ret i32 0 
}