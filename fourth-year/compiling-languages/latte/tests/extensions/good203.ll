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


@_g1 =    constant [6 x i8] c"hello\00"


@_g2 =    constant [8 x i8] c" there!\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


%A = type {%_A_vtype*, i32, i8*, %_arr*, i8*, i32, i1, %_arr*}


%_A_vtype = type {}


@_A_vdata =    global %_A_vtype {  }


define external ccc  %A* @_A_malloc()    {
entry:
  %0 = getelementptr inbounds %A, %A* zeroinitializer, i32 1 
  %1 = ptrtoint %A* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %A* 
  ret %A* %3 
}


define external ccc  i32 @main()    {
entry:
  %_v3 = bitcast [1 x i8]* @_g0 to i8* 
  %_v6 = bitcast [1 x i8]* @_g0 to i8* 
  %_v0 =  call ccc  %A*  @_A_malloc()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v1  
  %_v2 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i32 0, i32* %_v2  
  %_v4 = getelementptr  %A, %A* %_v0, i32 0, i32 2 
  store   i8* %_v3, i8** %_v4  
  %_v5 = getelementptr  %A, %A* %_v0, i32 0, i32 3 
  store   %_arr* zeroinitializer, %_arr** %_v5  
  %_v7 = getelementptr  %A, %A* %_v0, i32 0, i32 4 
  store   i8* %_v6, i8** %_v7  
  %_v8 = getelementptr  %A, %A* %_v0, i32 0, i32 5 
  store   i32 0, i32* %_v8  
  %_v9 = getelementptr  %A, %A* %_v0, i32 0, i32 6 
  store   i1 0, i1* %_v9  
  %_v10 = getelementptr  %A, %A* %_v0, i32 0, i32 7 
  store   %_arr* zeroinitializer, %_arr** %_v10  
  %_v11 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v11  
  store   %A* %_v0, %A** %_v11  
  %_v12 = bitcast [6 x i8]* @_g1 to i8* 
  %_v13 = load   %A*, %A** %_v11  
  %_v14 = getelementptr  %A, %A* %_v13, i32 0, i32 2 
  store   i8* %_v12, i8** %_v14  
  %_v15 = load   %A*, %A** %_v11  
  %_v16 = getelementptr  %A, %A* %_v15, i32 0, i32 2 
  %_v17 = load   i8*, i8** %_v16  
  %_v18 = bitcast [8 x i8]* @_g2 to i8* 
  %_v19 =  call ccc  i8*  @_concatStrings(i8*  %_v17, i8*  %_v18)  
  %_v20 = load   %A*, %A** %_v11  
  %_v21 = getelementptr  %A, %A* %_v20, i32 0, i32 4 
  store   i8* %_v19, i8** %_v21  
  %_v22 = load   %A*, %A** %_v11  
  %_v23 = getelementptr  %A, %A* %_v22, i32 0, i32 6 
  store   i1 1, i1* %_v23  
  %_v24 =  call ccc  %_arr*  @__arr_malloc()  
  %_v25 = mul   i32 4, 42 
  %_v26 =  call ccc  i8*  @_malloc(i32  %_v25)  
  %_v27 = getelementptr  %_arr, %_arr* %_v24, i32 0, i32 0 
  store   i8* %_v26, i8** %_v27  
  %_v28 = getelementptr  %_arr, %_arr* %_v24, i32 0, i32 1 
  store   i32 42, i32* %_v28  
  %_v29 = load   %A*, %A** %_v11  
  %_v30 = getelementptr  %A, %A* %_v29, i32 0, i32 3 
  store   %_arr* %_v24, %_arr** %_v30  
  %_v31 = load   %A*, %A** %_v11  
  %_v32 = getelementptr  %A, %A* %_v31, i32 0, i32 5 
  store   i32 42, i32* %_v32  
  br label %_B2 
_B1:
  %_v33 = load   %A*, %A** %_v11  
  %_v34 = getelementptr  %A, %A* %_v33, i32 0, i32 5 
  %_v35 = load   i32, i32* %_v34  
  %_v36 = sub   i32 %_v35, 1 
  %_v37 = load   %A*, %A** %_v11  
  %_v38 = getelementptr  %A, %A* %_v37, i32 0, i32 5 
  store   i32 %_v36, i32* %_v38  
  %_v39 = load   %A*, %A** %_v11  
  %_v40 = getelementptr  %A, %A* %_v39, i32 0, i32 5 
  %_v41 = load   i32, i32* %_v40  
  %_v42 = load   %A*, %A** %_v11  
  %_v43 = getelementptr  %A, %A* %_v42, i32 0, i32 3 
  %_v44 = load   %_arr*, %_arr** %_v43  
  %_v45 = load   %A*, %A** %_v11  
  %_v46 = getelementptr  %A, %A* %_v45, i32 0, i32 5 
  %_v47 = load   i32, i32* %_v46  
  %_v48 = getelementptr  %_arr, %_arr* %_v44, i32 0, i32 0 
  %_v49 = load   i8*, i8** %_v48  
  %_v50 = bitcast i8* %_v49 to i32* 
  %_v51 = getelementptr  i32, i32* %_v50, i32 %_v47 
  store   i32 %_v41, i32* %_v51  
  br label %_B2 
_B2:
  %_v52 = load   %A*, %A** %_v11  
  %_v53 = getelementptr  %A, %A* %_v52, i32 0, i32 5 
  %_v54 = load   i32, i32* %_v53  
  %_v55 = icmp sgt i32 %_v54, 0 
  br i1 %_v55, label %_B1, label %_B3 
_B3:
  br label %_B5 
_B4:
  %_v56 = load   %A*, %A** %_v11  
  %_v57 = getelementptr  %A, %A* %_v56, i32 0, i32 3 
  %_v58 = load   %_arr*, %_arr** %_v57  
  %_v59 = load   %A*, %A** %_v11  
  %_v60 = getelementptr  %A, %A* %_v59, i32 0, i32 5 
  %_v61 = load   i32, i32* %_v60  
  %_v62 = getelementptr  %_arr, %_arr* %_v58, i32 0, i32 0 
  %_v63 = load   i8*, i8** %_v62  
  %_v64 = bitcast i8* %_v63 to i32* 
  %_v65 = getelementptr  i32, i32* %_v64, i32 %_v61 
  %_v66 = load   i32, i32* %_v65  
   call ccc  void  @printInt(i32  %_v66)  
  %_v68 = load   %A*, %A** %_v11  
  %_v69 = getelementptr  %A, %A* %_v68, i32 0, i32 5 
  %_v70 = load   i32, i32* %_v69  
  %_v71 = add   i32 %_v70, 1 
  %_v72 = load   %A*, %A** %_v11  
  %_v73 = getelementptr  %A, %A* %_v72, i32 0, i32 5 
  store   i32 %_v71, i32* %_v73  
  br label %_B5 
_B5:
  %_v74 = load   %A*, %A** %_v11  
  %_v75 = getelementptr  %A, %A* %_v74, i32 0, i32 5 
  %_v76 = load   i32, i32* %_v75  
  %_v77 = icmp slt i32 %_v76, 42 
  br i1 %_v77, label %_B4, label %_B6 
_B6:
  ret i32 0 
}