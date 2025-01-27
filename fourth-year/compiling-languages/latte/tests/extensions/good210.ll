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


@_g1 =    constant [14 x i8] c"hello from a2\00"


@_g2 =    constant [13 x i8] c"hello from a\00"


%_arr = type {i8*, i32}


define external ccc  %_arr* @__arr_malloc()    {
entry:
  %0 = getelementptr inbounds %_arr, %_arr* zeroinitializer, i32 1 
  %1 = ptrtoint %_arr* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %_arr* 
  ret %_arr* %3 
}


%A = type {%_A_vtype*, i8*}


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


%B = type {%_B_vtype*, %A*, %A*, %B*}


%_B_vtype = type {}


@_B_vdata =    global %_B_vtype {  }


define external ccc  %B* @_B_malloc()    {
entry:
  %0 = getelementptr inbounds %B, %B* zeroinitializer, i32 1 
  %1 = ptrtoint %B* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %B* 
  ret %B* %3 
}


define external ccc  i32 @main()    {
entry:
  %_v2 = bitcast [1 x i8]* @_g0 to i8* 
  %_v0 =  call ccc  %A*  @_A_malloc()  
  %_v1 = getelementptr  %A, %A* %_v0, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v1  
  %_v3 = getelementptr  %A, %A* %_v0, i32 0, i32 1 
  store   i8* %_v2, i8** %_v3  
  %_v4 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v4  
  store   %A* %_v0, %A** %_v4  
  %_v7 = bitcast [1 x i8]* @_g0 to i8* 
  %_v5 =  call ccc  %A*  @_A_malloc()  
  %_v6 = getelementptr  %A, %A* %_v5, i32 0, i32 0 
  store   %_A_vtype* @_A_vdata, %_A_vtype** %_v6  
  %_v8 = getelementptr  %A, %A* %_v5, i32 0, i32 1 
  store   i8* %_v7, i8** %_v8  
  %_v9 = alloca %A* 
  store   %A* zeroinitializer, %A** %_v9  
  store   %A* %_v5, %A** %_v9  
  %_v10 = bitcast [14 x i8]* @_g1 to i8* 
  %_v11 = load   %A*, %A** %_v9  
  %_v12 = getelementptr  %A, %A* %_v11, i32 0, i32 1 
  store   i8* %_v10, i8** %_v12  
  %_v13 = bitcast [13 x i8]* @_g2 to i8* 
  %_v14 = load   %A*, %A** %_v4  
  %_v15 = getelementptr  %A, %A* %_v14, i32 0, i32 1 
  store   i8* %_v13, i8** %_v15  
  %_v16 =  call ccc  %B*  @_B_malloc()  
  %_v17 = getelementptr  %B, %B* %_v16, i32 0, i32 0 
  store   %_B_vtype* @_B_vdata, %_B_vtype** %_v17  
  %_v18 = getelementptr  %B, %B* %_v16, i32 0, i32 1 
  store   %A* zeroinitializer, %A** %_v18  
  %_v19 = getelementptr  %B, %B* %_v16, i32 0, i32 2 
  store   %A* zeroinitializer, %A** %_v19  
  %_v20 = getelementptr  %B, %B* %_v16, i32 0, i32 3 
  store   %B* zeroinitializer, %B** %_v20  
  %_v21 = alloca %B* 
  store   %B* zeroinitializer, %B** %_v21  
  store   %B* %_v16, %B** %_v21  
  %_v22 = load   %A*, %A** %_v4  
  %_v23 = load   %B*, %B** %_v21  
  %_v24 = getelementptr  %B, %B* %_v23, i32 0, i32 1 
  store   %A* %_v22, %A** %_v24  
  %_v25 = load   %A*, %A** %_v4  
  %_v26 = load   %B*, %B** %_v21  
  %_v27 = getelementptr  %B, %B* %_v26, i32 0, i32 2 
  store   %A* %_v25, %A** %_v27  
  %_v28 =  call ccc  %B*  @_B_malloc()  
  %_v29 = getelementptr  %B, %B* %_v28, i32 0, i32 0 
  store   %_B_vtype* @_B_vdata, %_B_vtype** %_v29  
  %_v30 = getelementptr  %B, %B* %_v28, i32 0, i32 1 
  store   %A* zeroinitializer, %A** %_v30  
  %_v31 = getelementptr  %B, %B* %_v28, i32 0, i32 2 
  store   %A* zeroinitializer, %A** %_v31  
  %_v32 = getelementptr  %B, %B* %_v28, i32 0, i32 3 
  store   %B* zeroinitializer, %B** %_v32  
  %_v33 = load   %B*, %B** %_v21  
  %_v34 = getelementptr  %B, %B* %_v33, i32 0, i32 3 
  store   %B* %_v28, %B** %_v34  
  %_v35 = load   %A*, %A** %_v9  
  %_v36 = load   %B*, %B** %_v21  
  %_v37 = getelementptr  %B, %B* %_v36, i32 0, i32 3 
  %_v38 = load   %B*, %B** %_v37  
  %_v39 = getelementptr  %B, %B* %_v38, i32 0, i32 1 
  store   %A* %_v35, %A** %_v39  
  %_v40 = load   %A*, %A** %_v4  
  %_v41 = load   %B*, %B** %_v21  
  %_v42 = getelementptr  %B, %B* %_v41, i32 0, i32 3 
  %_v43 = load   %B*, %B** %_v42  
  %_v44 = getelementptr  %B, %B* %_v43, i32 0, i32 2 
  store   %A* %_v40, %A** %_v44  
  %_v45 = load   %B*, %B** %_v21  
  %_v46 = getelementptr  %B, %B* %_v45, i32 0, i32 1 
  %_v47 = load   %A*, %A** %_v46  
  %_v48 = getelementptr  %A, %A* %_v47, i32 0, i32 1 
  %_v49 = load   i8*, i8** %_v48  
   call ccc  void  @printString(i8*  %_v49)  
  %_v51 = load   %B*, %B** %_v21  
  %_v52 = getelementptr  %B, %B* %_v51, i32 0, i32 2 
  %_v53 = load   %A*, %A** %_v52  
  %_v54 = getelementptr  %A, %A* %_v53, i32 0, i32 1 
  %_v55 = load   i8*, i8** %_v54  
   call ccc  void  @printString(i8*  %_v55)  
  %_v57 = load   %B*, %B** %_v21  
  %_v58 = getelementptr  %B, %B* %_v57, i32 0, i32 3 
  %_v59 = load   %B*, %B** %_v58  
  %_v60 = getelementptr  %B, %B* %_v59, i32 0, i32 1 
  %_v61 = load   %A*, %A** %_v60  
  %_v62 = getelementptr  %A, %A* %_v61, i32 0, i32 1 
  %_v63 = load   i8*, i8** %_v62  
   call ccc  void  @printString(i8*  %_v63)  
  %_v65 = load   %B*, %B** %_v21  
  %_v66 = getelementptr  %B, %B* %_v65, i32 0, i32 3 
  %_v67 = load   %B*, %B** %_v66  
  %_v68 = getelementptr  %B, %B* %_v67, i32 0, i32 2 
  %_v69 = load   %A*, %A** %_v68  
  %_v70 = getelementptr  %A, %A* %_v69, i32 0, i32 1 
  %_v71 = load   i8*, i8** %_v70  
   call ccc  void  @printString(i8*  %_v71)  
  ret i32 0 
}