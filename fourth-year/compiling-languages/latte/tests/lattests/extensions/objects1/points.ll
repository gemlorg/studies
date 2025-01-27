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


%Point2 = type {%_Point2_vtype*, i32, i32}


%_Point2_vtype = type {void (%Point2*, i32, i32)*, i32 (%Point2*)*, i32 (%Point2*)*}


@_Point2_vdata =    global %_Point2_vtype { void (%Point2*, i32, i32)* @_cf_Point2_move, i32 (%Point2*)* @_cf_Point2_getX, i32 (%Point2*)* @_cf_Point2_getY }


define external ccc  %Point2* @_Point2_malloc()    {
entry:
  %0 = getelementptr inbounds %Point2, %Point2* zeroinitializer, i32 1 
  %1 = ptrtoint %Point2* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Point2* 
  ret %Point2* %3 
}


%Point3 = type {%_Point3_vtype*, i32, i32, i32}


%_Point3_vtype = type {void (%Point2*, i32, i32)*, i32 (%Point2*)*, i32 (%Point2*)*, void (%Point3*, i32)*, i32 (%Point3*)*}


@_Point3_vdata =    global %_Point3_vtype { void (%Point2*, i32, i32)* @_cf_Point2_move, i32 (%Point2*)* @_cf_Point2_getX, i32 (%Point2*)* @_cf_Point2_getY, void (%Point3*, i32)* @_cf_Point3_moveZ, i32 (%Point3*)* @_cf_Point3_getZ }


define external ccc  %Point3* @_Point3_malloc()    {
entry:
  %0 = getelementptr inbounds %Point3, %Point3* zeroinitializer, i32 1 
  %1 = ptrtoint %Point3* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Point3* 
  ret %Point3* %3 
}


%Point4 = type {%_Point4_vtype*, i32, i32, i32, i32}


%_Point4_vtype = type {void (%Point2*, i32, i32)*, i32 (%Point2*)*, i32 (%Point2*)*, void (%Point3*, i32)*, i32 (%Point3*)*, void (%Point4*, i32)*, i32 (%Point4*)*}


@_Point4_vdata =    global %_Point4_vtype { void (%Point2*, i32, i32)* @_cf_Point2_move, i32 (%Point2*)* @_cf_Point2_getX, i32 (%Point2*)* @_cf_Point2_getY, void (%Point3*, i32)* @_cf_Point3_moveZ, i32 (%Point3*)* @_cf_Point3_getZ, void (%Point4*, i32)* @_cf_Point4_moveW, i32 (%Point4*)* @_cf_Point4_getW }


define external ccc  %Point4* @_Point4_malloc()    {
entry:
  %0 = getelementptr inbounds %Point4, %Point4* zeroinitializer, i32 1 
  %1 = ptrtoint %Point4* %0 to i32 
  %2 =  call ccc  i8*  @_malloc(i32  %1)  
  %3 = bitcast i8* %2 to %Point4* 
  ret %Point4* %3 
}


define external ccc  void @_cf_Point2_move(%Point2*  %_this, i32  %dx, i32  %dy)    {
entry:
  %_v0 = alloca %Point2* 
  store   %Point2* %_this, %Point2** %_v0  
  %_v2 = load   %Point2*, %Point2** %_v0  
  %_v1 = getelementptr  %Point2, %Point2* %_v2, i32 0, i32 1 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = add   i32 %_v3, %dx 
  %_v6 = load   %Point2*, %Point2** %_v0  
  %_v5 = getelementptr  %Point2, %Point2* %_v6, i32 0, i32 1 
  store   i32 %_v4, i32* %_v5  
  %_v8 = load   %Point2*, %Point2** %_v0  
  %_v7 = getelementptr  %Point2, %Point2* %_v8, i32 0, i32 2 
  %_v9 = load   i32, i32* %_v7  
  %_v10 = add   i32 %_v9, %dy 
  %_v12 = load   %Point2*, %Point2** %_v0  
  %_v11 = getelementptr  %Point2, %Point2* %_v12, i32 0, i32 2 
  store   i32 %_v10, i32* %_v11  
  ret void 
}


define external ccc  i32 @_cf_Point2_getX(%Point2*  %_this)    {
entry:
  %_v0 = alloca %Point2* 
  store   %Point2* %_this, %Point2** %_v0  
  %_v2 = load   %Point2*, %Point2** %_v0  
  %_v1 = getelementptr  %Point2, %Point2* %_v2, i32 0, i32 1 
  %_v3 = load   i32, i32* %_v1  
  ret i32 %_v3 
}


define external ccc  i32 @_cf_Point2_getY(%Point2*  %_this)    {
entry:
  %_v0 = alloca %Point2* 
  store   %Point2* %_this, %Point2** %_v0  
  %_v2 = load   %Point2*, %Point2** %_v0  
  %_v1 = getelementptr  %Point2, %Point2* %_v2, i32 0, i32 2 
  %_v3 = load   i32, i32* %_v1  
  ret i32 %_v3 
}


define external ccc  void @_cf_Point3_moveZ(%Point3*  %_this, i32  %dz)    {
entry:
  %_v0 = alloca %Point3* 
  store   %Point3* %_this, %Point3** %_v0  
  %_v2 = load   %Point3*, %Point3** %_v0  
  %_v1 = getelementptr  %Point3, %Point3* %_v2, i32 0, i32 3 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = add   i32 %_v3, %dz 
  %_v6 = load   %Point3*, %Point3** %_v0  
  %_v5 = getelementptr  %Point3, %Point3* %_v6, i32 0, i32 3 
  store   i32 %_v4, i32* %_v5  
  ret void 
}


define external ccc  i32 @_cf_Point3_getZ(%Point3*  %_this)    {
entry:
  %_v0 = alloca %Point3* 
  store   %Point3* %_this, %Point3** %_v0  
  %_v2 = load   %Point3*, %Point3** %_v0  
  %_v1 = getelementptr  %Point3, %Point3* %_v2, i32 0, i32 3 
  %_v3 = load   i32, i32* %_v1  
  ret i32 %_v3 
}


define external ccc  void @_cf_Point4_moveW(%Point4*  %_this, i32  %dw)    {
entry:
  %_v0 = alloca %Point4* 
  store   %Point4* %_this, %Point4** %_v0  
  %_v2 = load   %Point4*, %Point4** %_v0  
  %_v1 = getelementptr  %Point4, %Point4* %_v2, i32 0, i32 4 
  %_v3 = load   i32, i32* %_v1  
  %_v4 = add   i32 %_v3, %dw 
  %_v6 = load   %Point4*, %Point4** %_v0  
  %_v5 = getelementptr  %Point4, %Point4* %_v6, i32 0, i32 4 
  store   i32 %_v4, i32* %_v5  
  ret void 
}


define external ccc  i32 @_cf_Point4_getW(%Point4*  %_this)    {
entry:
  %_v0 = alloca %Point4* 
  store   %Point4* %_this, %Point4** %_v0  
  %_v2 = load   %Point4*, %Point4** %_v0  
  %_v1 = getelementptr  %Point4, %Point4* %_v2, i32 0, i32 4 
  %_v3 = load   i32, i32* %_v1  
  ret i32 %_v3 
}


define external ccc  i32 @main()    {
entry:
  %_v0 =  call ccc  %Point3*  @_Point3_malloc()  
  %_v1 = getelementptr  %Point3, %Point3* %_v0, i32 0, i32 0 
  store   %_Point3_vtype* @_Point3_vdata, %_Point3_vtype** %_v1  
  %_v2 = getelementptr  %Point3, %Point3* %_v0, i32 0, i32 1 
  store   i32 0, i32* %_v2  
  %_v3 = getelementptr  %Point3, %Point3* %_v0, i32 0, i32 2 
  store   i32 0, i32* %_v3  
  %_v4 = getelementptr  %Point3, %Point3* %_v0, i32 0, i32 3 
  store   i32 0, i32* %_v4  
  %_v5 = alloca %Point3* 
  store   %Point3* zeroinitializer, %Point3** %_v5  
  store   %Point3* %_v0, %Point3** %_v5  
  %_v6 =  call ccc  %Point3*  @_Point3_malloc()  
  %_v7 = getelementptr  %Point3, %Point3* %_v6, i32 0, i32 0 
  store   %_Point3_vtype* @_Point3_vdata, %_Point3_vtype** %_v7  
  %_v8 = getelementptr  %Point3, %Point3* %_v6, i32 0, i32 1 
  store   i32 0, i32* %_v8  
  %_v9 = getelementptr  %Point3, %Point3* %_v6, i32 0, i32 2 
  store   i32 0, i32* %_v9  
  %_v10 = getelementptr  %Point3, %Point3* %_v6, i32 0, i32 3 
  store   i32 0, i32* %_v10  
  %_v11 = alloca %Point3* 
  store   %Point3* zeroinitializer, %Point3** %_v11  
  store   %Point3* %_v6, %Point3** %_v11  
  %_v12 =  call ccc  %Point4*  @_Point4_malloc()  
  %_v13 = getelementptr  %Point4, %Point4* %_v12, i32 0, i32 0 
  store   %_Point4_vtype* @_Point4_vdata, %_Point4_vtype** %_v13  
  %_v14 = getelementptr  %Point4, %Point4* %_v12, i32 0, i32 1 
  store   i32 0, i32* %_v14  
  %_v15 = getelementptr  %Point4, %Point4* %_v12, i32 0, i32 2 
  store   i32 0, i32* %_v15  
  %_v16 = getelementptr  %Point4, %Point4* %_v12, i32 0, i32 3 
  store   i32 0, i32* %_v16  
  %_v17 = getelementptr  %Point4, %Point4* %_v12, i32 0, i32 4 
  store   i32 0, i32* %_v17  
  %_v18 = alloca %Point4* 
  store   %Point4* zeroinitializer, %Point4** %_v18  
  store   %Point4* %_v12, %Point4** %_v18  
  %_v19 = load   %Point3*, %Point3** %_v11  
  %_v20 = getelementptr  %Point3, %Point3* %_v19, i32 0, i32 0 
  %_v21 = load   %_Point3_vtype*, %_Point3_vtype** %_v20  
  %_v22 = getelementptr  %_Point3_vtype, %_Point3_vtype* %_v21, i32 0, i32 0 
  %_v23 = load   void (%Point2*, i32, i32)*, void (%Point2*, i32, i32)** %_v22  
  %_v24 = bitcast %Point3* %_v19 to %Point2* 
   call ccc  void  %_v23(%Point2*  %_v24, i32  2, i32  4)  
  %_v26 = load   %Point3*, %Point3** %_v11  
  %_v27 = getelementptr  %Point3, %Point3* %_v26, i32 0, i32 0 
  %_v28 = load   %_Point3_vtype*, %_Point3_vtype** %_v27  
  %_v29 = getelementptr  %_Point3_vtype, %_Point3_vtype* %_v28, i32 0, i32 3 
  %_v30 = load   void (%Point3*, i32)*, void (%Point3*, i32)** %_v29  
   call ccc  void  %_v30(%Point3*  %_v26, i32  7)  
  %_v32 = load   %Point3*, %Point3** %_v11  
  store   %Point3* %_v32, %Point3** %_v5  
  %_v33 = load   %Point3*, %Point3** %_v5  
  %_v34 = getelementptr  %Point3, %Point3* %_v33, i32 0, i32 0 
  %_v35 = load   %_Point3_vtype*, %_Point3_vtype** %_v34  
  %_v36 = getelementptr  %_Point3_vtype, %_Point3_vtype* %_v35, i32 0, i32 0 
  %_v37 = load   void (%Point2*, i32, i32)*, void (%Point2*, i32, i32)** %_v36  
  %_v38 = bitcast %Point3* %_v33 to %Point2* 
   call ccc  void  %_v37(%Point2*  %_v38, i32  3, i32  5)  
  %_v40 = load   %Point4*, %Point4** %_v18  
  %_v41 = getelementptr  %Point4, %Point4* %_v40, i32 0, i32 0 
  %_v42 = load   %_Point4_vtype*, %_Point4_vtype** %_v41  
  %_v43 = getelementptr  %_Point4_vtype, %_Point4_vtype* %_v42, i32 0, i32 0 
  %_v44 = load   void (%Point2*, i32, i32)*, void (%Point2*, i32, i32)** %_v43  
  %_v45 = bitcast %Point4* %_v40 to %Point2* 
   call ccc  void  %_v44(%Point2*  %_v45, i32  1, i32  3)  
  %_v47 = load   %Point4*, %Point4** %_v18  
  %_v48 = getelementptr  %Point4, %Point4* %_v47, i32 0, i32 0 
  %_v49 = load   %_Point4_vtype*, %_Point4_vtype** %_v48  
  %_v50 = getelementptr  %_Point4_vtype, %_Point4_vtype* %_v49, i32 0, i32 3 
  %_v51 = load   void (%Point3*, i32)*, void (%Point3*, i32)** %_v50  
  %_v52 = bitcast %Point4* %_v47 to %Point3* 
   call ccc  void  %_v51(%Point3*  %_v52, i32  6)  
  %_v54 = load   %Point4*, %Point4** %_v18  
  %_v55 = getelementptr  %Point4, %Point4* %_v54, i32 0, i32 0 
  %_v56 = load   %_Point4_vtype*, %_Point4_vtype** %_v55  
  %_v57 = getelementptr  %_Point4_vtype, %_Point4_vtype* %_v56, i32 0, i32 5 
  %_v58 = load   void (%Point4*, i32)*, void (%Point4*, i32)** %_v57  
   call ccc  void  %_v58(%Point4*  %_v54, i32  2)  
  %_v60 = load   %Point3*, %Point3** %_v5  
  %_v61 = getelementptr  %Point3, %Point3* %_v60, i32 0, i32 0 
  %_v62 = load   %_Point3_vtype*, %_Point3_vtype** %_v61  
  %_v63 = getelementptr  %_Point3_vtype, %_Point3_vtype* %_v62, i32 0, i32 1 
  %_v64 = load   i32 (%Point2*)*, i32 (%Point2*)** %_v63  
  %_v65 = bitcast %Point3* %_v60 to %Point2* 
  %_v66 =  call ccc  i32  %_v64(%Point2*  %_v65)  
   call ccc  void  @printInt(i32  %_v66)  
  %_v68 = load   %Point3*, %Point3** %_v5  
  %_v69 = getelementptr  %Point3, %Point3* %_v68, i32 0, i32 0 
  %_v70 = load   %_Point3_vtype*, %_Point3_vtype** %_v69  
  %_v71 = getelementptr  %_Point3_vtype, %_Point3_vtype* %_v70, i32 0, i32 2 
  %_v72 = load   i32 (%Point2*)*, i32 (%Point2*)** %_v71  
  %_v73 = bitcast %Point3* %_v68 to %Point2* 
  %_v74 =  call ccc  i32  %_v72(%Point2*  %_v73)  
   call ccc  void  @printInt(i32  %_v74)  
  %_v76 = load   %Point3*, %Point3** %_v11  
  %_v77 = getelementptr  %Point3, %Point3* %_v76, i32 0, i32 0 
  %_v78 = load   %_Point3_vtype*, %_Point3_vtype** %_v77  
  %_v79 = getelementptr  %_Point3_vtype, %_Point3_vtype* %_v78, i32 0, i32 4 
  %_v80 = load   i32 (%Point3*)*, i32 (%Point3*)** %_v79  
  %_v81 =  call ccc  i32  %_v80(%Point3*  %_v76)  
   call ccc  void  @printInt(i32  %_v81)  
  %_v83 = load   %Point4*, %Point4** %_v18  
  %_v84 = getelementptr  %Point4, %Point4* %_v83, i32 0, i32 0 
  %_v85 = load   %_Point4_vtype*, %_Point4_vtype** %_v84  
  %_v86 = getelementptr  %_Point4_vtype, %_Point4_vtype* %_v85, i32 0, i32 6 
  %_v87 = load   i32 (%Point4*)*, i32 (%Point4*)** %_v86  
  %_v88 =  call ccc  i32  %_v87(%Point4*  %_v83)  
   call ccc  void  @printInt(i32  %_v88)  
  ret i32 0 
}