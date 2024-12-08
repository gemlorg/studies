; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


define external ccc  i32 @d()    {
entry:
  ret i32 0 
}


define external ccc  i32 @s(i32  %x)    {
entry:
  %_v1 = add   i32 %x, 1 
  ret i32 %_v1 
}


define external ccc  i32 @main()    {
entry:
  %_v2 =  call ccc  i32  @d()  
  %_v3 =  call ccc  i32  @s(i32  %_v2)  
  %_v4 =  call ccc  i32  @s(i32  %_v3)  
  %_v5 =  call ccc  i32  @s(i32  %_v4)  
  %_v6 =  call ccc  i32  @s(i32  %_v5)  
  %_v7 =  call ccc  i32  @s(i32  %_v6)  
  %_v8 =  call ccc  i32  @s(i32  %_v7)  
  %_v9 =  call ccc  i32  @s(i32  %_v8)  
  %_v10 =  call ccc  i32  @s(i32  %_v9)  
  %_v11 =  call ccc  i32  @s(i32  %_v10)  
  %_v12 =  call ccc  i32  @s(i32  %_v11)  
  %_v13 =  call ccc  i32  @s(i32  %_v12)  
  %_v14 =  call ccc  i32  @s(i32  %_v13)  
  %_v15 =  call ccc  i32  @s(i32  %_v14)  
  %_v16 =  call ccc  i32  @s(i32  %_v15)  
  %_v17 =  call ccc  i32  @s(i32  %_v16)  
  %_v18 =  call ccc  i32  @s(i32  %_v17)  
  %_v19 =  call ccc  i32  @s(i32  %_v18)  
  %_v20 =  call ccc  i32  @s(i32  %_v19)  
  %_v21 =  call ccc  i32  @s(i32  %_v20)  
  %_v22 =  call ccc  i32  @s(i32  %_v21)  
  %_v23 =  call ccc  i32  @s(i32  %_v22)  
  %_v24 =  call ccc  i32  @s(i32  %_v23)  
  %_v25 =  call ccc  i32  @s(i32  %_v24)  
  %_v26 =  call ccc  i32  @s(i32  %_v25)  
  %_v27 =  call ccc  i32  @s(i32  %_v26)  
  %_v28 =  call ccc  i32  @s(i32  %_v27)  
  %_v29 =  call ccc  i32  @s(i32  %_v28)  
  %_v30 =  call ccc  i32  @s(i32  %_v29)  
  %_v31 =  call ccc  i32  @s(i32  %_v30)  
  %_v32 =  call ccc  i32  @s(i32  %_v31)  
  %_v33 =  call ccc  i32  @s(i32  %_v32)  
  %_v34 =  call ccc  i32  @s(i32  %_v33)  
  %_v35 =  call ccc  i32  @s(i32  %_v34)  
  %_v36 =  call ccc  i32  @s(i32  %_v35)  
  %_v37 =  call ccc  i32  @s(i32  %_v36)  
  %_v38 =  call ccc  i32  @s(i32  %_v37)  
  %_v39 =  call ccc  i32  @s(i32  %_v38)  
  %_v40 =  call ccc  i32  @s(i32  %_v39)  
  %_v41 =  call ccc  i32  @s(i32  %_v40)  
  %_v42 =  call ccc  i32  @s(i32  %_v41)  
  %_v43 =  call ccc  i32  @s(i32  %_v42)  
  %_v44 =  call ccc  i32  @s(i32  %_v43)  
  %_v45 =  call ccc  i32  @s(i32  %_v44)  
  %_v46 =  call ccc  i32  @s(i32  %_v45)  
  %_v47 =  call ccc  i32  @s(i32  %_v46)  
  %_v48 =  call ccc  i32  @s(i32  %_v47)  
  %_v49 =  call ccc  i32  @s(i32  %_v48)  
  %_v50 =  call ccc  i32  @s(i32  %_v49)  
  %_v51 =  call ccc  i32  @s(i32  %_v50)  
  %_v52 =  call ccc  i32  @s(i32  %_v51)  
  %_v53 =  call ccc  i32  @s(i32  %_v52)  
  %_v54 =  call ccc  i32  @s(i32  %_v53)  
  %_v55 =  call ccc  i32  @s(i32  %_v54)  
  %_v56 =  call ccc  i32  @s(i32  %_v55)  
  %_v57 =  call ccc  i32  @s(i32  %_v56)  
  %_v58 =  call ccc  i32  @s(i32  %_v57)  
  %_v59 =  call ccc  i32  @s(i32  %_v58)  
  %_v60 =  call ccc  i32  @s(i32  %_v59)  
  %_v61 =  call ccc  i32  @s(i32  %_v60)  
  %_v62 =  call ccc  i32  @s(i32  %_v61)  
  %_v63 =  call ccc  i32  @s(i32  %_v62)  
  %_v64 =  call ccc  i32  @s(i32  %_v63)  
  %_v65 =  call ccc  i32  @s(i32  %_v64)  
  %_v66 =  call ccc  i32  @s(i32  %_v65)  
  %_v67 =  call ccc  i32  @s(i32  %_v66)  
  %_v68 =  call ccc  i32  @s(i32  %_v67)  
  %_v69 =  call ccc  i32  @s(i32  %_v68)  
  %_v70 =  call ccc  i32  @s(i32  %_v69)  
  %_v71 =  call ccc  i32  @s(i32  %_v70)  
  %_v72 =  call ccc  i32  @s(i32  %_v71)  
  %_v73 =  call ccc  i32  @s(i32  %_v72)  
  %_v74 =  call ccc  i32  @s(i32  %_v73)  
  %_v75 =  call ccc  i32  @s(i32  %_v74)  
  %_v76 =  call ccc  i32  @s(i32  %_v75)  
  %_v77 =  call ccc  i32  @s(i32  %_v76)  
  %_v78 =  call ccc  i32  @s(i32  %_v77)  
  %_v79 =  call ccc  i32  @s(i32  %_v78)  
  %_v80 =  call ccc  i32  @s(i32  %_v79)  
  %_v81 =  call ccc  i32  @s(i32  %_v80)  
  %_v82 =  call ccc  i32  @s(i32  %_v81)  
  %_v83 =  call ccc  i32  @s(i32  %_v82)  
  %_v84 =  call ccc  i32  @s(i32  %_v83)  
  %_v85 =  call ccc  i32  @s(i32  %_v84)  
  %_v86 =  call ccc  i32  @s(i32  %_v85)  
  %_v87 =  call ccc  i32  @s(i32  %_v86)  
  %_v88 =  call ccc  i32  @s(i32  %_v87)  
  %_v89 =  call ccc  i32  @s(i32  %_v88)  
  %_v90 =  call ccc  i32  @s(i32  %_v89)  
  %_v91 =  call ccc  i32  @s(i32  %_v90)  
  %_v92 =  call ccc  i32  @s(i32  %_v91)  
  %_v93 =  call ccc  i32  @s(i32  %_v92)  
  %_v94 =  call ccc  i32  @s(i32  %_v93)  
  %_v95 =  call ccc  i32  @s(i32  %_v94)  
  %_v96 =  call ccc  i32  @s(i32  %_v95)  
  %_v97 =  call ccc  i32  @s(i32  %_v96)  
  %_v98 =  call ccc  i32  @s(i32  %_v97)  
  %_v99 =  call ccc  i32  @s(i32  %_v98)  
  %_v100 =  call ccc  i32  @s(i32  %_v99)  
  %_v101 =  call ccc  i32  @s(i32  %_v100)  
  %_v102 =  call ccc  i32  @s(i32  %_v101)  
  %_v103 =  call ccc  i32  @s(i32  %_v102)  
  %_v104 =  call ccc  i32  @s(i32  %_v103)  
  %_v105 =  call ccc  i32  @s(i32  %_v104)  
  %_v106 =  call ccc  i32  @s(i32  %_v105)  
  %_v107 =  call ccc  i32  @s(i32  %_v106)  
   call ccc  void  @printInt(i32  %_v107)  
  ret i32 0 
}