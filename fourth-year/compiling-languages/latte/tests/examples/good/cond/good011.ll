; ModuleID = 'program'


 


declare external ccc  void @printInt(i32)    


declare external ccc  void @printString(i8*)    


declare external ccc  void @error()    


declare external ccc  i32 @readInt()    


declare external ccc  i8* @readString()    


declare external ccc  i8* @_concatStrings(i8*, i8*)    


declare external ccc  i32 @_compareStrings(i8*, i8*)    


@_v2 = private   constant [6 x i8] c"1 < 2\00"


@_v6 = private   constant [3 x i8] c"ok\00"


@_v10 = private   constant [7 x i8] c"2 <= 2\00"


@_v14 = private   constant [3 x i8] c"ok\00"


@_v18 = private   constant [7 x i8] c"1 <= 2\00"


@_v22 = private   constant [3 x i8] c"ok\00"


@_v26 = private   constant [7 x i8] c"1 == 1\00"


@_v30 = private   constant [3 x i8] c"ok\00"


@_v34 = private   constant [7 x i8] c"2 >= 2\00"


@_v38 = private   constant [3 x i8] c"ok\00"


@_v42 = private   constant [7 x i8] c"2 >= 1\00"


@_v46 = private   constant [3 x i8] c"ok\00"


@_v50 = private   constant [6 x i8] c"2 > 1\00"


@_v54 = private   constant [3 x i8] c"ok\00"


define external ccc  i32 @main()    {
entry:
  %_v3 = bitcast [6 x i8]* @_v2 to i8* 
   call ccc  void  @printString(i8*  %_v3)  
  %_v7 = bitcast [3 x i8]* @_v6 to i8* 
   call ccc  void  @printString(i8*  %_v7)  
  %_v11 = bitcast [7 x i8]* @_v10 to i8* 
   call ccc  void  @printString(i8*  %_v11)  
  %_v15 = bitcast [3 x i8]* @_v14 to i8* 
   call ccc  void  @printString(i8*  %_v15)  
  %_v19 = bitcast [7 x i8]* @_v18 to i8* 
   call ccc  void  @printString(i8*  %_v19)  
  %_v23 = bitcast [3 x i8]* @_v22 to i8* 
   call ccc  void  @printString(i8*  %_v23)  
  %_v27 = bitcast [7 x i8]* @_v26 to i8* 
   call ccc  void  @printString(i8*  %_v27)  
  %_v31 = bitcast [3 x i8]* @_v30 to i8* 
   call ccc  void  @printString(i8*  %_v31)  
  %_v35 = bitcast [7 x i8]* @_v34 to i8* 
   call ccc  void  @printString(i8*  %_v35)  
  %_v39 = bitcast [3 x i8]* @_v38 to i8* 
   call ccc  void  @printString(i8*  %_v39)  
  %_v43 = bitcast [7 x i8]* @_v42 to i8* 
   call ccc  void  @printString(i8*  %_v43)  
  %_v47 = bitcast [3 x i8]* @_v46 to i8* 
   call ccc  void  @printString(i8*  %_v47)  
  %_v51 = bitcast [6 x i8]* @_v50 to i8* 
   call ccc  void  @printString(i8*  %_v51)  
  %_v55 = bitcast [3 x i8]* @_v54 to i8* 
   call ccc  void  @printString(i8*  %_v55)  
  ret i32 0 
}