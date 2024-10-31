source_filename = "test.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() #0 {
%1 = add nsw i32 0, 1
%2 = add nsw i32 0, 1
%3 = add nsw i32 0, 1
%4 = sub nsw i32 %2, %3
%5 = add nsw i32 0, 1
%6 = add nsw i32 0, 1
%7 = sub nsw i32 %5, %6
%8 = add nsw i32 0, 1
%9 = add nsw i32 0, 1
%10 = sub nsw i32 %8, %9
%11 = add nsw i32 0, 1
%12 = add nsw i32 0, 1
%13 = sub nsw i32 %11, %12
%14 = add nsw i32 0, 1
%15 = add nsw i32 0, 1
%16 = sub nsw i32 %14, %15
%17 = add nsw i32 0, 1
%18 = add nsw i32 0, 1
%19 = sub nsw i32 %17, %18
%20 = add nsw i32 0, 1
%21 = add nsw i32 0, 1
%22 = sub nsw i32 %20, %21
%23 = add nsw i32 0, 1
%24 = add nsw i32 0, 1
%25 = sub nsw i32 %23, %24
%26 = add nsw i32 0, 1
%27 = add nsw i32 0, 1
%28 = sub nsw i32 %26, %27
%29 = add nsw i32 0, 1
%30 = add nsw i32 0, 1
%31 = sub nsw i32 %29, %30
%32 = add nsw i32 0, 1
%33 = add nsw i32 0, 1
%34 = sub nsw i32 %32, %33
%35 = add nsw i32 0, 1
%36 = add nsw i32 0, 1
%37 = sub nsw i32 %35, %36
%38 = add nsw i32 0, 1
%39 = add nsw i32 0, 1
%40 = sub nsw i32 %38, %39
%41 = add nsw i32 0, 1
%42 = add nsw i32 0, 1
%43 = sub nsw i32 %41, %42
%44 = add nsw i32 0, 1
%45 = add nsw i32 0, 1
%46 = sub nsw i32 %44, %45
%47 = add nsw i32 0, 1
%48 = add nsw i32 0, 1
%49 = sub nsw i32 %47, %48
%50 = add nsw i32 0, 1
%51 = add nsw i32 0, 1
%52 = sub nsw i32 %50, %51
%53 = add nsw i32 0, 1
%54 = add nsw i32 0, 1
%55 = sub nsw i32 %53, %54
%56 = add nsw i32 0, 1
%57 = add nsw i32 0, 1
%58 = sub nsw i32 %56, %57
%59 = add nsw i32 %55, %58
%60 = add nsw i32 %52, %59
%61 = add nsw i32 %49, %60
%62 = add nsw i32 %46, %61
%63 = add nsw i32 %43, %62
%64 = add nsw i32 %40, %63
%65 = add nsw i32 %37, %64
%66 = add nsw i32 %34, %65
%67 = add nsw i32 %31, %66
%68 = add nsw i32 %28, %67
%69 = add nsw i32 %25, %68
%70 = add nsw i32 %22, %69
%71 = add nsw i32 %19, %70
%72 = add nsw i32 %16, %71
%73 = add nsw i32 %13, %72
%74 = add nsw i32 %10, %73
%75 = add nsw i32 %7, %74
%76 = add nsw i32 %4, %75
%77 = add nsw i32 %1, %76
call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %77)
ret i32 0
}
declare i32 @printf(i8*, ...) #1
attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}