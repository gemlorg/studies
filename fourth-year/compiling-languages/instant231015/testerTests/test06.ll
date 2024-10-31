source_filename = "test.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() #0 {
%1 = alloca i32, align 4
%2 = add nsw i32 0, 0
store i32 %2, i32* %1, align 4
%3 = alloca i32, align 4
%4 = add nsw i32 0, 1
store i32 %4, i32* %3, align 4
%5 = alloca i32, align 4
%6 = add nsw i32 0, 0
store i32 %6, i32* %5, align 4
%7 = alloca i32, align 4
%8 = add nsw i32 0, 1
store i32 %8, i32* %7, align 4
%9 = alloca i32, align 4
%10 = add nsw i32 0, 0
store i32 %10, i32* %9, align 4
%11 = alloca i32, align 4
%12 = add nsw i32 0, 1
store i32 %12, i32* %11, align 4
%13 = alloca i32, align 4
%14 = add nsw i32 0, 0
store i32 %14, i32* %13, align 4
%15 = alloca i32, align 4
%16 = add nsw i32 0, 1
store i32 %16, i32* %15, align 4
%17 = load i32, i32* %1, align 4
%18 = load i32, i32* %3, align 4
%19 = mul nsw i32 %17, %18
%20 = load i32, i32* %5, align 4
%21 = load i32, i32* %7, align 4
%22 = mul nsw i32 %20, %21
%23 = load i32, i32* %9, align 4
%24 = load i32, i32* %11, align 4
%25 = load i32, i32* %13, align 4
%26 = load i32, i32* %15, align 4
%27 = add nsw i32 %25, %26
%28 = add nsw i32 %24, %27
%29 = add nsw i32 %23, %28
%30 = add nsw i32 %22, %29
%31 = add nsw i32 %19, %30
call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %31)
%33 = add nsw i32 0, 1
store i32 %33, i32* %1, align 4
%34 = add nsw i32 0, 2
store i32 %34, i32* %3, align 4
%35 = add nsw i32 0, 1
store i32 %35, i32* %5, align 4
%36 = add nsw i32 0, 2
store i32 %36, i32* %7, align 4
%37 = add nsw i32 0, 1
store i32 %37, i32* %9, align 4
%38 = add nsw i32 0, 2
store i32 %38, i32* %11, align 4
%39 = add nsw i32 0, 1
store i32 %39, i32* %13, align 4
%40 = add nsw i32 0, 2
store i32 %40, i32* %15, align 4
%41 = alloca i32, align 4
%42 = add nsw i32 0, 1
store i32 %42, i32* %41, align 4
%43 = alloca i32, align 4
%44 = add nsw i32 0, 2
store i32 %44, i32* %43, align 4
%45 = alloca i32, align 4
%46 = add nsw i32 0, 1
store i32 %46, i32* %45, align 4
%47 = alloca i32, align 4
%48 = add nsw i32 0, 2
store i32 %48, i32* %47, align 4
%49 = alloca i32, align 4
%50 = add nsw i32 0, 1
store i32 %50, i32* %49, align 4
%51 = alloca i32, align 4
%52 = add nsw i32 0, 2
store i32 %52, i32* %51, align 4
%53 = add nsw i32 0, 2
%54 = load i32, i32* %1, align 4
%55 = mul nsw i32 %53, %54
%56 = load i32, i32* %3, align 4
%57 = add nsw i32 0, 2
%58 = sdiv i32 %56, %57
%59 = load i32, i32* %5, align 4
%60 = load i32, i32* %7, align 4
%61 = load i32, i32* %9, align 4
%62 = load i32, i32* %11, align 4
%63 = load i32, i32* %13, align 4
%64 = load i32, i32* %15, align 4
%65 = load i32, i32* %41, align 4
%66 = load i32, i32* %43, align 4
%67 = add nsw i32 0, 2
%68 = sdiv i32 %66, %67
%69 = load i32, i32* %45, align 4
%70 = load i32, i32* %47, align 4
%71 = load i32, i32* %49, align 4
%72 = load i32, i32* %51, align 4
%73 = add nsw i32 %71, %72
%74 = add nsw i32 %70, %73
%75 = add nsw i32 %69, %74
%76 = add nsw i32 %68, %75
%77 = add nsw i32 %65, %76
%78 = add nsw i32 %64, %77
%79 = add nsw i32 %63, %78
%80 = add nsw i32 %62, %79
%81 = add nsw i32 %61, %80
%82 = add nsw i32 %60, %81
%83 = add nsw i32 %59, %82
%84 = add nsw i32 %58, %83
%85 = add nsw i32 %55, %84
%86 = add nsw i32 0, 10
%87 = sdiv i32 %85, %86
call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %87)
ret i32 0
}
declare i32 @printf(i8*, ...) #1
attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}