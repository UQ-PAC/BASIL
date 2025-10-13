; ModuleID = 'dsa-testing-O0/basil-tests/unsafe_pointer_arithmetic.c'
source_filename = "dsa-testing-O0/basil-tests/unsafe_pointer_arithmetic.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca i32*, align 8
  %2 = alloca i8*, align 8
  %3 = alloca i32*, align 8
  %4 = alloca i32**, align 8
  %5 = alloca i32*, align 8
  %6 = call noalias i8* @malloc(i64 noundef 20) #2
  %7 = bitcast i8* %6 to i32*
  store i32* %7, i32** %1, align 8
  %8 = load i32*, i32** %1, align 8
  store i32 12, i32* %8, align 4
  %9 = load i32*, i32** %1, align 8
  %10 = bitcast i32* %9 to i8*
  store i8* %10, i8** %2, align 8
  %11 = load i8*, i8** %2, align 8
  %12 = getelementptr i8, i8* %11, i64 1
  %13 = bitcast i8* %12 to i32*
  store i32* %13, i32** %3, align 8
  %14 = call noalias i8* @malloc(i64 noundef 8) #2
  %15 = bitcast i8* %14 to i32**
  store i32** %15, i32*** %4, align 8
  %16 = load i32*, i32** %3, align 8
  %17 = load i32**, i32*** %4, align 8
  store i32* %16, i32** %17, align 8
  %18 = load i32**, i32*** %4, align 8
  %19 = load i32*, i32** %18, align 8
  store i32* %19, i32** %5, align 8
  ret i32 0
}

; Function Attrs: nounwind
declare dso_local noalias i8* @malloc(i64 noundef) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon,+outline-atomics,+v8a" }
attributes #1 = { nounwind "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon,+outline-atomics,+v8a" }
attributes #2 = { nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6}
!llvm.ident = !{!7}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 1, !"branch-target-enforcement", i32 0}
!2 = !{i32 1, !"sign-return-address", i32 0}
!3 = !{i32 1, !"sign-return-address-all", i32 0}
!4 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
!5 = !{i32 7, !"uwtable", i32 1}
!6 = !{i32 7, !"frame-pointer", i32 1}
!7 = !{!"clang version 14.0.6"}
