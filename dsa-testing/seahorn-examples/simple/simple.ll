; ModuleID = 'dsa-testing/seahorn-examples/simple/simple.c'
source_filename = "dsa-testing/seahorn-examples/simple/simple.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.S = type { i32**, i32** }

@g = dso_local global i32 0, align 4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0, i8** noundef %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i8**, align 8
  %6 = alloca %struct.S, align 8
  %7 = alloca %struct.S, align 8
  %8 = alloca i32*, align 8
  %9 = alloca i32*, align 8
  store i32 0, i32* %3, align 4
  store i32 %0, i32* %4, align 4
  store i8** %1, i8*** %5, align 8
  %10 = call noalias i8* @malloc(i64 noundef 4) #2
  %11 = bitcast i8* %10 to i32*
  store i32* %11, i32** %8, align 8
  %12 = call noalias i8* @malloc(i64 noundef 4) #2
  %13 = bitcast i8* %12 to i32*
  store i32* %13, i32** %9, align 8
  %14 = getelementptr inbounds %struct.S, %struct.S* %6, i32 0, i32 0
  store i32** %8, i32*** %14, align 8
  %15 = getelementptr inbounds %struct.S, %struct.S* %6, i32 0, i32 1
  store i32** %9, i32*** %15, align 8
  %16 = getelementptr inbounds %struct.S, %struct.S* %6, i32 0, i32 0
  %17 = load i32**, i32*** %16, align 8
  store i32* @g, i32** %17, align 8
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
