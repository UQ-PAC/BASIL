; ModuleID = 'dsa/split/split.c'
source_filename = "dsa/split/split.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "aarch64-unknown-linux-gnu"

@x = dso_local global i32 0, align 4
@y = dso_local global ptr null, align 8
@array = internal global [10 x i32] [i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7, i32 8, i32 9], align 4

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 0, ptr %2, align 4
  store i32 %0, ptr %3, align 4
  store ptr @x, ptr @y, align 8
  %4 = load i32, ptr getelementptr inbounds ([10 x i32], ptr @array, i64 0, i64 1), align 4
  ret i32 %4
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="non-leaf" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+fp-armv8,+neon,+outline-atomics,+v8a,-fmv" }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 1}
!5 = !{!"clang version 19.1.7"}
