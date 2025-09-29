; ModuleID = 'dsa-testing/sadra-examples/indirect_overlapping/indirect_overlapping.c'
source_filename = "dsa-testing/sadra-examples/indirect_overlapping/indirect_overlapping.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

%struct.str = type { i64, i64, i64 }

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
  %1 = alloca %struct.str, align 8
  %2 = alloca %struct.str*, align 8
  %3 = getelementptr inbounds %struct.str, %struct.str* %1, i32 0, i32 0
  store i64 3, i64* %3, align 8
  %4 = getelementptr inbounds %struct.str, %struct.str* %1, i32 0, i32 2
  store i64 10, i64* %4, align 8
  store %struct.str* %1, %struct.str** %2, align 8
  %5 = load %struct.str*, %struct.str** %2, align 8
  %6 = getelementptr inbounds %struct.str, %struct.str* %5, i32 0, i32 0
  store i64 4, i64* %6, align 8
  %7 = load %struct.str*, %struct.str** %2, align 8
  %8 = getelementptr inbounds %struct.str, %struct.str* %7, i32 0, i32 1
  store i64 12, i64* %8, align 8
  %9 = load %struct.str*, %struct.str** %2, align 8
  %10 = getelementptr inbounds %struct.str, %struct.str* %9, i32 0, i32 2
  store i64 13, i64* %10, align 8
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="non-leaf" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon,+outline-atomics,+v8a" }

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
