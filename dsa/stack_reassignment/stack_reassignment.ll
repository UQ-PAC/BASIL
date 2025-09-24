; ModuleID = '/host/dsa/stack_reassignment/stack_reassignment.c'
source_filename = "/host/dsa/stack_reassignment/stack_reassignment.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

@.str = private unnamed_addr constant [3 x i8] c"%d\00", align 1

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main() #0 !dbg !7 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32*, align 8
  call void @llvm.dbg.declare(metadata i32* %1, metadata !12, metadata !DIExpression()), !dbg !13
  call void @llvm.dbg.declare(metadata i32* %2, metadata !14, metadata !DIExpression()), !dbg !15
  call void @llvm.dbg.declare(metadata i32** %3, metadata !16, metadata !DIExpression()), !dbg !18
  store i32 1, i32* %1, align 4, !dbg !19
  store i32 2, i32* %2, align 4, !dbg !20
  store i32* %1, i32** %3, align 8, !dbg !21
  %4 = load i32*, i32** %3, align 8, !dbg !22
  %5 = load i32, i32* %4, align 4, !dbg !23
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), i32 %5), !dbg !24
  store i32* %2, i32** %3, align 8, !dbg !25
  %7 = load i32*, i32** %3, align 8, !dbg !26
  %8 = load i32, i32* %7, align 4, !dbg !27
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i64 0, i64 0), i32 %8), !dbg !28
  ret i32 0, !dbg !29
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare dso_local i32 @printf(i8*, ...) #2

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/host/dsa/stack_reassignment/stack_reassignment.c", directory: "/")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 10.0.0-4ubuntu1 "}
!7 = distinct !DISubprogram(name: "main", scope: !8, file: !8, line: 3, type: !9, scopeLine: 3, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!8 = !DIFile(filename: "/host/dsa/stack_reassignment/stack_reassignment.c", directory: "")
!9 = !DISubroutineType(types: !10)
!10 = !{!11}
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !DILocalVariable(name: "a", scope: !7, file: !8, line: 5, type: !11)
!13 = !DILocation(line: 5, column: 9, scope: !7)
!14 = !DILocalVariable(name: "b", scope: !7, file: !8, line: 5, type: !11)
!15 = !DILocation(line: 5, column: 11, scope: !7)
!16 = !DILocalVariable(name: "c", scope: !7, file: !8, line: 7, type: !17)
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !11, size: 64)
!18 = !DILocation(line: 7, column: 10, scope: !7)
!19 = !DILocation(line: 9, column: 7, scope: !7)
!20 = !DILocation(line: 10, column: 7, scope: !7)
!21 = !DILocation(line: 12, column: 7, scope: !7)
!22 = !DILocation(line: 13, column: 19, scope: !7)
!23 = !DILocation(line: 13, column: 18, scope: !7)
!24 = !DILocation(line: 13, column: 5, scope: !7)
!25 = !DILocation(line: 15, column: 7, scope: !7)
!26 = !DILocation(line: 16, column: 19, scope: !7)
!27 = !DILocation(line: 16, column: 18, scope: !7)
!28 = !DILocation(line: 16, column: 5, scope: !7)
!29 = !DILocation(line: 17, column: 1, scope: !7)
