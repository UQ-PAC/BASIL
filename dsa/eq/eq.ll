; ModuleID = '/host/dsa/eq/eq.c'
source_filename = "/host/dsa/eq/eq.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

@x = common dso_local global i32* null, align 8, !dbg !0
@p = common dso_local global i32* null, align 8, !dbg !13
@y = common dso_local global i32* null, align 8, !dbg !6
@z = common dso_local global i32* null, align 8, !dbg !11
@q = common dso_local global i32* null, align 8, !dbg !15

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main(i32 %0) #0 !dbg !21 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 0, i32* %2, align 4
  store i32 %0, i32* %3, align 4
  call void @llvm.dbg.declare(metadata i32* %3, metadata !24, metadata !DIExpression()), !dbg !25
  call void @llvm.dbg.declare(metadata i32* %4, metadata !26, metadata !DIExpression()), !dbg !27
  call void @llvm.dbg.declare(metadata i32* %5, metadata !28, metadata !DIExpression()), !dbg !29
  %6 = load i32, i32* %3, align 4, !dbg !30
  %7 = icmp ne i32 %6, 0, !dbg !30
  br i1 %7, label %8, label %9, !dbg !32

8:                                                ; preds = %1
  store i32* %4, i32** @x, align 8, !dbg !33
  store i32* bitcast (i32** @x to i32*), i32** @p, align 8, !dbg !35
  br label %10, !dbg !36

9:                                                ; preds = %1
  store i32* %5, i32** @y, align 8, !dbg !37
  store i32* bitcast (i32** @y to i32*), i32** @p, align 8, !dbg !39
  br label %10

10:                                               ; preds = %9, %8
  store i32* bitcast (i32** @z to i32*), i32** @q, align 8, !dbg !40
  %11 = load i32, i32* %2, align 4, !dbg !41
  ret i32 %11, !dbg !41
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!2}
!llvm.module.flags = !{!17, !18, !19}
!llvm.ident = !{!20}

!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = distinct !DIGlobalVariable(name: "x", scope: !2, file: !8, line: 1, type: !9, isLocal: false, isDefinition: true)
!2 = distinct !DICompileUnit(language: DW_LANG_C99, file: !3, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !4, globals: !5, splitDebugInlining: false, nameTableKind: None)
!3 = !DIFile(filename: "/host/dsa/eq/eq.c", directory: "/")
!4 = !{}
!5 = !{!0, !6, !11, !13, !15}
!6 = !DIGlobalVariableExpression(var: !7, expr: !DIExpression())
!7 = distinct !DIGlobalVariable(name: "y", scope: !2, file: !8, line: 2, type: !9, isLocal: false, isDefinition: true)
!8 = !DIFile(filename: "/host/dsa/eq/eq.c", directory: "")
!9 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !10, size: 64)
!10 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!11 = !DIGlobalVariableExpression(var: !12, expr: !DIExpression())
!12 = distinct !DIGlobalVariable(name: "z", scope: !2, file: !8, line: 3, type: !9, isLocal: false, isDefinition: true)
!13 = !DIGlobalVariableExpression(var: !14, expr: !DIExpression())
!14 = distinct !DIGlobalVariable(name: "p", scope: !2, file: !8, line: 4, type: !9, isLocal: false, isDefinition: true)
!15 = !DIGlobalVariableExpression(var: !16, expr: !DIExpression())
!16 = distinct !DIGlobalVariable(name: "q", scope: !2, file: !8, line: 5, type: !9, isLocal: false, isDefinition: true)
!17 = !{i32 7, !"Dwarf Version", i32 4}
!18 = !{i32 2, !"Debug Info Version", i32 3}
!19 = !{i32 1, !"wchar_size", i32 4}
!20 = !{!"clang version 10.0.0-4ubuntu1 "}
!21 = distinct !DISubprogram(name: "main", scope: !8, file: !8, line: 7, type: !22, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !2, retainedNodes: !4)
!22 = !DISubroutineType(types: !23)
!23 = !{!10, !10}
!24 = !DILocalVariable(name: "c", arg: 1, scope: !21, file: !8, line: 7, type: !10)
!25 = !DILocation(line: 7, column: 14, scope: !21)
!26 = !DILocalVariable(name: "a", scope: !21, file: !8, line: 8, type: !10)
!27 = !DILocation(line: 8, column: 8, scope: !21)
!28 = !DILocalVariable(name: "b", scope: !21, file: !8, line: 9, type: !10)
!29 = !DILocation(line: 9, column: 8, scope: !21)
!30 = !DILocation(line: 10, column: 8, scope: !31)
!31 = distinct !DILexicalBlock(scope: !21, file: !8, line: 10, column: 8)
!32 = !DILocation(line: 10, column: 8, scope: !21)
!33 = !DILocation(line: 11, column: 8, scope: !34)
!34 = distinct !DILexicalBlock(scope: !31, file: !8, line: 10, column: 11)
!35 = !DILocation(line: 12, column: 8, scope: !34)
!36 = !DILocation(line: 13, column: 4, scope: !34)
!37 = !DILocation(line: 14, column: 8, scope: !38)
!38 = distinct !DILexicalBlock(scope: !31, file: !8, line: 13, column: 11)
!39 = !DILocation(line: 15, column: 8, scope: !38)
!40 = !DILocation(line: 17, column: 6, scope: !21)
!41 = !DILocation(line: 18, column: 1, scope: !21)
