; ModuleID = '/host/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic.c'
source_filename = "/host/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone
define dso_local i32* @callee(i32* %0) #0 !dbg !7 {
  %2 = alloca i32*, align 8
  %3 = alloca i32*, align 8
  store i32* %0, i32** %2, align 8
  call void @llvm.dbg.declare(metadata i32** %2, metadata !13, metadata !DIExpression()), !dbg !14
  call void @llvm.dbg.declare(metadata i32** %3, metadata !15, metadata !DIExpression()), !dbg !16
  %4 = load i32*, i32** %2, align 8, !dbg !17
  %5 = getelementptr inbounds i32, i32* %4, i64 4, !dbg !18
  store i32* %5, i32** %3, align 8, !dbg !16
  %6 = load i32*, i32** %3, align 8, !dbg !19
  ret i32* %6, !dbg !20
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main() #0 !dbg !21 {
  %1 = alloca i32*, align 8
  %2 = alloca i32*, align 8
  %3 = alloca i32*, align 8
  call void @llvm.dbg.declare(metadata i32** %1, metadata !24, metadata !DIExpression()), !dbg !25
  %4 = call noalias i8* @malloc(i64 20) #3, !dbg !26
  %5 = bitcast i8* %4 to i32*, !dbg !26
  store i32* %5, i32** %1, align 8, !dbg !25
  %6 = load i32*, i32** %1, align 8, !dbg !27
  store i32 12, i32* %6, align 4, !dbg !28
  call void @llvm.dbg.declare(metadata i32** %2, metadata !29, metadata !DIExpression()), !dbg !30
  %7 = load i32*, i32** %1, align 8, !dbg !31
  %8 = getelementptr inbounds i32, i32* %7, i64 4, !dbg !32
  store i32* %8, i32** %2, align 8, !dbg !30
  %9 = load i32*, i32** %2, align 8, !dbg !33
  store i32 13, i32* %9, align 4, !dbg !34
  call void @llvm.dbg.declare(metadata i32** %3, metadata !35, metadata !DIExpression()), !dbg !36
  %10 = load i32*, i32** %2, align 8, !dbg !37
  %11 = call i32* @callee(i32* %10), !dbg !38
  store i32* %11, i32** %3, align 8, !dbg !36
  %12 = load i32*, i32** %3, align 8, !dbg !39
  store i32 14, i32* %12, align 4, !dbg !40
  ret i32 0, !dbg !41
}

; Function Attrs: nounwind
declare dso_local noalias i8* @malloc(i64) #2

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/host/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic.c", directory: "/")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 10.0.0-4ubuntu1 "}
!7 = distinct !DISubprogram(name: "callee", scope: !8, file: !8, line: 2, type: !9, scopeLine: 2, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!8 = !DIFile(filename: "/host/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic.c", directory: "")
!9 = !DISubroutineType(types: !10)
!10 = !{!11, !11}
!11 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !12, size: 64)
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!13 = !DILocalVariable(name: "arg", arg: 1, scope: !7, file: !8, line: 2, type: !11)
!14 = !DILocation(line: 2, column: 18, scope: !7)
!15 = !DILocalVariable(name: "ret", scope: !7, file: !8, line: 3, type: !11)
!16 = !DILocation(line: 3, column: 10, scope: !7)
!17 = !DILocation(line: 3, column: 16, scope: !7)
!18 = !DILocation(line: 3, column: 20, scope: !7)
!19 = !DILocation(line: 4, column: 12, scope: !7)
!20 = !DILocation(line: 4, column: 5, scope: !7)
!21 = distinct !DISubprogram(name: "main", scope: !8, file: !8, line: 7, type: !22, scopeLine: 7, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!22 = !DISubroutineType(types: !23)
!23 = !{!12}
!24 = !DILocalVariable(name: "bar", scope: !21, file: !8, line: 8, type: !11)
!25 = !DILocation(line: 8, column: 10, scope: !21)
!26 = !DILocation(line: 8, column: 16, scope: !21)
!27 = !DILocation(line: 9, column: 6, scope: !21)
!28 = !DILocation(line: 9, column: 10, scope: !21)
!29 = !DILocalVariable(name: "foo", scope: !21, file: !8, line: 10, type: !11)
!30 = !DILocation(line: 10, column: 10, scope: !21)
!31 = !DILocation(line: 10, column: 16, scope: !21)
!32 = !DILocation(line: 10, column: 20, scope: !21)
!33 = !DILocation(line: 11, column: 6, scope: !21)
!34 = !DILocation(line: 11, column: 10, scope: !21)
!35 = !DILocalVariable(name: "bat", scope: !21, file: !8, line: 12, type: !11)
!36 = !DILocation(line: 12, column: 10, scope: !21)
!37 = !DILocation(line: 12, column: 23, scope: !21)
!38 = !DILocation(line: 12, column: 16, scope: !21)
!39 = !DILocation(line: 13, column: 6, scope: !21)
!40 = !DILocation(line: 13, column: 10, scope: !21)
!41 = !DILocation(line: 14, column: 1, scope: !21)
