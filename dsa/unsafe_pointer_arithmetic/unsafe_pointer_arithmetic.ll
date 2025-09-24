; ModuleID = '/host/dsa/unsafe_pointer_arithmetic/unsafe_pointer_arithmetic.c'
source_filename = "/host/dsa/unsafe_pointer_arithmetic/unsafe_pointer_arithmetic.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main() #0 !dbg !11 {
  %1 = alloca i32*, align 8
  %2 = alloca i8*, align 8
  %3 = alloca i32*, align 8
  %4 = alloca i32**, align 8
  %5 = alloca i32*, align 8
  call void @llvm.dbg.declare(metadata i32** %1, metadata !15, metadata !DIExpression()), !dbg !16
  %6 = call noalias i8* @malloc(i64 20) #3, !dbg !17
  %7 = bitcast i8* %6 to i32*, !dbg !17
  store i32* %7, i32** %1, align 8, !dbg !16
  %8 = load i32*, i32** %1, align 8, !dbg !18
  store i32 12, i32* %8, align 4, !dbg !19
  call void @llvm.dbg.declare(metadata i8** %2, metadata !20, metadata !DIExpression()), !dbg !21
  %9 = load i32*, i32** %1, align 8, !dbg !22
  %10 = bitcast i32* %9 to i8*, !dbg !23
  store i8* %10, i8** %2, align 8, !dbg !21
  call void @llvm.dbg.declare(metadata i32** %3, metadata !24, metadata !DIExpression()), !dbg !25
  %11 = load i8*, i8** %2, align 8, !dbg !26
  %12 = getelementptr i8, i8* %11, i64 1, !dbg !27
  %13 = bitcast i8* %12 to i32*, !dbg !28
  store i32* %13, i32** %3, align 8, !dbg !25
  call void @llvm.dbg.declare(metadata i32*** %4, metadata !29, metadata !DIExpression()), !dbg !31
  %14 = call noalias i8* @malloc(i64 8) #3, !dbg !32
  %15 = bitcast i8* %14 to i32**, !dbg !32
  store i32** %15, i32*** %4, align 8, !dbg !31
  %16 = load i32*, i32** %3, align 8, !dbg !33
  %17 = load i32**, i32*** %4, align 8, !dbg !34
  store i32* %16, i32** %17, align 8, !dbg !35
  call void @llvm.dbg.declare(metadata i32** %5, metadata !36, metadata !DIExpression()), !dbg !37
  %18 = load i32**, i32*** %4, align 8, !dbg !38
  %19 = load i32*, i32** %18, align 8, !dbg !39
  store i32* %19, i32** %5, align 8, !dbg !37
  ret i32 0, !dbg !40
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nounwind
declare dso_local noalias i8* @malloc(i64) #2

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!7, !8, !9}
!llvm.ident = !{!10}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, retainedTypes: !3, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/host/dsa/unsafe_pointer_arithmetic/unsafe_pointer_arithmetic.c", directory: "/")
!2 = !{}
!3 = !{!4, !5}
!4 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!5 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !6, size: 64)
!6 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!7 = !{i32 7, !"Dwarf Version", i32 4}
!8 = !{i32 2, !"Debug Info Version", i32 3}
!9 = !{i32 1, !"wchar_size", i32 4}
!10 = !{!"clang version 10.0.0-4ubuntu1 "}
!11 = distinct !DISubprogram(name: "main", scope: !12, file: !12, line: 4, type: !13, scopeLine: 4, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!12 = !DIFile(filename: "/host/dsa/unsafe_pointer_arithmetic/unsafe_pointer_arithmetic.c", directory: "")
!13 = !DISubroutineType(types: !14)
!14 = !{!6}
!15 = !DILocalVariable(name: "bar", scope: !11, file: !12, line: 5, type: !5)
!16 = !DILocation(line: 5, column: 10, scope: !11)
!17 = !DILocation(line: 5, column: 16, scope: !11)
!18 = !DILocation(line: 6, column: 6, scope: !11)
!19 = !DILocation(line: 6, column: 10, scope: !11)
!20 = !DILocalVariable(name: "car", scope: !11, file: !12, line: 7, type: !4)
!21 = !DILocation(line: 7, column: 12, scope: !11)
!22 = !DILocation(line: 7, column: 26, scope: !11)
!23 = !DILocation(line: 7, column: 18, scope: !11)
!24 = !DILocalVariable(name: "foo", scope: !11, file: !12, line: 8, type: !5)
!25 = !DILocation(line: 8, column: 10, scope: !11)
!26 = !DILocation(line: 8, column: 24, scope: !11)
!27 = !DILocation(line: 8, column: 28, scope: !11)
!28 = !DILocation(line: 8, column: 16, scope: !11)
!29 = !DILocalVariable(name: "tar", scope: !11, file: !12, line: 9, type: !30)
!30 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !5, size: 64)
!31 = !DILocation(line: 9, column: 12, scope: !11)
!32 = !DILocation(line: 9, column: 18, scope: !11)
!33 = !DILocation(line: 10, column: 12, scope: !11)
!34 = !DILocation(line: 10, column: 6, scope: !11)
!35 = !DILocation(line: 10, column: 10, scope: !11)
!36 = !DILocalVariable(name: "bat", scope: !11, file: !12, line: 11, type: !5)
!37 = !DILocation(line: 11, column: 10, scope: !11)
!38 = !DILocation(line: 11, column: 17, scope: !11)
!39 = !DILocation(line: 11, column: 16, scope: !11)
!40 = !DILocation(line: 12, column: 1, scope: !11)
