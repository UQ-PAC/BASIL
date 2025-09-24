; ModuleID = '/host/dsa/stackambiguity/stackambiguity.c'
source_filename = "/host/dsa/stackambiguity/stackambiguity.c"
target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone
define dso_local i32 @test(i32 %0, i32 %1, i32 %2, i32 %3) #0 !dbg !7 {
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca [4 x i32], align 4
  %11 = alloca i32*, align 8
  store i32 %0, i32* %5, align 4
  call void @llvm.dbg.declare(metadata i32* %5, metadata !12, metadata !DIExpression()), !dbg !13
  store i32 %1, i32* %6, align 4
  call void @llvm.dbg.declare(metadata i32* %6, metadata !14, metadata !DIExpression()), !dbg !15
  store i32 %2, i32* %7, align 4
  call void @llvm.dbg.declare(metadata i32* %7, metadata !16, metadata !DIExpression()), !dbg !17
  store i32 %3, i32* %8, align 4
  call void @llvm.dbg.declare(metadata i32* %8, metadata !18, metadata !DIExpression()), !dbg !19
  call void @llvm.dbg.declare(metadata i32* %9, metadata !20, metadata !DIExpression()), !dbg !21
  store i32 0, i32* %9, align 4, !dbg !21
  call void @llvm.dbg.declare(metadata [4 x i32]* %10, metadata !22, metadata !DIExpression()), !dbg !26
  %12 = getelementptr inbounds [4 x i32], [4 x i32]* %10, i64 0, i64 0, !dbg !27
  %13 = load i32, i32* %5, align 4, !dbg !28
  store i32 %13, i32* %12, align 4, !dbg !27
  %14 = getelementptr inbounds i32, i32* %12, i64 1, !dbg !27
  %15 = load i32, i32* %6, align 4, !dbg !29
  store i32 %15, i32* %14, align 4, !dbg !27
  %16 = getelementptr inbounds i32, i32* %14, i64 1, !dbg !27
  %17 = load i32, i32* %7, align 4, !dbg !30
  store i32 %17, i32* %16, align 4, !dbg !27
  %18 = getelementptr inbounds i32, i32* %16, i64 1, !dbg !27
  %19 = getelementptr inbounds i32, i32* %12, i64 4, !dbg !27
  br label %20, !dbg !27

20:                                               ; preds = %20, %4
  %21 = phi i32* [ %18, %4 ], [ %22, %20 ], !dbg !27
  store i32 0, i32* %21, align 4, !dbg !27
  %22 = getelementptr inbounds i32, i32* %21, i64 1, !dbg !27
  %23 = icmp eq i32* %22, %19, !dbg !27
  br i1 %23, label %24, label %20, !dbg !27

24:                                               ; preds = %20
  call void @llvm.dbg.declare(metadata i32** %11, metadata !31, metadata !DIExpression()), !dbg !33
  %25 = getelementptr inbounds [4 x i32], [4 x i32]* %10, i64 0, i64 2, !dbg !34
  store i32* %25, i32** %11, align 8, !dbg !33
  %26 = load i32, i32* %8, align 4, !dbg !35
  %27 = icmp slt i32 %26, 3, !dbg !37
  br i1 %27, label %28, label %32, !dbg !38

28:                                               ; preds = %24
  %29 = load i32, i32* %8, align 4, !dbg !39
  %30 = sext i32 %29 to i64, !dbg !41
  %31 = getelementptr inbounds [4 x i32], [4 x i32]* %10, i64 0, i64 %30, !dbg !41
  store i32* %31, i32** %11, align 8, !dbg !42
  br label %32, !dbg !43

32:                                               ; preds = %28, %24
  %33 = load i32*, i32** %11, align 8, !dbg !44
  %34 = load i32, i32* %33, align 4, !dbg !45
  %35 = add nsw i32 %34, 1, !dbg !46
  store i32 %35, i32* %9, align 4, !dbg !47
  %36 = load i32, i32* %9, align 4, !dbg !48
  ret i32 %36, !dbg !49
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone
define dso_local i32 @main(i32 %0, i8** %1) #0 !dbg !50 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i8**, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  store i32 0, i32* %3, align 4
  store i32 %0, i32* %4, align 4
  call void @llvm.dbg.declare(metadata i32* %4, metadata !56, metadata !DIExpression()), !dbg !57
  store i8** %1, i8*** %5, align 8
  call void @llvm.dbg.declare(metadata i8*** %5, metadata !58, metadata !DIExpression()), !dbg !59
  call void @llvm.dbg.declare(metadata i32* %6, metadata !60, metadata !DIExpression()), !dbg !61
  %11 = load i32, i32* %4, align 4, !dbg !62
  %12 = add nsw i32 4, %11, !dbg !63
  store i32 %12, i32* %6, align 4, !dbg !61
  call void @llvm.dbg.declare(metadata i32* %7, metadata !64, metadata !DIExpression()), !dbg !65
  %13 = load i32, i32* %4, align 4, !dbg !66
  %14 = mul nsw i32 5, %13, !dbg !67
  store i32 %14, i32* %7, align 4, !dbg !65
  call void @llvm.dbg.declare(metadata i32* %8, metadata !68, metadata !DIExpression()), !dbg !69
  %15 = load i32, i32* %4, align 4, !dbg !70
  %16 = sub nsw i32 6, %15, !dbg !71
  store i32 %16, i32* %8, align 4, !dbg !69
  call void @llvm.dbg.declare(metadata i32* %9, metadata !72, metadata !DIExpression()), !dbg !73
  %17 = load i32, i32* %6, align 4, !dbg !74
  %18 = load i32, i32* %7, align 4, !dbg !75
  %19 = load i32, i32* %8, align 4, !dbg !76
  %20 = load i32, i32* %4, align 4, !dbg !77
  %21 = call i32 @test(i32 %17, i32 %18, i32 %19, i32 %20), !dbg !78
  store i32 %21, i32* %9, align 4, !dbg !73
  call void @llvm.dbg.declare(metadata i32* %10, metadata !79, metadata !DIExpression()), !dbg !80
  %22 = load i32, i32* %7, align 4, !dbg !81
  %23 = load i32, i32* %8, align 4, !dbg !82
  %24 = load i32, i32* %4, align 4, !dbg !83
  %25 = call i32 @test(i32 %22, i32 %23, i32 90, i32 %24), !dbg !84
  store i32 %25, i32* %10, align 4, !dbg !80
  %26 = load i32, i32* %9, align 4, !dbg !85
  %27 = load i32, i32* %10, align 4, !dbg !86
  %28 = add nsw i32 %26, %27, !dbg !87
  ret i32 %28, !dbg !88
}

attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+neon" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0-4ubuntu1 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "/host/dsa/stackambiguity/stackambiguity.c", directory: "/")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 10.0.0-4ubuntu1 "}
!7 = distinct !DISubprogram(name: "test", scope: !8, file: !8, line: 1, type: !9, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!8 = !DIFile(filename: "/host/dsa/stackambiguity/stackambiguity.c", directory: "")
!9 = !DISubroutineType(types: !10)
!10 = !{!11, !11, !11, !11, !11}
!11 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!12 = !DILocalVariable(name: "a", arg: 1, scope: !7, file: !8, line: 1, type: !11)
!13 = !DILocation(line: 1, column: 14, scope: !7)
!14 = !DILocalVariable(name: "b", arg: 2, scope: !7, file: !8, line: 1, type: !11)
!15 = !DILocation(line: 1, column: 21, scope: !7)
!16 = !DILocalVariable(name: "c", arg: 3, scope: !7, file: !8, line: 1, type: !11)
!17 = !DILocation(line: 1, column: 28, scope: !7)
!18 = !DILocalVariable(name: "d", arg: 4, scope: !7, file: !8, line: 1, type: !11)
!19 = !DILocation(line: 1, column: 35, scope: !7)
!20 = !DILocalVariable(name: "q", scope: !7, file: !8, line: 2, type: !11)
!21 = !DILocation(line: 2, column: 7, scope: !7)
!22 = !DILocalVariable(name: "x", scope: !7, file: !8, line: 3, type: !23)
!23 = !DICompositeType(tag: DW_TAG_array_type, baseType: !11, size: 128, elements: !24)
!24 = !{!25}
!25 = !DISubrange(count: 4)
!26 = !DILocation(line: 3, column: 7, scope: !7)
!27 = !DILocation(line: 3, column: 14, scope: !7)
!28 = !DILocation(line: 3, column: 15, scope: !7)
!29 = !DILocation(line: 3, column: 18, scope: !7)
!30 = !DILocation(line: 3, column: 21, scope: !7)
!31 = !DILocalVariable(name: "y", scope: !7, file: !8, line: 4, type: !32)
!32 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !11, size: 64)
!33 = !DILocation(line: 4, column: 8, scope: !7)
!34 = !DILocation(line: 4, column: 13, scope: !7)
!35 = !DILocation(line: 5, column: 7, scope: !36)
!36 = distinct !DILexicalBlock(scope: !7, file: !8, line: 5, column: 7)
!37 = !DILocation(line: 5, column: 9, scope: !36)
!38 = !DILocation(line: 5, column: 7, scope: !7)
!39 = !DILocation(line: 6, column: 12, scope: !40)
!40 = distinct !DILexicalBlock(scope: !36, file: !8, line: 5, column: 14)
!41 = !DILocation(line: 6, column: 10, scope: !40)
!42 = !DILocation(line: 6, column: 7, scope: !40)
!43 = !DILocation(line: 7, column: 3, scope: !40)
!44 = !DILocation(line: 8, column: 8, scope: !7)
!45 = !DILocation(line: 8, column: 7, scope: !7)
!46 = !DILocation(line: 8, column: 10, scope: !7)
!47 = !DILocation(line: 8, column: 5, scope: !7)
!48 = !DILocation(line: 9, column: 10, scope: !7)
!49 = !DILocation(line: 9, column: 3, scope: !7)
!50 = distinct !DISubprogram(name: "main", scope: !8, file: !8, line: 12, type: !51, scopeLine: 12, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!51 = !DISubroutineType(types: !52)
!52 = !{!11, !11, !53}
!53 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !54, size: 64)
!54 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !55, size: 64)
!55 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_unsigned_char)
!56 = !DILocalVariable(name: "argc", arg: 1, scope: !50, file: !8, line: 12, type: !11)
!57 = !DILocation(line: 12, column: 14, scope: !50)
!58 = !DILocalVariable(name: "argv", arg: 2, scope: !50, file: !8, line: 12, type: !53)
!59 = !DILocation(line: 12, column: 27, scope: !50)
!60 = !DILocalVariable(name: "a", scope: !50, file: !8, line: 13, type: !11)
!61 = !DILocation(line: 13, column: 7, scope: !50)
!62 = !DILocation(line: 13, column: 15, scope: !50)
!63 = !DILocation(line: 13, column: 13, scope: !50)
!64 = !DILocalVariable(name: "b", scope: !50, file: !8, line: 14, type: !11)
!65 = !DILocation(line: 14, column: 7, scope: !50)
!66 = !DILocation(line: 14, column: 15, scope: !50)
!67 = !DILocation(line: 14, column: 13, scope: !50)
!68 = !DILocalVariable(name: "c", scope: !50, file: !8, line: 15, type: !11)
!69 = !DILocation(line: 15, column: 7, scope: !50)
!70 = !DILocation(line: 15, column: 15, scope: !50)
!71 = !DILocation(line: 15, column: 13, scope: !50)
!72 = !DILocalVariable(name: "d", scope: !50, file: !8, line: 16, type: !11)
!73 = !DILocation(line: 16, column: 7, scope: !50)
!74 = !DILocation(line: 16, column: 16, scope: !50)
!75 = !DILocation(line: 16, column: 19, scope: !50)
!76 = !DILocation(line: 16, column: 22, scope: !50)
!77 = !DILocation(line: 16, column: 25, scope: !50)
!78 = !DILocation(line: 16, column: 11, scope: !50)
!79 = !DILocalVariable(name: "e", scope: !50, file: !8, line: 17, type: !11)
!80 = !DILocation(line: 17, column: 7, scope: !50)
!81 = !DILocation(line: 17, column: 16, scope: !50)
!82 = !DILocation(line: 17, column: 19, scope: !50)
!83 = !DILocation(line: 17, column: 26, scope: !50)
!84 = !DILocation(line: 17, column: 11, scope: !50)
!85 = !DILocation(line: 18, column: 10, scope: !50)
!86 = !DILocation(line: 18, column: 14, scope: !50)
!87 = !DILocation(line: 18, column: 12, scope: !50)
!88 = !DILocation(line: 18, column: 3, scope: !50)
