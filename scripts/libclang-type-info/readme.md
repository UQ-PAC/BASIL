
This is a c++ program that creates a single translation unit in libclang, traverses the AST to 
find all function declarations, and dumps their type signature in JSON format. 


```
Dump C function declaration signatures
Usage:
  ./bin/main [OPTION...]

  -f, --file arg           Source file
  -t, --target arg         Clang target triple   
                           (https://clang.llvm.org/docs/CrossCompilation.html#target-triple) (default: aarch64-linux-gnu)
  -c, --clang-options arg  Additional options to clang
```

Example output:

`./bin/main -f test/test.c`


```json
...
        "malloc": {
            "is_variadic": false,
            "location": {
                "column": 14,
                "file_name": "/usr/include/stdlib.h",
                "line": 553
            },
            "name": "malloc",
            "params": [
                {
                    "name": "__size",
                    "type": {
                        "is_func_ptr": false,
                        "is_void": false,
                        "name": "size_t",
                        "size": 8
                    }
                }
            ],
            "result_type": {
                "is_func_ptr": true,
                "is_void": false,
                "name": "void *",
                "size": 8
            }
        }
...
```

