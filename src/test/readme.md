
Options specified in make/lift-directories.mk can be overriden using this file, for example to specify the lifting templates:

```
$ cat correct/test_name/config.mk
ENABLED_COMPILERS = clang clang_pic gcc gcc_pic
```


Lift all:

```
make
```

Lift one:

```
                                # relative to correct/secret_write
make -C correct/secret_write -f ../../make/lift-directories.mk
```

or

```
cd correct/secret_write
make -f ../../make/lift-directories.mk
```

