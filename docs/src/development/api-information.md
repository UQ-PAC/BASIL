# Transform Viewer GUI

The frontend project is stored in a separate repository: [UQ-PAC/basil-gui-frontend](https://github.com/UQ-PAC/basil-gui-frontend).
It is included here as a git submodule, to use the gui ensure this submodule is checked out:

```
git submodule init
git submodule update
```

Then run basil with the server entrypoint:

```shell
./mill runMain server.ApiServer <reguar basil args>
```

And open `http://localhost:8080` in a browser.

## Adding Additional Snapshots

Most epoch logic is placed in src/main/scala/util/RunUtils.scala or a related helper.

The collectedSnapshots arrabuffer is passed through runutils, the following can be used to wrap
a transform function so that it logs the diff (this is automatically disabled when the server is
not runnign but the transform is still applied).

```scala
logTransform[T](collectedSnapshots)("unique-transform-name", transformFunc: IRContext => T)(ctx)
```


### Naming Conventions

The epoch name (e.g., "name_of_the_new_transform") must be a unique and URI-compatible (not contain "/"), 
string that accurately describes the transformation that just took place.
This name is what the front-end uses to identify and fetch the corresponding program state pair.
