# Version numbering

Basil has a version numbering system which allows the built binary to report
its version and Git tag/commit.

You can view the current version information with the `--version` flag, for example:
```console
$ ./mill run --version
The Basil Pipeline
version: 0.1.2-alpha-1013-g6e0ea8d26-dirty
```

The version information comes from Git and is obtained from `git describe --tags --dirty`.
The format is:
```
[most recent tag]-[commits since that tag]-g[commit hash]-[dirty flag]
```
Any of the last 3 components can be omitted if it exactly matches the preveious component.

## Incrementing the version

To increment the version, simply add a new Git tag.
The new version will be detected on subsequent builds.
A git tag can be added with `git tag`, then `git push --tags`.
Alternatively, Github releases can be used.

Note that this means that building from source might produce different versions depending
on whether the tag has been pushed.
To avoid this discrepancy as much as possible, it is recommend to add the tag as soon as
the relevant commit is pushed to main.


