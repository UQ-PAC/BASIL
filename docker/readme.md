

---

#### Publishing container images to github registry:

- This only needs to be done when the docker images are modified or you wish to 
make a new version of basil available. Only the dev environment is used 
by the github actions.

1. Create a github access token with the priviledge to write to packages
2. Login to repository with podman

```
$ podman login ghcr.io -u $username
Password: <enter github acccess token>
```

3. Push the container

```
$ podman-compose push basil
$ podman-compose push basil-dev
```

# Compiler-Explorer

The configuration used to run add the tool to compiler explorer lives here: https://github.com/ailrst/compiler-explorer

