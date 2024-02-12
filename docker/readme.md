

---

### Using Containers 

(This method has only been tested on linux)

The docker config can be used to provide bap with the asli-plugin as well as compile the tool itself.

Requirements:

- podman, podman-compose

#### 1. Obtain the image image

##### From github 

`podman pull ghcr.io/uq-pac/basil-dev:latest`

##### Build the images

To build the images, from the root of the respository run

```
podman-compose build
```

#### 2. Use the images with compose 

Individual services can be built with `podman compose build $servicename` from the root of the repo. 

The services provided are:

- `basil-dev` dev environment containng scala build environment, bap and cross-compilers
   - To compile basil into the current directory using the sbt and scala provided by the docker image: 
      - `podman compose run basil-dev sbt assembly`
   - To enter a shell inside the container
      - `podman compose run basil-build`
- `basil-build`
   - The same as above however containg only the scala build environment and compiled basil
   - To recompile with the currently checked-out repo run `podman-compose build basil-build`
   - To enter a shell inside the container
      - `podman compose run basil-build`
- `basil` precompiled jar file and tools
   - To run the jar inside the docker image `podman-compose run basil-dev $arguments...`
- `compiler-explorer`
    - instance of [godbolt.org](godbolt.org) with the tools installed. 

Or enter the dev container manually, mounting the current directory (the same as podman-compose basil-dev).


#### 2. Use the images manually  

##### Compiler Explorer Container

```sh
podman pull ghcr.io/uq-pac/basil-compiler-explorer:latest
```


```sh
podman-compose run compiler-explorer
```

OR

```
podman run -p 10240:10240 ghcr.io/uq-pac/basil-compiler-explorer:latest
```

##### Development/Testing Container

This mounts the current directory as the working directory of the container.

```
podman pull ghcr.io/uq-pac/basil-dev /bin/bash
podman run -v .:/host -w /host -it ghcr.io/uq-pac/basil-dev /bin/bash
```



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

