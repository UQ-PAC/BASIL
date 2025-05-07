# API Information Page

The discusses the set-up and use of BASIL's API. 

## Set up
In order to run the API, use the following command line: 
```shell
./mill runMain API/ApiServer
```

## Documentation
This outlines briefly some documentation standards I have followed

# Git commits
For each of my commits, I have added the name of the branch first: {Branch_name}: {Commit message}
<br>
e.g. Jesse-interface-thesis-proj: Creates a basic GET API endpoint which displays an IR in a html script




---
# Development Process
This section is a brief summary describing my design decisions - both with the UI and the code 
architecture. 

## Language and Libraries
I am using scala for the backend API to ensure. This allows direct interaction with the API endpoints
and the scala code that generates the IR and modifies it through transform passes. 
<br>
The main library used is http4s. It is a type-safe HTTP library for Scala. 

## API type
I am using a RestAPI which ....

## Architecture

# Jesse Random ideas and Notes
Here I have a list of a few of my choices and ideas. This will be tidied up later

## Code Architecture
I call a html file with code to display the IR atm. 

## I am up to
I have displayed an IR now. I have not run it through any transform passes yet. This still needs to be done. 


