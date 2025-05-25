# API Information Page

The discusses the set-up and use of BASIL's API. 

## Set up
In order to run the API, use the following command line: 
```shell
./mill runMain API/ApiServer
```
---
In order to change th test file, navigate to ApiServer 
located at (src/main/scala/API/ApiServer.scala) and change the 
inputFile and relfFile. 

## Documentation
This outlines briefly some documentation standards I have followed

### Git commits
For each of my commits, I have added the name of the branch first: {Branch_name}: {Commit message}
<br>
e.g. Jesse-interface-thesis-proj: Creates a basic GET API endpoint which displays an IR in a html script

### Javascript style
I should get a code style plugin for this


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
I am using a RestAPI which using the standard GET method. So far I am only using this method. 
There is no need to implement a POST, PUT or GET yet. 

### GET protocol
There are two version of the Get protocol. They are:
- GET (api/info) -> retrieve all
- Get (api/info/{id}) -> retrieve specific

## Architecture


# Jesse Random ideas and Notes
Here I have a list of a few of my choices and ideas. This will be tidied up later

## Code Architecture
I call a html file with code to display the IR atm. 

## I am up to
I have displayed an IR now. I have not run it through any transform passes yet. This still needs to be done. 
<br>
25/05/2025: I have displayed two versions on the IR between a selection of transform passes. 
I now need to focus on the view of these elements and clean up the code. I also need more IRs
to select from.


## classes changes
build.sc - I needed to add dependencies (for imports such as http4)
<br>
ApiServer - Code that will run the API endpoints. I am hoping to refactor this later
<br>
api-information.md - ReadMe for the API changes
<br>
index.html - Contains the javascript and future ReactCode
<br>
Simp.scala - This contains code to build the IR. I need split up the IR between transform passes here


