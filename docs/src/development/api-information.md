# API and Frontend Interface Information Page

This page discusses the set-up and use of BASIL's API and frontend development interface. 

## Set up
To ensure all required dependencies are available, follow these steps:
1. Install Frontend Dependencies: The web component uses a tool Vite that requires Node modules. 
Install this before running the development server:
```shell
npm install
```
2. Run the _API_ Server (Backend). The following command uses the Mill build tool to automatically 
download and compile all necessary Scala libraries, then starts the _API_ server.
```shell
./mill runMain API/ApiServer
```
3. Run the Web Interface (Frontend): In a separate terminal, start the development server
for the user interface. 
```shell
npm run dev
```
---

## Development Style Guide for Future Development
This section outlines the style guide and essential knowledge of tools required
before further development on the interface.

### Prettier
Prettier has been selected as the project's automatic code formatter. Before pushing
any changes, run the following command to automatically apply this style to all 
JavaScript, JSX, JSON, TypeScript, and CSS files. 

### Frontend Naming Conventions
Review the following established naming conventions for all new components, variables, and files:

| System Component | Naming Convention | Example | Specific Rule/Prefix | 
 | ----- | ----- | ----- | ----- | 
| **Files** (.tsx, etc.), **Interfaces**, **React Components** | PascalCase | `CfgViewer.tsx`, `IRData.ts` | **Capitalise** every word. | 
| **CSS files**, **Assets**, **Utility .ts files** (non-component) | kebab-case | `spinner-loading-icon.svg`, `modal-base.css` | Use hyphens between words. | 
| **Variables** and **Functions** | camelCase | `loadIrData()`, `currentEpochIndex` | Start lowercase, **capitalise** subsequent words. | 
| **Booleans** | camelCase | `isDarkMode`, `hasError` | Must use `is` or `has` prefix. | 
| **Hooks** | camelCase | `useCfgData.ts` | Must use the `use` prefix. |


### Git commits
For each of the frontend commits, I (Jesse) added the name of the branch first: 
{Branch_name}: {Commit message}
<br>
e.g. Jesse-interface-thesis-proj: Creates a basic GET API endpoint which displays 
an IR in a html script

## Future Recommendations for the Interface
Several future recommendations have been listed below. 

### Example 1...

## Documentation
This outlines briefly some documentation standards I have followed

---
# Development Process
This section is a brief summary describing some of the design decisions.

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

- I am now using react + TypeScript. 

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


