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

```shell
npm run format
```

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

### Grey Epochs that result in no changes
Check the epochs as they arrive and whether the IR before and after differ in 
any way. Of not, then grey them out, visually displaying no change.

### Potential for Multiple Code-Colour Schemes 
Multiple code Colour schemes be implemented, with the selection potentially in
the settings modal.

### Customisable font Size
Allow the font size to be adjusted from settings. 

### Single Procedure View 
One improvement discussed with developers was to set the IR-IR view to display a single procedure 
by default, maintaining consistency with the other visualisation modes. The option to view the
entire IR would remain available as a manually selected secondary mode.

### Load On Demand Planetesimals
LOD refers to loading only a subsection of the information at a time; as the user 
scrolls down the view, new sections are dynamically loaded, ensuring fast API responses and 
eliminating long wait times. When handling extremely large IRs, rendering thousands of lines 
simultaneously can lead to noticeable delays. Implementing LOD rendering (perhaps loading only 
1000 lines of code at a time) would significantly improve performance.

## Adding Additional Epoch Snapshots
Most epoch logic is placed in src/main/scala/util/RunUtils.scala or a related helper.

### Required Steps to Create a New Epoch

In order to capture the epoch state, you must:

1. Capture the BEFORE State: Create a deep copy of the program before running the transform. 
The call to IRToDSL.convertProgram(ctx.program).resolve ensures a correct, deep snapshot is taken.
2. Run the Transform: Execute the transformation function, which modifies the program 
ctx.program in-place.
3. Capture the AFTER State: Create a deep copy of the program after the transform finishes.
4. Add the Snapshot: Call the addEpochSnapshot helper function, providing the unique name and 
the before/after states.

Example Pattern

The following Scala code demonstrates how to implement an epoch for a hypothetical transform:

```scala
// 1. Capture the BEFORE State
val beforeNewTransformProg = IRToDSL.convertProgram(ctx.program).resolve

// 2. Run the Transform
ir.transforms.newTransform(ctx.program)

// 3. Capture the AFTER State
val afterNewTransformProg = IRToDSL.convertProgram(ctx.program).resolve

// 4. Add the Snapshot
addEpochSnapshot(
"name_of_the_new_transform", // <--- THIS IS THE UNIQUE EPOCH NAME
beforeNewTransformProg,
afterNewTransformProg,
collectedEpochs
)
```


### Naming Conventions
The epoch name (e.g., "name_of_the_new_transform") must be a unique, clear, and descriptive 
string (using snake_case) that accurately describes the transformation that just took place. 
This name is what the front-end uses to identify and fetch the corresponding program state pair.

## Architecture Used
Smart Hook Dumb Component is the main pattern used to enforce good coding standards such as the
separation of concerns principle. For more in depth detail on the Architecture, ask to see Jesse
Graf's Thesis on a Decompiler User Interface from 2025. 
