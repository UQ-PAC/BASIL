#include <stdio.h>
#include <stdlib.h>

/* Define constants */
#define MAX_LINE_LENGTH 256

/* Function prototypes */
void handleInput(char* input);
void printHelp();
void openFile(char* filename);
void saveFile();
void closeFile();

/* Global variables */
char* currentFilename = NULL;
FILE* currentFile = NULL;

int main() {
    char input[MAX_LINE_LENGTH];

    while (1) {
        printf("Enter a command: ");
        fgets(input, MAX_LINE_LENGTH, stdin);

        // Remove trailing newline character
        input[strcspn(input, "\n")] = '\0';

        handleInput(input);
    }

    return 0;
}

void handleInput(char* input) {
    if (strcmp(input, "help") == 0) {
        printHelp();
    } else if (strncmp(input, "open ", 5) == 0) {
        char* filename = input + 5;
        openFile(filename);
    } else if (strcmp(input, "save") == 0) {
        saveFile();
    } else if (strcmp(input, "close") == 0) {
        closeFile();
    } else if (strcmp(input, "exit") == 0) {
        if (currentFile != NULL) {
            closeFile();
        }
        exit(0);
    } else {
        printf("Unknown command. Type 'help' for a list of commands.\n");
    }
}

void printHelp() {
    printf("Commands:\n");
    printf("  help                 - Display this help message\n");
    printf("  open <filename>      - Open a file for editing\n");
    printf("  save                 - Save changes to the current file\n");
    printf("  close                - Close the current file\n");
    printf("  exit                 - Exit the editor\n");
}

void openFile(char* filename) {
    if (currentFile != NULL) {
        closeFile();
    }

    currentFilename = strdup(filename);
    currentFile = fopen(currentFilename, "r+");

    if (currentFile == NULL) {
        printf("Failed to open file '%s'\n", currentFilename);
    } else {
        printf("Opened file '%s'\n", currentFilename);
    }
}

void saveFile() {
    if (currentFile == NULL) {
        printf("No file is currently open\n");
        return;
    }

    rewind(currentFile);

    // TODO: Implement saving changes to the file

    printf("Saved changes to file '%s'\n", currentFilename);
}

void closeFile() {
    if (currentFile != NULL) {
        fclose(currentFile);
        free(currentFilename);
        currentFile = NULL;
        currentFilename = NULL;
        printf("Closed the file\n");
    }
}

