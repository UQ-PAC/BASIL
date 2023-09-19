#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
        int a;
} A;

typedef struct {
        void (*update)(A*);
} A_functable;

void A_func1(A* this) {printf("Func1\n");}
void A_func2(A* this) {printf("Func2\n");}
void A_func3(A* this) {printf("Func3\n");}
A_functable A_vmt = {A_func1};
A_functable A_vmt2 = {A_func2};
A_functable A_vmt3 = {A_func1};

A_functable *newInstance(A_functable* vmt) {
        A_functable *newTable = malloc(sizeof(A_functable));
        memcpy(newTable, vmt, sizeof(A_functable));
        return newTable;
}

int main(int argc) {
        A_functable *tbl = 0;
        A thisParameter;
        void (*funcptr)(A*);

        switch (argc) {
                case 0:
                        tbl = newInstance(&A_vmt);
                        break;
                case 1:
                        tbl = newInstance(&A_vmt2);
                        break;
                case 2:
                        tbl = newInstance(&A_vmt3);
                        break;

        }

        funcptr = (tbl->update);
        (*funcptr)(&thisParameter);
}