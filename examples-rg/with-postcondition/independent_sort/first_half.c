int arr[32];

// Requires true
// Ensures forall i,j :: 0 <= i <= j < 16 ==> arr[i] <= arr[j]
void first_half() {
    int i, e, j;
    for (i = 1; i < 16; i++) {
        e = arr[i];
        j = i - 1;
        while (j >= 0 && arr[j] > e) {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        arr[j + 1] = e;
    }
}