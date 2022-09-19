int arr[32];

// Requires true
// Ensures forall i,j :: 16 <= i <= j < 32 ==> arr[i] <= arr[j]
void first_half() {
    int i, e, j;
    for (i = 17; i < 32; i++) {
        e = arr[i];
        j = i - 1;
        while (j >= 16 && arr[j] > e) {
            arr[j + 1] = arr[j];
            j = j - 1;
        }
        arr[j + 1] = e;
    }
}
