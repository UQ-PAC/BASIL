L: arr[] -> true
Rely: forall i :: 16 <= i < 32 ==> arr[i] == old(arr[i])
Guarantee: forall i :: 0 <= i < 16 ==> arr[i] == old(arr[i])