
void main(int n) {
    int arr[4];
    arr[0] = 7;
    arr[1] = 13;
    arr[2] = 9;
    arr[3] = 8;

    int j;
    j=0;
    arrsum(n, arr, &j);
    print j;
}

void arrsum(int n, int arr[], int *sump) {
  int i;
  for (i = 0; i < n; ++i) {
    *sump = *sump + arr[i];
  }
}
