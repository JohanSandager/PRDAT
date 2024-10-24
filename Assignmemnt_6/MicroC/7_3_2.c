
void main(int n) {
    int arr[20];

    int j;
    j=0;

    squares(n, arr);
    arrsum(n, arr, &j);
    print j;
}

void arrsum(int n, int arr[], int *sump) {
  int i; 
  for (i = 0; i < n; i=i+1) {
    *sump = *sump + arr[i];
  }
}

void squares(int n, int arr[]) {
  int i; 
  for (i = 0; i < n; i=i+1) {
    arr[i] = i*i;
  }
}
