
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
  i=0; 
  while (i < n) {
    *sump = *sump + arr[i];
    i=i+1;
  }
}

void squares(int n, int arr[]) {
  int i; 
  i=0; 

  while (i < n) {
    arr[i] = i*i;
    i=i+1;
  }
}
