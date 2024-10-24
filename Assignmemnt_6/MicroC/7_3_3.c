
void main(int n) {
    int ns[7];
    ns[0] = 1;
    ns[1] = 2;
    ns[2] = 1;
    ns[3] = 1;
    ns[4] = 1;
    ns[5] = 2;
    ns[6] = 0;

    int max;
    max=3;
    
    int freq[3];
    histogram(n, ns, max, freq);

    int i; 
    for (i=0; i < max; i=i+1) {
        print freq[i];
    }

}
void histogram(int n, int ns[], int max, int freq[]) {
    int i; 
    for (i = 0; i < max; i=i+1) {
        freq[i] = 0;
    } 

    for (i = 0; i < n; i=i+1) {
        int x;
        x = ns[i];
        freq[x] = freq[x] + 1;
    }
}
