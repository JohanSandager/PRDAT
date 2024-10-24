
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
    i=0; 

    while (i < max) {
        print freq[i];
        i = i+1;
    }

}
void histogram(int n, int ns[], int max, int freq[]) {
    int i; 
    i=0; 
    while (i < max) {
        freq[i] = 0;
        i = i+1;
    } 

    i=0; 

    while (i < n) {
        int x;
        x = ns[i];
        freq[x] = freq[x] + 1;
        i=i+1;
    }
}
