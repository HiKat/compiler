int func1(int a, int b);
int func2(int a, int b);
int array[2];

void main(){
    array[0] = 100;
    array[1] = 9999; 
    func2(999, func1(1, 9));
}

int func1(int a, int b){
    b = (a + b);
    if(b > 8){
        b = (b * 100);
    }
    else{
        b = 0;
    }
    return b;
}

int func2(int a, int b){
    int d;
    while(a > b){
        b = ((b + 1) * 2);
    }
    array[0] = b;
    array[1] = a;
    d = (array[0]+array[1]);
    return d;
}
