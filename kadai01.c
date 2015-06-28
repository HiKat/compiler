int func(int a){
    a = 3;
    return a;
}
int func2(int a){
    a = 2;
    return a;
}
void func3();
int main(){
    return func(4)+func2(1);
}
