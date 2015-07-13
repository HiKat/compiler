int comp_num(int a, int b, int c);
int sort_array[8];

int comp_num(int a, int b, int c){
    if(a > b) return 0;
    if((a < 0) || (a == (b + 9999))) return 9999;
    if(a < 0) return 100;
    if(1) return 234;
}


int main(){
    int i;
    int j;
    int h;
    int x, m, y, z;
    z =((x + m) + (x + m));
    z = z + 332;
    z = comp_num(222, m, m);
    z = comp_num(111, comp_num(8, 9, 7), 0) + comp_num(2, 3, 7);
    
    for(j = 8; j > 1; j = (j-1)){
        
        int hh;
        
        for(i = 0; i < (j-1); i = (i+1)){
            if(comp_num(sort_array[i+1],sort_array[i], 7)){
                h = sort_array[i+1];
                sort_array[i+1] = sort_array[i];
                sort_array[i] = h;
                
                x = 100;
                hh = 199;
                
            }
            
        }
    }
    i = 0;
    while(8 > i){
        if(i != 7){
        print(sort_array[i]);
        }
        else(print(sort_array[7]));
        i = (i+1);
    }
    
}
int comp_num(int x, int y, int j);


