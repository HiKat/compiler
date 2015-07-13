int comp_num(int a, int b);
int sort_array[8];


int comp_num(int a, int b){
    if(a > b) return 0;
    if(a < b) return 1;
    if(a == b) return 2;
}


int main(){
    int i;
    int j;
    int h;

    sort_array[0] = 6;
    sort_array[1] = 4;
    sort_array[2] = 2;
    sort_array[3] = 5;
    sort_array[4] = 7;
    sort_array[5] = 8;
    sort_array[6] = 1;
    sort_array[7] = 3;

    
    for(j = 8; j > 1; j = (j-1)){
        for(i = 0; i < (j-1); i = (i+1)){
            if(comp_num(sort_array[i+1],sort_array[i])){
                h = sort_array[i+1];
                sort_array[i+1] = sort_array[i];
                sort_array[i] = h;
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


