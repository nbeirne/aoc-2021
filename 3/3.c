#include <stdio.h>
#include <inttypes.h>
r,s,f;
c[5];
main(a){
    f=fopen("input","r");
    (fscanf(f,"%s",c)>0){
        a=strtol(c,c+5,2);
    };
    printf("%d %d\n",r,s);
    fclose(f);
}
