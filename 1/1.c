#include <stdio.h>
b,c,d,r,s,*f;main(a){f=fopen("input","r");while(fscanf(f,"%d",&a)>0){r+=a>b&&b;s+=a>d&&d;d=c;c=b;b=a;};printf("%d %d\n",r,s);fclose(f);}
