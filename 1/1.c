#include <stdio.h>
b,c,d,r,s;main(a){FILE*f=fopen("input","r");while(fscanf(f,"%d",&a)>0){r+=a>b&&b;s+=a+b+c>b+c+d&&d;d=c;c=b;b=a;};printf("%d %d\n",r,s);fclose(f);}
