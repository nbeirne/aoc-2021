#include <stdio.h>
int x,y;
p,d,e,a,t;char i[7];main(n){
  FILE*f=fopen("input","r");
  while(fscanf(f,"%s %d\n",i,&n)>0){
    if(*i=='f') {
        x+=n;
    }
    if (*i =='d') {
        y+=n;

    }
    if (*i =='u') {
        y-=n;
    }

    //*i=='d'?d+=n,a+=n:*i=='u'?d-=n,a-=n:1?p+=n,e+=n*a:0;
  };
  printf("%d %d\n",p*d,p*e);
  printf("%d %d\n",x*y,p*e);
  fclose(f);
}
