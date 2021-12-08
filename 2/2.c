#include <stdio.h>
p,
d,
e,
a;
char i[7];
main(n){
  FILE*f=fopen("test","r");
  while(fscanf(f,"%s %d\n",i,&n)>0){
    *i=='d'?
        d+=n,
        a+=n:
    *i=='u'?
        d-=n,
        a-=n:
    1?
        e+=p*n*a,
        p+=n
    :0;
  };
  printf("%d %d\n",p*d,e);
  fclose(f);
}
