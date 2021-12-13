#include <stdio.h>
#include <string.h>
r,*f,i,c,
t[99]; // stack
long s[99]; // result for sorting. store number

int qs(long*a, long*b){return*a<*b?-1:*a>*b;}

tr(int v){ // stack
  c=getchar();
  char fl=c=='('?')':c=='['?']':c=='{'?'}':c=='<'?'>':0; // flip
  if (c == v) {
    printf("pop: %c\n", v);
    tr(v);
  } else if (!fl) {
    printf("invalid char: %c, expecting %c\n", c, v);
    r+=c=='>'?25137:c=='}'?1197:c==']'?57:3;
    while(getchar()!='\n');
  } else {
    printf("push: %c\n", fl);
    tr(fl);
  }

  //s[a]=s[a]*5+(c=='>'?4:c=='}'?3:c==']'?2:1); // happen during pop after a newline

  

}

main(a){
  f=fopen("test","r");

  tr(getchar());

  printf("done\n");

  return 0;

  while((c=getc(f))!=EOF){
    if(c=='\n'){
      while(i){
        s[a]=s[a]*5+(t[i]=='>'?4:t[i]=='}'?3:t[i]==']'?2:1);
        i--;
      }
      a++;
    }else if(c==t[i]){
      t[i--]=0;
    }else if(c==']'||c==')'||c=='>'||c=='}'){
      r+=c=='>'?25137:c=='}'?1197:c==']'?57:3;
      i=0;
      while(getc(f)!='\n');
    }else{
      t[++i]=c=='('?')':c=='['?']':c=='{'?'}':'>';
    }
  }
  qsort(s,a,8,qs);
  printf("%d %ld\n", r, s[a/2]);
  fclose(f);
}
