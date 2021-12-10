#include <stdio.h>
#include <stdint.h>
r,*f,l[99],i,p;
long long s[99];
char t[99];
char*c;

int qs(long long*a,long long*b){return(*a)<(*b)?-1:(*a)>(*b)?1:0;}

char flip(c) {
  switch (c) {
    case '(': return ')';
    case '[': return ']';
    case '{': return '}';
    case '<': return '>';
  }
}

main(a){
  f=fopen("input","r");
  p=0;
  while(fscanf(f,"%s\n",&l)>0){
    // take line
    // parse
    c=l;
    *t=' ';
    i=0;
    //printf("got: %s\n", c);
    for (c=l;(*c)!=0;c++) {

      if (*c == t[i]) {
        t[i--]=0;
      } else if (*c == ']'|| *c==')'|| *c=='>'|| *c=='}') {
        switch(*c){case'>':r+=23940;case'}':r+=1140;case']':r+=54;case')':r+=3;}
        i=0;
        break;
      } else {
        t[++i] = flip(*c);
      }
    }
    if(i>0) {
      //printf("remaining stack: %d %s\n", i, t);
      while(i) {
        //printf("charb: %c %d %d\n", t[i], s[p]);
        s[p] = s[p]*5;
        //printf("charm: %c %d %d\n", t[i], s[p]);
        switch(t[i]){case'>':s[p]+=1;case'}':s[p]+=1;case']':s[p]+=1;case')':s[p]+=1;}
        //printf("char: %c %d %lld\n", t[i], s[p]);
        i--;
      }
      p++;
    }
  }
  qsort(s,p,sizeof(long long),qs);
  printf("done %d %lld\n", r, s[p/2]);
  fclose(f);
}
