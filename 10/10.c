#include <stdio.h>
#include <string.h>
r,*f,i,c,t[99];long s[99];int qs(long*a, long*b){return*a<*b?-1:*a>*b;}main(a){f=fopen("input","r");while((c=getc(f))!=EOF){if(c=='\n'){while(i){s[a]=s[a]*5+(t[i]=='>'?4:t[i]=='}'?3:t[i]==']'?2:1);i--;}a++;}else if(c==t[i]){t[i--]=0;}else if(c==']'||c==')'||c=='>'||c=='}'){r+=c=='>'?25137:c=='}'?1197:c==']'?57:3;i=0;while(getc(f)!='\n');}else{t[++i]=c=='('?')':c=='['?']':c=='{'?'}':'>';}}qsort(s,a,8,qs);printf("%d %ld\n", r, s[a/2]);fclose(f);}
