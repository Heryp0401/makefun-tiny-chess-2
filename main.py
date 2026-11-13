import os,json as j,urllib.request as u
H=os.environ.get("MAKEFUN_ENV_URL")
W=[0,82,337,365,477,1025,0]
KN=[-33,-31,-18,-14,14,18,31,33]
KI=[-17,-16,-15,-1,1,15,16,17]
BD=[-17,-15,15,17]
RD=[-16,-1,1,16]
D={4:3,0:2,7:1,116:12,112:8,119:4}
MA=100000
CS=[(1,4,1,7),(2,4,-1,0),(4,116,1,119),(8,116,-1,112)]
class P:
 def __init__(s):s.b=[0]*128;s.s=1;s.c=0
def F(f):
 p=P();a=f.split();r=a[0].split('/')
 for i,w in enumerate(r):
  k=0
  for c in w:
   if c.isdigit():k+=int(c)
   else:p.b[(7-i)*16+k]=('PNBRQK'.index(c.upper())+1)*(1 if c.isupper() else -1);k+=1
 p.s=2*(a[1]=='w')-1;p.c=sum(1<<i for i,x in enumerate('KQkq') if x in a[2])
 return p
def A(p,q,y):
 b=p.b;d=-16*y
 for o in(d-1,d+1):
  t=q+o
  if not t&0x88 and b[t]==y:return 1
 for o in KN+KI:
  t=q+o
  if not t&0x88 and b[t]in(y*2,y*6):return 1
 for o in BD+RD:
  t=q+o
  while not t&0x88:
   v=b[t]
   if v:
    if v==y*5 or v==y*(3+(o in RD)):return 1
    break
   t+=o
 return 0
def K(p,y):
 b=p.b
 return next((q for q in range(128)if not q&0x88 and b[q]==y*6),-1)
def G(p):
 b=p.b;s=p.s;V=[]
 for q in range(128):
  c=b[q]
  if q&0x88 or not c or(c>0)!=(s>0):continue
  t=abs(c)
  if t==1:
   d=16*s;o=q+d
   if not o&0x88 and not b[o]:
    r=o>>4
    V+=[q|(o<<7)|(z<<14)for z in(5,4,3,2)]if r in(0,7)else[q|(o<<7)]
    if(q>>4)==1+5*(s<0)and not b[q+2*d]:V.append(q|((q+2*d)<<7))
   for o in(d-1,d+1):
    w=q+o
    if w&0x88:continue
    v=b[w]
    if v and(v>0)!=(s>0):
     r=w>>4
     V+=[q|(w<<7)|(z<<14)for z in(5,4,3,2)]if r in(0,7)else[q|(w<<7)]
  elif t==2 or t==6:
   for o in[KN,KI][t==6]:
    w=q+o
    if w&0x88:continue
    v=b[w]
    if not v or(v>0)!=(s>0):V.append(q|(w<<7))
   if t==6:
    for z in CS:
     if p.c&z[0] and q==z[1]:
      kt=q+2*z[2]
      if not b[q+z[2]]and not b[kt]and(z[2]>0 or not b[q+3*z[2]])and all(not A(p,q+i*z[2],-s)for i in(0,1,2)):V.append(q|(kt<<7))
  else:
   for o in[BD,RD,BD+RD][t-3]:
    w=q+o
    while not w&0x88:
     v=b[w]
     if not v:V.append(q|(w<<7))
     else:
      if(v>0)!=(s>0):V.append(q|(w<<7))
      break
     w+=o
 return V
def M(p,m):
 b=p.b;f=m&127;t=(m>>7)&127;z=(m>>14)&7;c=b[f];v=b[t]
 u=(m,v,p.c);b[f]=0;b[t]=(z or abs(c))*(1 if p.s>0 else -1)
 if abs(c)==6 and abs(t-f)==2:
  for z in CS:
   if t==z[1]+2*z[2]:b[z[1]+z[2]]=b[z[3]];b[z[3]]=0
 p.c&=~(D.get(f,0)|D.get(t,0))
 p.s=-p.s
 if A(p,K(p,-p.s),p.s):U(p,u);return None
 return u
def U(p,u):
 m,v,c=u;b=p.b;f=m&127;t=(m>>7)&127;z=(m>>14)&7;d=b[t];p.s=-p.s
 b[f]=(1 if z else abs(d))*(1 if p.s>0 else -1);b[t]=v
 if abs(d)==6 and abs(t-f)==2:
  for z in CS:
   if t==z[1]+2*z[2]:b[z[3]]=b[z[1]+z[2]];b[z[1]+z[2]]=0
 p.c=c
def E(p):
 b=p.b;g=0
 for q in range(128):
  if q&0x88:continue
  c=b[q]
  if not c:continue
  t=abs(c);i=56-(q>>4)*8+(q&7);i^=56*(c<0)
  g+=(W[t]+((i>>3)*8 if t==1 else 0))*c//t
 return g if p.s>0 else -g
N=[0,0]
def Q(p,a,b,y):
 N[0]+=1
 if N[0]>N[1]:return E(p)
 c=A(p,K(p,p.s),-p.s);s=-MA if c else E(p)
 if not c:
  if s>=b:return b
  if s>a:a=s
 for m in G(p):
  if not c and not p.b[m>>7&127]:continue
  u=M(p,m)
  if u is None:continue
  v=-Q(p,-b,-a,y+1);U(p,u)
  if v>=b:return b
  if v>a:a=v
  if v>s:s=v
 return a if not c else(s if s>-MA else-MA+y)
def S(p,d,a,b,y):
 N[0]+=1
 if N[0]>N[1]:return E(p)
 if d<=0:return Q(p,a,b,y)
 c=A(p,K(p,p.s),-p.s)
 V=G(p);be=-MA;l=0
 for m in V:
  u=M(p,m)
  if u is None:continue
  l+=1
  s=-S(p,d-1,-b,-a,y+1);U(p,u)
  be=max(be,s);a=max(a,s)
  if a>=b:break
 if not l:return-MA+y if c else 0
 return be
def R(p,n):
 N[0]=0;N[1]=n;V=G(p);be=V[0];d=1
 while N[0]<n:
  a=-MA;cb=be
  for m in V:
   u=M(p,m)
   if u is None:continue
   s=-S(p,d-1,-MA,-a,1);U(p,u)
   if s>a:a,cb=s,m
  be=cb;d+=1
 return be
def Y(m):
 f=m&127;t=(m>>7)&127;z=(m>>14)&7
 return chr(97+f%8)+chr(49+f//16)+chr(97+t%8)+chr(49+t//16)+('nbrq'[z-2]if z else'')
def J(q,w):
 try:return j.load(u.urlopen(H+q,j.dumps(w).encode()))
 except:return j.load(u.urlopen(H+"/game",b'{}'))
def main():
 while(g:=J("/game",{}))["status"]!="done":
  while g["status"]=="your_move":g=J("/move",{"game":g["game"],"move":Y(R(F(g["fen"]),99999))})
main()
