data nile;
      input y @@;
      datalines;
   1120  1160  963  1210  1160  1160  813  1230   1370  1140
   995   935   1110 994   1020  960   1180 799    958   1140
   1100  1210  1150 1250  1260  1220  1030 1100   774   840
   874   694   940  833   701   916   692  1020   1050  969
   831   726   456  824   702   1120  1100 832    764   821
   768   845   864  862   698   845   744  796    1040  759
   781   865   845  944   984   897   822  1010   771   676
   649   846   812  742   801   1040  860  874    848   890
   744   749   838  1050  918   986   797  923    975   815
   1020  906   901  1170  912   746   919  718    714   740
   ;
run;
proc iml;
reset log;
use nile;read all;
/*y(t)  =alpha(t)+eps(t)*/
/*alpha(t+1)=alpha(t)+mu(t)*/

start kalman(var) global(y);
V=j(nrow(Y),1,0);/*Forecast error*/
F=j(nrow(Y),1,0);/*variance Forecast error*/
K=j(nrow(Y),1,0);/*Kalman gain*/
A=j(nrow(Y)+1,1,0);/*One step ahead forecast*/
P=j(nrow(Y)+1,1,10e7);/*Variance of the filtered*/
Do t=1 to nrow(Y);
v[t]=y[t]-a[t];/*erreur*/
f[t]=p[t]+exp(var[1]);/*var(eps)*/
k[t]=p[t]/f[t];
a[t+1]=a[t]+k[t]*v[t];
p[t+1]=p[t]*(1-k[t])+exp(var[2]); /*var(mu)*/
end;
t1=0;
do i=2 to nrow(Y);
t1=t1+log(f[i])+v[i]*v[i]/f[i];
end;
ld1=-(nrow(Y)/2)*log(8*atan(1))-0.5*t1;
return(Ld1);
finish kalman;
parm=1||1;/*Initialisation*/
opt={1 2 . 4};

call NLPQN(rc, Ldv, "kalman", parm, opt);

/*Filtering*/
var=ldv;
V=j(nrow(Y),1,0);/*Forecast error*/
F=j(nrow(Y),1,0);/*variance Forecast error*/
K=j(nrow(Y),1,0);/*Kalman gain*/
A=j(nrow(Y)+1,1,0);/*One step ahead forecast*/
P=j(nrow(Y)+1,1,10e7);/*Variance of the filtered*/
L=j(nrow(Y),1,0);

Do t=1 to nrow(Y);
v[t]=y[t]-a[t];/*erreur*/
f[t]=p[t]+exp(var[1]);/*var(eps)*/
k[t]=p[t]/F[t];
L[t]=1-k[t];
a[t+1]=a[t]+k[t]*v[t];
p[t+1]=p[t]*(1-k[t])+exp(var[2]); /*var(mu)*/
end;

/*Smoothing*/
	r=j(nrow(Y),1,0);
	alpha=j(nrow(Y),1,0);
	do t=-nrow(Y) to -2;
	t1=abs(t);
	r[t1-1]=inv(F[t1])*v[t1]+L[t1]*r[t1];
	alpha[t1]=a[t1]+P[t1]*r[t1-1];
	end;
	rr=inv(F[1])*v[1]+L[1]*r[1];
	alpha[1]=a[1]+P[1]*rr;
	resu=(1:nrow(Y)-1)`||a[2:nrow(a)-1]||alpha[2:nrow(Y)]||y[2:nrow(Y)];

print resu;
