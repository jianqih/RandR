%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This file calculates the condition 
%%% numbers reported
%%% in Chapter 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dat=load('cps09mar.txt');
sam = (dat(:,11)==4)&(dat(:,12)==7)&(dat(:,2)==0);  
dat = dat(sam,:);
y = log(dat(:,5)./(dat(:,6).*dat(:,7)));
edu = dat(:,4);
q = dat(:,1)-edu-6;
k = 30;
n = length(dat);

x1 = [ones(n,1),q.^(1:k)];
x2 = [ones(n,1),(q/max(q)).^(1:k)];
x3 = [x1./mean(x1)];
x4 = [ones(n,1),(2*q/max(q)-1).^(1:k)];

qq = ones(n,1);
for j = 1:k
  qj = (q.^j);
  bj = (qq'*qq)\(qq'*qj);
  qj = qj - qq*bj;
  qj = qj/sqrt(qj'*qj/n);
  qq = [qq,qj];
end
x5 = qq;

r = zeros(k,6);

for j = 1:k

  x1j = x1(:,1:(1+j));
  x2j = x2(:,1:(1+j));
  x3j = x3(:,1:(1+j));
  x4j = x4(:,1:(1+j));
  x5j = x5(:,1:(1+j));
  r(j,1) = j;
  r(j,2) = rcond(x1j'*x1j);
  r(j,3) = rcond(x2j'*x2j);
  r(j,4) = rcond(x3j'*x3j);
  r(j,5) = rcond(x4j'*x4j);
  r(j,6) = rcond(x5j'*x5j);
end

format shortE;
display(r);

