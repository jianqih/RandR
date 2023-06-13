%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This file executes the 
%%% empirical work reported
%%% in Chapter 4
%%%
%%% Uses data files cps09mar.txt, DDK2011.xlsx
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diary('chapter4matlab.log');

%   Load the data and create subsamples
dat = load('cps09mar.txt');
experience = dat(:,1)-dat(:,4)-6;
mbf = (dat(:,11)==2)&(dat(:,12)<=2)&(dat(:,2)==1)&(experience==12);
sam = (dat(:,11)==4)&(dat(:,12)==7)&(dat(:,2)==0);
    
dat1=dat(mbf,:);
dat2=dat(sam,:);

%   Table 4.1
y = log(dat1(:,5)./(dat1(:,6).*dat1(:,7)));
x = [dat1(:,4),ones(length(dat1),1)];
beta = (x'*x)\(x'*y);

e = y-x*beta;
leverage = sum((x.*(x*inv(x'*x))),2);

[n,k] = size(x);
a = n/(n-k);
sig2 = (e'*e)/(n-k);
u1 = x.*e;
u2 = x.*(e./sqrt(1-leverage));
u3 = x.*(e./(1-leverage));
xx = inv(x'*x);
v0 = xx*sig2;
v1 = xx*(u1'*u1)*xx;
v1a = a*xx*(u1'*u1)*xx;
v2 = xx*(u2'*u2)*xx;
v3 = xx*(u3'*u3)*xx;
s0 = sqrt(diag(v0)); % Homoskedastic formula
s1 = sqrt(diag(v1)); % White formula
s1a = sqrt(diag(v1a)); % HC1 formula
s2 = sqrt(diag(v2)); % HC2 formula
s3 = sqrt(diag(v3)); % HC3 formula

fprintf(" Table 4.1 (Regression (3.12)) \n");
fprintf(" Coefficient estimates \n");
display(beta');
fprintf(" Homoskedastic se \n");
display(s0');
fprintf(" White se \n");
display(s1');
fprintf(" HC1 se \n");
display(s1a');
fprintf(" HC2 se \n");
display(s2');
fprintf(" HC3 se \n");
display(s3');



% Equation 3.13
y = log(dat2(:,5)./(dat2(:,6).*dat2(:,7)));
experience = dat2(:,1)-dat2(:,4)-6;    
exp2 = (experience.^2)/100;
x = [dat2(:,4),experience,exp2,ones(length(dat2),1)];
beta = (x'*x)\(x'*y);

e = y-x*beta;
leverage = sum((x.*(x*inv(x'*x))),2);

[n,k] = size(x);
a = n/(n-k);
sig2 = (e'*e)/(n-k);
u1 = x.*e;
u2 = x.*(e./sqrt(1-leverage));
u3 = x.*(e./(1-leverage));
xx = inv(x'*x);
v0 = xx*sig2;
v1 = xx*(u1'*u1)*xx;
v1a = a*xx*(u1'*u1)*xx;
v2 = xx*(u2'*u2)*xx;
v3 = xx*(u3'*u3)*xx;
s0 = sqrt(diag(v0)); % Homoskedastic formula
s1 = sqrt(diag(v1)); % White formula
s1a = sqrt(diag(v1a)); % HC1 formula
s2 = sqrt(diag(v2)); % HC2 formula
s3 = sqrt(diag(v3)); % HC3 formula

fprintf(" \n Regression (3.13) \n");
fprintf(" Coefficient estimates \n");
display(beta');
fprintf(" Homoskedastic se \n");
display(s0');
fprintf(" White se \n");
display(s1');
fprintf(" HC1 se \n");
display(s1a');
fprintf(" HC2 se \n");
display(s2');
fprintf(" HC3 se \n");
display(s3');


% Table 4.2.
edu12 = (dat(:,4)>11);
dat3 = dat(edu12,:);
marriedF = (dat3(:,12)<=3)&(dat3(:,2)==1);
marriedM = (dat3(:,12)<=3)&(dat3(:,2)==0);
unionF = (dat3(:,8)==1)&(dat3(:,2)==1);
unionM = (dat3(:,8)==1)&(dat3(:,2)==0);
fmarriedF = (dat3(:,12)<=6)&(dat3(:,12)>3)&(dat3(:,2)==1);
fmarriedM = (dat3(:,12)<=6)&(dat3(:,12)>3)&(dat3(:,2)==0);
black = (dat3(:,11)==2);
american_indian = (dat3(:,11)==3);
asian = (dat3(:,11)==4);
mixed = (dat3(:,11)>=6);

y = log(dat3(:,5)./(dat3(:,6).*dat3(:,7)));
experience = dat3(:,1)-dat3(:,4)-6;    
exp2 = (experience.^2)/100;
x = [dat3(:,4),experience,exp2,dat3(:,2),...
     unionF,unionM,marriedF,marriedM,fmarriedF,fmarriedM,...
     dat3(:,3),black,american_indian,asian,mixed,ones(length(dat3),1)];
beta = (x'*x)\(x'*y);
e = y-x*beta;
leverage = sum((x.*(x*inv(x'*x))),2);
[n,k] = size(x);
u2 = x.*(e./sqrt(1-leverage));
xx = inv(x'*x);
v2 = xx*(u2'*u2)*xx;
s2 = sqrt(diag(v2)); % HC2 formula

fprintf("\n Table 4.2: Log Wage Regression with HC2 se\n");
fprintf("    Coef.    HC2 se\n");
display([beta,s2]);

% DDK (2011) 

% Load the data and create variables
data = xlsread('DDK2011.xlsx');
schoolid = data(:,2);
tracking = data(:,7);
totalscore = data(:,39);
y = (totalscore - mean(totalscore))./std(totalscore);
x = [tracking,ones(size(y,1),1)];
[n,k] = size(x);
xx = x'*x;
invx = inv(xx);
beta = xx\(x'*y);
e = y - x*beta;
% Clustered robust standard error
[schools,~,schoolidx] = unique(schoolid);
G = size(schools,1);
cluster_sums = zeros(G,k);
for (j = 1:k) 
    cluster_sums(:,j) = accumarray(schoolidx,x(:,j).*e);
end
omega = cluster_sums'*cluster_sums;
scale = G/(G-1)*(n-1)/(n-k);
V_clustered = scale*invx*omega*invx;
se_clustered = sqrt(diag(V_clustered));
fprintf("\n Regression (4.52) of test scores on the tracking dummy \n");
fprintf(" Coefficient estimates \n");
display(beta');
fprintf(" Clustered ste \n");
display(se_clustered');
diary off;
