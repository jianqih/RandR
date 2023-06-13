%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This matlab file creates Tables 10.1, 10.2, and 10.3. 
%%% It does the calculations reported in Section 10.8
%%% and generates the bootstrap draws for Figure 10.1 
%%% (which are drawn in R in figure10_1.R)
%%%
%%% Uses data file cps09mar.xlsx
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

B = 10000;

%   Load the data and create subsamples
dat = xlsread('cps09mar.xlsx');
experience = dat(:,1)-dat(:,4)-6;
mbf = (dat(:,11)==2)&(dat(:,12)<=2)&(dat(:,2)==1);
    
mbf12 = mbf&(experience==12);
    
dat1 = dat(mbf12,:);
dat2 = dat(mbf,:);

%   First regression
w = dat1(:,5)./(dat1(:,6).*dat1(:,7));
y = log(w);
edu = dat1(:,4);

x = [edu,ones(length(dat1),1)];
[n,k] = size(x);
xx = inv(x'*x);
beta = xx*(x'*y);
e = y - x*beta;
sig2 = (e'*e)/n;
betat = [beta;sig2];
g1 = [16;1];
g = [g1;1/2];
mu = exp(g1'*beta+sig2/2);
theta = [beta;sig2;mu];

%  Asymptotic s.e.
leverage = sum((x.*(x*inv(x'*x)))')';
u = x.*((e./sqrt(1-leverage))*ones(1,k));
v = xx*(u'*u)*xx;
s = sqrt(diag(v));
x0 = zeros(k,1);
xxt = [xx,x0;x0',1/n];
ut = [x.*(e*ones(1,k)),(e.^2)-sig2];
v = xxt*(ut'*ut)*xxt;
st = mu*sqrt(g'*v*g);
e2 = e.^2;
sv = sqrt(var(e2)/n);
stheta = [s',sv,st];

% Jackknife
jack = zeros(n,4);
for i = (1:n);
  yi = y;
  yi(i,:) = [];
  xi = x;
  xi(i,:) = [];
  betai = inv(xi'*xi)*(xi'*yi);
  ei = yi-xi*betai;
  sigi = (ei'*ei)/(n-1);
  mui = exp(g1'*betai+sigi/2);
  jack(i,:) = [betai',sigi,mui];
end
sn = ((n-1)^2)/n;
seJ = sqrt(sn*var(jack));

display('Table 10.1');
display('Jackknifed estimates');
display(jack);
display('Jackknife and asymptotic s.e.');
display([seJ;stheta]);

% Bootstrap
rng(13);
u1 = randi([1 n],n,1);
  yb = y(u1);
  xb = x(u1,:);
  betahatb = inv(xb'*xb)*(xb'*yb);
  eb = yb - xb*betahatb;
  sigb = (eb'*eb)/n;
  mub = exp(g1'*betahatb+sigb/2);
  theta1 = [betahatb;sigb;mub];
u2 = randi([1 n],n,1);
  yb = y(u2);
  xb = x(u2,:);
  betahatb = inv(xb'*xb)*(xb'*yb);
  eb = yb - xb*betahatb;
  sigb = (eb'*eb)/n;
  mub = exp(g1'*betahatb+sigb/2);
  theta2 = [betahatb;sigb;mub];

display('Calculations in Section 10.8');
display('First two bootstrap sample indices');
display([u1,u2]);
display('First two bootstrap estimate vectors');
display([theta1,theta2]);

thetas = zeros(B,4);
tstats = zeros(B,4);
rng(13);
for bi = (1:B)
  u = randi([1 n],n,1);
  yb = y(u);
  xb = x(u,:);
  xx = inv(xb'*xb);
  betahatb = xx*(xb'*yb);
  thetas(bi,1:2) = betahatb';
  eb = yb - xb*betahatb;
  sigb = (eb'*eb)/n;
  mub = exp(g1'*betahatb+sigb/2); 
  thetas(bi,3) = sigb;
  thetas(bi,4) = mub;

  leverage = sum((xb.*(xb*inv(xb'*xb)))')';
  u = xb.*((eb./sqrt(1-leverage))*ones(1,k));
  v = xx*(u'*u)*xx;
  s = sqrt(diag(v));
  x0 = zeros(k,1);
  xxt = [xx,x0;x0',1/n];
  ut = [xb.*(eb*ones(1,k)),(eb.^2)-sigb];
  v = xxt*(ut'*ut)*xxt;
  st = mub*sqrt(g'*v*g);
  e2 = eb.^2;
  sv = sqrt(var(e2)/n);
  sthetab = [s',sv,st];
  tstats(bi,:) = (thetas(bi,:)-theta')./sthetab;
end

% Bootstrap Standard Errors 
seboot = sqrt(var(thetas));

% Bootstrap Percentile Interval
q1 = quantile(thetas,.025)';
q2 = quantile(thetas,.975)';

% Bootstrap BC Percentile Interval
z0 = norminv(mean(thetas <= theta'));
z1 = norminv(.025);
z2 = norminv(.975);
xa1 = normcdf(z1+2*z0);
xa2 = normcdf(z2+2*z0);
qa1 = diag(quantile(thetas,xa1));
qa2 = diag(quantile(thetas,xa2));

% Bootstrap BCa Percentile Interval
mj = mean(jack);
d = (sum((mj-jack).^2)).^(3/2);
a = sum((mj-jack).^3)./d/6;
xa1 = normcdf(z0+(z1+z0)./(1-a.*(z1+z0)));
xa2 = normcdf(z0+(z2+z0)./(1-a.*(z2+z0)));
qa1 = diag(quantile(thetas,xa1));
qa2 = diag(quantile(thetas,xa2));

% Bootstrap percentile-t
qt1 = theta - quantile(tstats,.975)'.*stheta';
qt2 = theta - quantile(tstats,.025)'.*stheta';

display('Table 10.2');
display('Estimates and s.e.');
display([theta';stheta;seJ;seboot]);
display('95% Percentile Confidence Intervals');
display([q1,q2]);
display('Bias-Corrected Percentile 95% Confidence Intervals');
display([qa1,qa2]);
display('BCa Percentile 95% Confidence Intervals');
display([qa1,qa2]);
display('Percentile-t 95% Confidence Intervals');
display([qt1,qt2]);

% For Figure 10.1, save bootstrap replications

betas1 = thetas(:,1);
theta4 = thetas(:,4);
b1 = beta(1);
bootreps = [b1,mu;betas1,theta4];
fileID = fopen('bootreps.txt','w'); 
fprintf(fileID,'%12.8f %12.8f\r\n',bootreps);
fclose(fileID);


%   Second regression (Section 10.17)
y = log(dat2(:,5)./(dat2(:,6).*dat2(:,7)));
exp1 = dat2(:,1)-dat2(:,4)-6;
exp2 = (exp1.^2)/100;
x = [dat2(:,4),exp1,exp2,ones(length(dat2),1)];
[n,k] = size(x);
beta = inv(x'*x)*(x'*y);
e = y - x*beta;
theta = -50*beta(2)/beta(3);

%  Asymptotic s.e.
leverage = sum((x.*(x*inv(x'*x)))')';
u = x.*((e./sqrt(1-leverage))*ones(1,k));
xx = inv(x'*x);
v = xx*(u'*u)*xx;
se = sqrt(diag(v));
g = [0;-50/beta(3);-theta/beta(3);0];
s = sqrt(g'*v*g);

% Jackknife
thetaJ = zeros(n,1);
for bi = (1:n)
  yb = y;
  xb = x;
  yb(bi,:)=[];
  xb(bi,:)=[];
  betahatb = inv(xb'*xb)*(xb'*yb);
  thetaJ(bi) = -50*betahatb(2)/betahatb(3);
end
sn = ((n-1)^2)/n;
sethetaJ = sqrt(sn*var(thetaJ));

% Bootstrap
thetas1 = zeros(B,1);
thetast = zeros(B,1);
tau = 25;
for bi = (1:B)
  u = randi([1 n],n,1);
  yb = y(u);
  xb = x(u,:);
  betahatb = inv(xb'*xb)*(xb'*yb);
  thetab = -50*betahatb(2)/betahatb(3);
  thetat = thetab*(abs(thetab-theta) <= tau) + (theta-tau)*(thetab < theta-tau) + (theta+tau)*(thetab > theta+tau);
  thetas1(bi) = thetab;
  thetast(bi) = thetat;
end
sethetaboot1 = sqrt(var(thetas1));
sethetaboott = sqrt(var(thetast));

% Repeat Bootstrap
thetas2 = zeros(B,1);
tau = 25;
for bi = (1:B)
  u = randi([1 n],n,1);
  yb = y(u);
  xb = x(u,:);
  betahatb = inv(xb'*xb)*(xb'*yb);
  thetab = -50*betahatb(2)/betahatb(3);
  thetas2(bi,1) = thetab;
end
sethetaboot2 = sqrt(var(thetas2));

format shortG;
display('Log(wage) equation');
display([beta,se]);

display('Tabled 10.3');
display('Experience with maximum wage');
display('Estimates and s.e.');
display([theta;s;sethetaJ;sethetaboot1;sethetaboot2;sethetaboott]);

