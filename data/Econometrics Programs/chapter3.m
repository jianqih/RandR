%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This file executes the 
%%% empirical work reported
%%% in Chapter 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diary('chapter3matlab.log');

%   Load the data and create subsamples
dat = load('cps09mar.txt');
experience = dat(:,1)-dat(:,4)-6;
mbf = (dat(:,11)==2)&(dat(:,12)<=2)&(dat(:,2)==1)&(experience==12);
sam = (dat(:,11)==4)&(dat(:,12)==7)&(dat(:,2)==0);
    
dat1 = dat(mbf,:);
dat2 = dat(sam,:);

%   First regression
y = log(dat1(:,5)./(dat1(:,6).*dat1(:,7)));
x = [dat1(:,4),ones(length(dat1),1)];
xx = x'*x;
xy = x'*y;
beta = xx\xy;
fprintf("\n Regression (3.12) \n");
display(beta);

%   Second regression
y = log(dat2(:,5)./(dat2(:,6).*dat2(:,7)));
experience = dat2(:,1)-dat2(:,4)-6;
exp2 = (experience.^2)/100;
x = [dat2(:,4),experience,exp2,ones(length(dat2),1)];
xx = x'*x;
xy = x'*y;
beta = xx\xy;
fprintf("\n Regression (3.13) \n");
display(beta);

%	Create leverage and influence
e = y-x*beta;
xxi = inv(xx);
leverage = sum((x.*(x*xxi)),2);
d = leverage.*e./(1-leverage);
influence = max(abs(d));
fprintf("\n The influence of log wage regression (3.13) \n");
display(influence);

%  Regression with the restricted sample
index45 = experience < 45;
x_r = x(index45,:);
y_r = y(index45);
beta_r = (x_r'*x_r)\(x_r'*y_r);
fprintf("\n Regression (3.44) \n");
display(beta_r);

diary off;