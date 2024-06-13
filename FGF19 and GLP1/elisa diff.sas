

/*import data*/

proc import
datafile="F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation\FGF sample\40v40 elisa2.xlsx"
out=data1
dbms=xlsx replace;
run;

proc means data=data1;var fgf glp1;class group;run;
proc ttest data=data1;var fgf glp1;class group;run;
proc npar1way data=data1 wilcoxon;var fgf glp1;class group;run;


data data1;set data1;
if bmi<24 then bmi24=1;
else if bmi>=24 then bmi24=2;
run;


/*scale data*/
proc standard data=data1 mean=0 std=1 out=z;
var fgf glp1;run;


proc logistic data=z desc;
model group=fgf ;
strata cluster;
run;

proc logistic data=z desc;
model group=fgf bmi24;
strata cluster;
run;


proc logistic data=z desc;
model group=glp1 ;
strata cluster;
run;

proc logistic data=z desc;
model group=glp1  bmi24;
strata cluster;
run;
