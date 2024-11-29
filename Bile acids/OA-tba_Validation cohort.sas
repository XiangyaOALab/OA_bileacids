
libname out "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation";
libname ss "F:\0 project\steps\steps data\SS data\dataset";



proc import
datafile="F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation\metadata_clean cntl.xlsx"
out=case_control_final3
dbms=xlsx
replace;
run;


/*Baseline characteristics*/

proc freq data=case_control_final3;tables skoa*sex/chisq;run;

proc means data=case_control_final3;var age bmi;class skoa;run;
proc npar1way data=case_control_final3 wilcoxon;var age bmi;class skoa;run;



/*prepare TBA data*/
proc sql;
create table data2 as 
select * from ss.tba1214
where id in (select pe_id from case_control_final3);
quit;


data data3;set data2;
id_n=_n_;
run;

data id ;set data3;
keep id  serum_date id_n;
run;

data data4;set data3;
drop id  serum_date ;
run;


proc transpose data=data4
               out=data5;
			   id id_n;
run;


data data5;set data5;
drop _label_;
run;


data data5;
set data5;
tba_name=_n_;
run;


data tba_name;set data5;
keep tba_name _name_;
run;


data data6;set  data5;
drop tba_name _name_;
run;


proc transpose data=data6
               out=data7;
run;


proc transpose data=data7
               out=data8;
run;


data data9;
set data8;
array _{154};
do i=1 to 154;
    if _{i} ne . then count2=sum(count2,1);
  end;
if count2=. then count2=0;
else count2=count2;
run;

proc freq;tables count2;run;


data data9;
set data9;
if count2<15 then delete;
run;


proc transpose data=data9
               out=data10;
run;


data data10;
set data10;
id=substr(_name_, 2, 8);
id2=input(id,8.);
run;


proc sql;
create table data11 as
select a.id, b.* from id a inner join data10 b
on a.id_n=b.id2;
quit;


data data11;set data11;
drop _NAME_ id2;
run;


proc sql;
create table data12 as
select a.*, b.* from data11 a inner join Case_control_final3 b
on a.id=b.pe_id;
quit;


data sp;set data12;run;



%let maxnum=60;
%macro resample;
%do i=1 %to &maxnum;
data dataset&i;
set sp;
keep
id
skoa
result_date
skoa_date
questionnaire_date
age
sex
bmi
smoking 
drinking
cluster
unibi
klcat
ost
jsn
vas_cat
col&i 
tba_name;
tba_name=&i;
run;
data dataset&i;
set dataset&i;
rename col&i=tba;
run;
%end;
%mend;
%resample;


data total_data;
set dataset1-dataset60;
run;
proc freq;tables skoa;run;

data total_data2;
set total_data;
if tba<0 then delete;
run;


proc sql;
create table total_data3 
as select a.*, b._name_ as tbaname from total_data2 a left join tba_name b
on a.tba_name=b.tba_name;
quit;



proc univariate data=total_data3;var tba;run;



data total_data4;
set total_data3;

if bmi<25 then bmi25=0;
if bmi>=25 then bmi25=1;

if bmi<24 then bmi24=0;
if bmi>=24 then bmi24=1;

if bmi<28 then bmi28=0;
if bmi>=28 then bmi28=1;

if bmi<30 then bmi30=0;
if bmi>=30 then bmi30=1;

if bmi<24 then bmicat24=0;
if 24<=bmi<28 then bmicat24=1;
if 28<=bmi then bmicat24=2;

if bmi<25 then bmicat25=0;
if 25<=bmi<30 then bmicat25=1;
if 30<=bmi then bmicat25=2;
run;


data total_data4;set total_data4;
logtba=log10(tba);
run;

/*scale data*/
proc sort data=total_data4;by tbaname;run;
proc standard data=total_data4 out=z mean=0 std=1;
var tba;by tbaname;run;


/*OA vs. Control*/
proc sort data=z; by tbaname; run;
proc logistic data=z desc;
class skoa(ref='0') /param=ref;
model skoa(event='1')=tba  bmicat24; /*age sex bmi*/
by tbaname;
ods output parameterestimates=pred;
strata cluster;
run;


data pred2;set pred;
if variable="tba";
rename probchisq=raw_p;
run;

proc multtest inpvalues=pred2 fdr OUT=fdrtest;
run;

proc sort data=fdrtest; by raw_p;run;


proc means data=total_data4;var tba;class skoa;by tbaname;run;
proc univariate data=total_data4;var tba;class skoa;by tbaname;run;


filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation\TBA Results-case clean cntl.xlsx";

proc export data=fdrtest              
outfile=test
dbms=xlsx replace;
sheet="age1sexseed1000-bmicat24";
run;



/*Laterality*/
proc sort data=z; by tbaname; run;
proc genmod data=z desc;
class unibi;
model unibi=tba  bmicat24/link=cumlogit dist=multinomial;
by tbaname;
ods output parameterestimates=pred;
run;


data pred2;set pred;
if parameter="tba";
rename probchisq=raw_p;
run;

proc multtest inpvalues=pred2 fdr OUT=fdrtest;
run;

proc sort data=fdrtest; by raw_p;run;



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation\TBA Results-case clean cntl.xlsx";

proc export data=fdrtest              
outfile=test
dbms=xlsx replace;
sheet="unibi";
run;


/*KL grade*/

proc sort data=z; by tbaname; run;
proc genmod data=z desc;
class klcat;
model klcat=tba  bmicat24/link=cumlogit dist=multinomial;
by tbaname;
ods output parameterestimates=pred;
run;


data pred2;set pred;
if parameter="tba";
rename probchisq=raw_p;
run;

proc multtest inpvalues=pred2 fdr OUT=fdrtest;
run;

proc sort data=fdrtest; by raw_p;run;



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation\TBA Results-case clean cntl.xlsx";

proc export data=fdrtest              
outfile=test
dbms=xlsx replace;
sheet="klcat";
run;




/*Pain severity*/

proc freq data=z;tables skoa*vas_cat;run;


data z2;set z;
if skoa=0 and vas_cat in (1,2) then delete;
if skoa=1 and vas_cat=0 then delete;
run;
proc freq data=z2;tables  skoa*vas_cat;run;



proc sort data=z2; by tbaname; run;
proc genmod data=z2 desc;
class vas_cat;
model vas_cat=tba  bmicat24/link=cumlogit dist=multinomial;
by tbaname;
ods output parameterestimates=pred;
run;


data pred2;set pred;
if parameter="tba";
rename probchisq=raw_p;
run;

proc multtest inpvalues=pred2 fdr OUT=fdrtest;
run;

proc sort data=fdrtest; by raw_p;run;



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation\TBA Results-case clean cntl.xlsx";

proc export data=fdrtest              
outfile=test
dbms=xlsx replace;
sheet="vas";
run;


/*ost*/

proc freq data=z;tables skoa*ost;run;

proc sort data=z; by tbaname; run;
proc genmod data=z desc;
class ost;
model ost=tba bmicat24/link=cumlogit dist=multinomial;
by tbaname;
ods output parameterestimates=pred;
run;


data pred2;set pred;
if parameter="tba";
rename probchisq=raw_p;
run;

proc multtest inpvalues=pred2 fdr OUT=fdrtest;
run;

proc sort data=fdrtest; by raw_p;run;



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation\TBA Results-case clean cntl.xlsx";

proc export data=fdrtest              
outfile=test
dbms=xlsx replace;
sheet="ost";
run;


/*jsn*/

proc freq data=z;tables skoa*(jsn jsn2);run;

proc sort data=z; by tbaname; run;
proc genmod data=z desc;
class jsn;
model jsn=tba  bmicat24/link=cumlogit dist=multinomial;
by tbaname;
ods output parameterestimates=pred;
run;


data pred2;set pred;
if parameter="tba";
rename probchisq=raw_p;
run;

proc multtest inpvalues=pred2 fdr OUT=fdrtest;
run;

proc sort data=fdrtest; by raw_p;run;



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\TBA validation\TBA Results-case clean cntl.xlsx";

proc export data=fdrtest              
outfile=test
dbms=xlsx replace;
sheet="jsn";
run;
