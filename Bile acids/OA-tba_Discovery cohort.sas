
libname out "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA";
libname dataset "F:\0 project\XO\dataset";



/*import metadata*/

proc import
datafile="F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\metadata.xlsx"
out=metadata2
dbms=xlsx replace;
run;


/*Baseline characteristics*/

proc freq data=metadata2;tables skoa*(sex drink_cat)/chisq;run;

proc means data=metadata2;var age bmi meat_eggs_fq3 dairy_fq3 veg_fq3 ;class skoa;run;
proc npar1way data=metadata2 wilcoxon;var age bmi meat_eggs_fq3 dairy_fq3 veg_fq3 ;class skoa;run;


/*prepare TBA data*/

data data;set dataset.Xb1_tba_filtered_ratio dataset.Xc1_tba_filtered_ratio;run;

proc sql;
create table data2 as 
select * from data
where id in (select id from metadata2);
quit;


data data3;set data2;
id_n=_n_;
run;

data id ;set data3;
keep id id_n;
run;


proc transpose data=data3
               out=data4;
			   id id_n;
run;


data data4;set data4;
drop _label_;
if _n_=1  then delete;
run;


data data4;
set data4;
tba_name=_n_;
run;


data tba_name;set data4;
keep tba_name _name_;
run;


data data5;set  data4;
drop tba_name _name_;
run;


proc transpose data=data5
               out=data6;
run;



proc transpose data=data6
               out=data7;
run;


data data8;
set data7;
array _{1714};
do i=1 to 1714;
    if _{i} ne . then count2=sum(count2,1);
  end;
if count2=. then count2=0;
else count2=count2;
run;

proc freq;tables count2;run;


data data8;
set data8;
if count2<172 then delete;
run;


proc transpose data=data8
               out=data9;
run;


data data9;
set data9;
id=substr(_name_, 2, 8);
id2=input(id,8.);
run;


proc sql;
create table data10 as
select a.id, b.* from id a inner join data9 b
on a.id_n=b.id2;
quit;


data data10;set data10;
drop _NAME_ id2;
run;


proc sql;
create table data11 as
select a.*, b.* from data10 a inner join metadata2 b
on a.id=b.id;
quit;


data skoa;set data11;run;
proc freq data=skoa;tables skoa;run;



%let maxnum=57;
%macro resample;
%do i=1 %to &maxnum;
data dataset&i;
set skoa;
keep
id age sex bmi
meat_eggs_fq2 dairy_fq2 veg_fq2 meat_eggs_fq3 dairy_fq3 veg_fq3
ra1 drink_cat smoke_cat
skoa klgrade unibi OST JSN
WOPNKL WOPNKR vas_l vas_r womacl_cat womacr_cat womac_cat vas_catl vas_catr vas_cat
esr crp
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

proc contents data=data1; run;

data total_data;
set dataset1-dataset57;
run;
proc freq data=total_data;tables skoa;run;

data total_data2;
set total_data;
if tba<0 then delete;
run;

proc sql;
create table total_data3 
as select a.*, b._name_ as tbaname from total_data2 a left join tba_name b
on a.tba_name=b.tba_name;
quit;


proc sort data=total_data3; by tbaname; run;
proc means;var tba;by tbaname;class group; run;


data total_data3;set total_data3;
if skoa=3 then group=1;
else if skoa=0 then group=0;run;
proc freq;tables group;run;


data total_data3;set total_data3;
if age<60 then agecat1=1;
else if 60<=age<70 then agecat1=2;
else if age>=70 then agecat1=3;

if age<65 then agecat2=1;
else if age>=65 then agecat2=2;

if 0<bmi<25 then bmi25=1;
else if bmi>=25 then bmi25=2;

if bmi<25 then bmi25_2=1;
else if bmi>=25 then bmi25_2=2;

if 0<bmi<24 then bmi24=1;
else if bmi>=24 then bmi24=2;

if bmi<24 then bmi24_2=1;
else if bmi>=24 then bmi24_2=2;
run;


/*scale tba data*/
proc sort data=total_data3;by tbaname;run;
proc standard data=total_data3 out=z mean=0 std=1;
var tba;by tbaname;run;


/*OA vs. Control*/
proc sort data=z; by tbaname; run;
proc logistic data=z desc;
class group(ref='0') /param=ref;
model group(event='1')=age sex bmi24_2 drink_cat meat_eggs_fq2 dairy_fq2 veg_fq2 tba ;
by tbaname;
ods output parameterestimates=pred;
run;


data pred2;set pred;
if variable="tba";
rename probchisq=raw_p;
run;

/*fdr test*/
proc multtest inpvalues=pred2 fdr OUT=fdrtest;
run;

proc sort data=fdrtest; by raw_p;run;


filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\Results-SKOA and TBA-ratio.xlsx";

proc export data=fdrtest
outfile=test
dbms=xlsx replace;
sheet="sheet1";
run;


proc means data=total_data3;var tba;by tbaname;class group; run;



/*Indicators of OA*/

/*Laterality*/
proc sort data=z; by tbaname; run;
proc genmod data=z desc;
class unibi;
model unibi=age sex bmi24_2 drink_cat meat_eggs_fq2 dairy_fq2 veg_fq2 tba/link=cumlogit dist=multinomial;
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



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\Results-SKOA and TBA-ratio.xlsx";

proc export data=fdrtest
outfile=test
dbms=xlsx replace;
sheet="unibi";
run;


/*kl grade*/

data z;set z;
if group=0 then klcat=0;
else if group=1 and klgrade=2 then klcat=1;
else if group=1 and klgrade>2 then klcat=2;
run;

proc freq;tables group klcat;run;


proc sort data=z; by tbaname; run;
proc genmod data=z desc;
class klcat;
model klcat=age sex bmi24_2 drink_cat meat_eggs_fq2 dairy_fq2 veg_fq2 tba/link=cumlogit dist=multinomial;
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



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\Results-SKOA and TBA-ratio.xlsx";

proc export data=fdrtest
outfile=test
dbms=xlsx replace;
sheet="klcat";
run;




/*Pain severity */

proc freq data=z;tables group*vas_cat;run;


data z2;set z;
if group=0 and vas_cat in (1,2) then delete;
if group=1 and vas_cat=0 then delete;
run;
proc freq data=z2;tables  group*vas_cat;run;


proc sort data=z2; by tbaname; run;
proc genmod data=z2 desc;
class vas_cat;
model vas_cat=age sex bmi24_2 drink_cat meat_eggs_fq2 dairy_fq2 veg_fq2 tba/link=cumlogit dist=multinomial;
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



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\Results-SKOA and TBA-ratio.xlsx";

proc export data=fdrtest
outfile=test
dbms=xlsx replace;
sheet="vas";
run;




/*OST*/

proc freq data=z;tables group*OST;run;


proc sort data=z; by tbaname; run;
proc genmod data=z desc;
class OST;
model OST=age sex bmi24_2 drink_cat meat_eggs_fq2 dairy_fq2 veg_fq2 tba/link=cumlogit dist=multinomial;
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



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\Results-SKOA and TBA-ratio.xlsx";

proc export data=fdrtest
outfile=test
dbms=xlsx replace;
sheet="OST";
run;



/*JSN*/

proc freq data=z;tables group*JSN;run;


proc sort data=z; by tbaname; run;
proc genmod data=z desc;
class JSN;
model JSN=age sex bmi24_2 drink_cat meat_eggs_fq2 dairy_fq2 veg_fq2 tba/link=cumlogit dist=multinomial;
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



filename test "F:\0 project\XO\7 Metabolomics\1 bile acid\1 SKOA and TBA\Results-SKOA and TBA-ratio.xlsx";

proc export data=fdrtest
outfile=test
dbms=xlsx replace;
sheet="JSN";
run;



