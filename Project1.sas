options linesize=78;

libname Project1 "D:\Desktop\SAS Projects\Project1";

* Create temporary data set*;

data FTA1;
  set Project1.fta;
  run;

* Generate summary of the data*;

proc contents data=Project1.fta;
  title 'FTA Sample Data Contents';
  run;

proc means data=Project1.fta;
  title 'FTA Sample Data Statistics';
  run;


**********; 
* Part A *;
**********;


**************************************************************;
*Step 1 Create 6 files, 1980, 1984, 1986, 1988, 1994, 1996   *;
**************************************************************;

data d80 d84 d86 d88 d94 d96;
  set Project1.fta;

  ctar= TA - TROW; *Change in Canadian tariff due to NAFTA;
  utar= TCA - TROWA; *Change in U.S tariff due to FTA;

if year=1980 then output d80;
if year=1984 then output d84;
if year=1986 then output d86;
if year=1988 then output d88;
if year=1994 then output d94;
if year=1996 then output d96;

  keep SIC4 ctar utar L LA LN LNA LP LPA;
  run;


**************************************************************;
*Step 2 Merge 6 files, 1980, 1984, 1986, 1988, 1994, 1996    *;
**************************************************************;
* Alternative way -- Sorting by Macro *;
*%macro sorting (yr)*; 
*proc sort data=d&yr.*;
*by SIC4*;
*run*;
*%mend*;

*%sorting(80)*;
*%sorting(84)*;
*%sorting(86)*;
*%sorting(88)*;
*%sorting(94)*;
*%sorting(96)*; 

  proc sort data=d80;
    by SIC4;
	run;

  proc sort data=d84;
  	by SIC4;
	run;

  proc sort data=d86;
    by SIC4;
	run;
	
  proc sort data=d88;
    by SIC4;
	run;

  proc sort data=d94;
  	by SIC4;
	run;

  proc sort data=d96;
    by SIC4;
	run;

  data diff1;
    merge d80 (rename =(ctar=ctar80 utar=utar80 l=l80 la=la80 ln=ln80 lna=lna80 lp=lp80 lpa=lpa80 ))
          d84 (rename =(ctar=ctar84 utar=utar84 l=l84 la=la84 ln=ln84 lna=lna84 lp=lp84 lpa=lpa84 ))
		  d86 (rename =(ctar=ctar86 utar=utar86 l=l86 la=la86 ln=ln86 lna=lna86 lp=lp86 lpa=lpa86 ))
          d88 (rename =(ctar=ctar88 utar=utar88 l=l88 la=la88 ln=ln88 lna=lna88 lp=lp88 lpa=lpa88 )) 
          d94 (rename =(ctar=ctar94 utar=utar94 l=l94 la=la94 ln=ln94 lna=lna94 lp=lp94 lpa=lpa94 ))
		  d96 (rename =(ctar=ctar96 utar=utar96 l=l96 la=la96 ln=ln96 lna=lna96 lp=lp96 lpa=lpa96 ));
    by SIC4;
    run;


**************************************************;
*Step 3 Create Double Differences                *;
**************************************************;
*Time Periods:*;
  *1980-86 1988-96
  *1980-88 1988-96
  *1980-86 1988-94
  *1980-84 1988-94 *;

*Create measures *;
  data diff2;
    set diff1;

*Double Differencing:;

  * Canada Employment Variables;
  ddl=(log(l96)-log(l88))/(1996-1988)-(log(l86)-log(l80))/(1986-1980);*Canadian Total Employment;
  ddln=(log(ln96)-log(ln88))/(1996-1988)-(log(ln86)-log(ln80))/(1986-1980);*Canadian Non-Production Employment;
  ddlp=(log(lp96)-log(lp88))/(1996-1988)-(log(lp86)-log(lp80))/(1986-1980);*Canadian Production Employment;
  ddsk=(log(lp96/ln96)-log(lp88/ln88))/(1996-1988)-(log(lp86/ln86)-log(lp80/ln80))/(1986-1980);*Canadian Skill Upgrading;

  * U.S Employment Variables;
  ddla=(log(la96)-log(la88))/(1996-1988)-(log(la86)-log(la80))/(1986-1980);*U.S Total Employment;
  ddlna=(log(lna96)-log(lna88))/(1996-1988)-(log(lna86)-log(lna80))/(1986-1980);*U.S Non-Production Employment;
  ddlpa=(log(lpa96)-log(lpa88))/(1996-1988)-(log(lpa86)-log(lpa80))/(1986-1980);*U.S Production Employment;
  ddska=(log(lpa96/lna96)-log(lpa88/lna88))/(1996-1988)-(log(lpa86/lna86)-log(lpa80/lna80))/(1986-1980);*U.S Skill Upgrading;

  *Tariffs;
  ddctar=(ctar96-ctar88)/(1996-1988) - (ctar86-ctar80)/(1986-1980);*Change in Canadian tariff;
  ddutar=(utar96-utar88)/(1996-1988) - (utar86-utar80)/(1986-1980);*Change in U.S tariff;

  *set pre-FTA tariff changes to zero;
  ddctar1=(ctar96-ctar88)/(1996-1988);*Change in Canadian tariff;
  ddutar1=(utar96-utar88)/(1996-1988);*Change in U.S tariff;

  run;


  proc means data=diff2 N mean stddev sum min max;
    var ddl ddla ddln ddlna ddlp ddlpa ddctar ddsk ddska ddctar1 ddutar ddutar1;
	title 'Double-Differencing Statistical Output';
	run;

*Determine correlation between variables;
  proc corr data=diff2;
    var ddl ddla ddln ddlna ddlp ddlpa ddctar ddsk ddska ddctar1 ddutar ddutar1;
	title 'Correlation between Variables';
	run;
    

**************************************************;
*Step 4 Run Regressions                          *;
**************************************************;

  *Employment - All Workers;
    proc reg data=diff2;
      model ddl = ddctar1 ddutar1 ddla;
	  title 'Regression on Employment - All Workers';
	  run;

  *Employment - All Workers without US Tariff;
    proc reg data=diff2;
      model ddl = ddctar1 ddla;
	  title 'Regression on Employment - All Workers (without US Tariff)';
	  run;

  *Employment - Production Workers;
	proc reg data=diff2;
	  model ddlp = ddctar1 ddutar1 ddlpa;
	  title 'Regression on Employment - Production Workers';
	  run;

  *Employment - Production Workers without US Tariff;
	proc reg data=diff2;
	  model ddlp = ddctar1 ddlpa;
	  title 'Regression on Employment - Production Workers (without US Tariff)';
	  run;

  *Employment - Non-Production Workers;
	proc reg data=diff2;
	  model ddln = ddctar1 ddutar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers';
	  run;

  *Employment - Non-Production Workers without US Tariff;
	proc reg data=diff2;
	  model ddln = ddctar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers (without US Tariff)';
	  run;
	  
  *Employment - Skill Upgrading;
	proc reg data=diff2;
	  model ddsk = ddctar1 ddutar1 ddska;
	  title 'Regression on Employment - Skill Upgrading Workers';
	  run;

  *Employment - Skill Upgrading without US Tariff;
	proc reg data=diff2;
	  model ddsk = ddctar1 ddska;
	  title 'Regression on Employment - Skill Upgrading (without US Tariff)';
	  run;



**********;
* Part B *;
**********;
**********Modification I - Change from double-differencing to single-differencing**********;

data singlediff_pre;
    set diff1;

*Single Differencing:;
  * Canada Employment Variables;
  sdl=(log(l86)-log(l80))/(1986-1980);*Canadian Total Employment pre-FTA;
  sdln=(log(ln86)-log(ln80))/(1986-1980);*Canadian Non-Production Employment pre-FTA;
  sdlp=(log(lp86)-log(lp80))/(1986-1980);*Canadian Production Employment pre-FTA;
  sdsk=(log(lp86/ln86)-log(lp80/ln80))/(1986-1980);*Canadian Skill Upgrading pre-FTA;

  * U.S Employment Variables;
  sdla=(log(la86)-log(la80))/(1986-1980);*U.S Total Employment pre-FTA;
  sdlna=(log(lna86)-log(lna80))/(1986-1980);*U.S Non-Production Employment pre-FTA;
  sdlpa=(log(lpa86)-log(lpa80))/(1986-1980);*U.S Production Employment pre-FTA;
  sdska=(log(lpa86/lna86)-log(lpa80/lna80))/(1986-1980);*U.S Skill Upgrading pre-FTA;

  *Tariffs;
    sdctar=(ctar86-ctar80)/(1986-1980);*Change in Canadian tariff pre-FTA;
    sdutar=(utar86-utar80)/(1986-1980);*Change in U.S tariff pre-FTA;

	sdctar1=0;
    sdutar1=0;

	keep sdl sdla sdln sdlna sdlp sdlpa sdsk sdska sdctar sdutar;

    run;

data singlediff_aft;
    set diff1;

  * Canada Employment Variables;
  sdl=(log(l96)-log(l88))/(1996-1988);*Canadian Total Employment after-FTA;
  sdln=(log(ln96)-log(ln88))/(1996-1988);*Canadian Non-Production Employment after-FTA;
  sdlp=(log(lp96)-log(lp88))/(1996-1988);*Canadian Production Employment after-FTA;
  sdsk=(log(lp96/ln96)-log(lp88/ln88))/(1996-1988);*Canadian Skill Upgrading after-FTA;

  * U.S Employment Variables;
  sdla=(log(la96)-log(la88))/(1996-1988);*U.S Total Employment after-FTA;
  sdlna=(log(lna96)-log(lna88))/(1996-1988);*U.S Non-Production Employment after-FTA;
  sdlpa=(log(lpa96)-log(lpa88))/(1996-1988);*U.S Production Employment after-FTA;
  sdska=(log(lpa96/lna96)-log(lpa88/lna88))/(1996-1988);*U.S Skill Upgrading after-FTA;

  *Tariffs;
    sdctar=(ctar96-ctar88)/(1996-1988);*Change in Canadian tariff after-FTA;
    sdutar=(utar96-utar88)/(1996-1988);*Change in U.S tariff after-FTA;

	sdctar1=0;
    sdutar1=0;
    
   	keep sdl sdla sdln sdlna sdlp sdlpa sdsk sdska sdctar sdutar;

    run;

	*Merge two set of data;
	data singlediff;
	  merge singlediff_pre singlediff_aft;
	  run;

  *Run Regression for single-differencing model;
  
	*Employment - All Workers;
	proc reg data=singlediff;
	  model sdl = sdctar sdutar sdla;
      title 'Regression on Employment - All Workers (single-differencing)';
	  run;

	*Employment - Production Workers;
	proc reg data=singlediff;
	  model sdlp = sdctar sdutar sdlpa;
	  title 'Regression on Employment - Production Workers (single-differencing)';
	  run;

    *Employment - Non-Production Workers;
	proc reg data=singlediff;
	  model sdln = sdctar sdutar sdlna;
	  title 'Regression on Employment - Non-Production Workers (single-differencing)';
	  run;

	*Employment - Skill Upgrading;
	proc reg data=singlediff;
	  model sdsk = sdctar sdutar sdska;
	  title 'Regression on Employment - Skill Upgrading (single-differencing)';
	  run;

**********Modification II - i. Time periods (1996-1988)-(1988-1980)**********;
*Create measures *;
  data diff3;
    set diff1;

*Double Differencing:;

  * Canada Employment Variables;
  ddl=(log(l96)-log(l88))/(1996-1988)-(log(l88)-log(l80))/(1988-1980);*Canadian Total Employment;
  ddln=(log(ln96)-log(ln88))/(1996-1988)-(log(ln88)-log(ln80))/(1988-1980);*Canadian Non-Production Employment;
  ddlp=(log(lp96)-log(lp88))/(1996-1988)-(log(lp88)-log(lp80))/(1988-1980);*Canadian Production Employment;
  ddsk=(log(lp96/ln96)-log(lp88/ln88))/(1996-1988)-(log(lp88/ln88)-log(lp80/ln80))/(1988-1980);*Canadian Skill Upgrading;

  * U.S Employment Variables;
  ddla=(log(la96)-log(la88))/(1996-1988)-(log(la88)-log(la80))/(1988-1980);*U.S Total Employment;
  ddlna=(log(lna96)-log(lna88))/(1996-1988)-(log(lna88)-log(lna80))/(1988-1980);*U.S Non-Production Employment;
  ddlpa=(log(lpa96)-log(lpa88))/(1996-1988)-(log(lpa88)-log(lpa80))/(1988-1980);*U.S Production Employment;
  ddska=(log(lpa96/lna96)-log(lpa88/lna88))/(1996-1988)-(log(lpa88/lna88)-log(lpa80/lna80))/(1988-1980);*U.S Skill Upgrading;

  *Tariffs;
  ddctar=(ctar96-ctar88)/(1996-1988) - (ctar88-ctar80)/(1988-1980);*Change in Canadian tariff;
  ddutar=(utar96-utar88)/(1996-1988) - (utar88-utar80)/(1988-1980);*Change in U.S tariff;

  *set pre-FTA tariff changes to zero;
  ddctar1=(ctar96-ctar88)/(1996-1988);*Change in Canadian tariff;
  ddutar1=(utar96-utar88)/(1996-1988);*Change in U.S tariff;

  run;


  proc means data=diff3 N mean stddev;
    var ddl ddla ddln ddlna ddlp ddlpa ddctar ddsk ddska ddctar1 ddutar ddutar1;
	title 'Double-Differencing Statistical Output - II(i)';
	run;

*Determine correlation between variables;
  proc corr data=diff3;
    var ddl ddla ddln ddlna ddlp ddlpa ddctar ddsk ddska ddctar1 ddutar ddutar1;
	title 'Correlation between Variables - II(i)';
	run;
       
***Regressions - II(i)*;
  *Employment - All Workers;
    proc reg data=diff3;
      model ddl = ddctar1 ddutar1 ddla;
	  title 'Regression on Employment - All Workers - II(i)';
	  run;

  *Employment - All Workers without US Tariff;
    proc reg data=diff3;
      model ddl = ddctar1 ddla;
	  title 'Regression on Employment - All Workers (without US Tariff) - II(i)';
	  run;

  *Employment - Production Workers;
	proc reg data=diff3;
	  model ddlp = ddctar1 ddutar1 ddlpa;
	  title 'Regression on Employment - Production Workers - II(i)';
	  run;

  *Employment - Production Workers without US Tariff;
	proc reg data=diff3;
	  model ddlp = ddctar1 ddlpa;
	  title 'Regression on Employment - Production Workers (without US Tariff) - II(i)';
	  run;

  *Employment - Non-Production Workers;
	proc reg data=diff3;
	  model ddln = ddctar1 ddutar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers - II(i)';
	  run;

  *Employment - Non-Production Workers without US Tariff;
	proc reg data=diff3;
	  model ddln = ddctar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers (without US Tariff) - II(i)';
	  run;
	  
  *Employment - Skill Upgrading;
	proc reg data=diff3;
	  model ddsk = ddctar1 ddutar1 ddska;
	  title 'Regression on Employment - Skill Upgrading Workers - II(i)';
	  run;

  *Employment - Skill Upgrading without US Tariff;
	proc reg data=diff3;
	  model ddsk = ddctar1 ddska;
	  title 'Regression on Employment - Skill Upgrading (without US Tariff) - II(i)';
	  run;

**********Modification II - ii. Time periods (1994-1988)-(1986-1980)**********;
*Create measures *;
  data diff4;
    set diff1;

*Double Differencing:;

  * Canada Employment Variables;
  ddl=(log(l94)-log(l88))/(1994-1988)-(log(l86)-log(l80))/(1986-1980);*Canadian Total Employment;
  ddln=(log(ln94)-log(ln88))/(1994-1988)-(log(ln86)-log(ln80))/(1986-1980);*Canadian Non-Production Employment;
  ddlp=(log(lp94)-log(lp88))/(1994-1988)-(log(lp86)-log(lp80))/(1986-1980);*Canadian Production Employment;
  ddsk=(log(lp94/ln94)-log(lp88/ln88))/(1994-1988)-(log(lp86/ln86)-log(lp80/ln80))/(1986-1980);*Canadian Skill Upgrading;

  * U.S Employment Variables;
  ddla=(log(la94)-log(la88))/(1994-1988)-(log(la86)-log(la80))/(1986-1980);*U.S Total Employment;
  ddlna=(log(lna94)-log(lna88))/(1994-1988)-(log(lna86)-log(lna80))/(1986-1980);*U.S Non-Production Employment;
  ddlpa=(log(lpa94)-log(lpa88))/(1994-1988)-(log(lpa86)-log(lpa80))/(1986-1980);*U.S Production Employment;
  ddska=(log(lpa94/lna94)-log(lpa88/lna88))/(1994-1988)-(log(lpa86/lna86)-log(lpa80/lna80))/(1986-1980);*U.S Skill Upgrading;

  *Tariffs;
  ddctar=(ctar94-ctar88)/(1994-1988) - (ctar86-ctar80)/(1986-1980);*Change in Canadian tariff;
  ddutar=(utar94-utar88)/(1994-1988) - (utar86-utar80)/(1986-1980);*Change in U.S tariff;

  *set pre-FTA tariff changes to zero;
  ddctar1=(ctar94-ctar88)/(1994-1988);*Change in Canadian tariff;
  ddutar1=(utar94-utar88)/(1994-1988);*Change in U.S tariff;

  run;


  proc means data=diff4 N mean stddev;
    var ddl ddla ddln ddlna ddlp ddlpa ddctar ddsk ddska ddctar1 ddutar ddutar1;
	title 'Double-Differencing Statistical Output - II(ii)';
	run;

*Determine correlation between variables;
  proc corr data=diff4;
    var ddl ddla ddln ddlna ddlp ddlpa ddctar ddsk ddska ddctar1 ddutar ddutar1;
	title 'Correlation between Variables - II(ii)';
	run;
	      
***Regressions - II(ii)*;
  *Employment - All Workers;
    proc reg data=diff4;
      model ddl = ddctar1 ddutar1 ddla;
	  title 'Regression on Employment - All Workers - II(ii)';
	  run;

  *Employment - All Workers without US Tariff;
    proc reg data=diff4;
      model ddl = ddctar1 ddla;
	  title 'Regression on Employment - All Workers (without US Tariff) - II(ii)';
	  run;

  *Employment - Production Workers;
	proc reg data=diff4;
	  model ddlp = ddctar1 ddutar1 ddlpa;
	  title 'Regression on Employment - Production Workers - II(ii)';
	  run;

  *Employment - Production Workers without US Tariff;
	proc reg data=diff4;
	  model ddlp = ddctar1 ddlpa;
	  title 'Regression on Employment - Production Workers (without US Tariff) - II(ii)';
	  run;

  *Employment - Non-Production Workers;
	proc reg data=diff4;
	  model ddln = ddctar1 ddutar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers - II(ii)';
	  run;

  *Employment - Non-Production Workers without US Tariff;
	proc reg data=diff4;
	  model ddln = ddctar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers (without US Tariff) - II(ii)';
	  run;
	  
  *Employment - Skill Upgrading;
	proc reg data=diff4;
	  model ddsk = ddctar1 ddutar1 ddska;
	  title 'Regression on Employment - Skill Upgrading Workers - II(ii)';
	  run;

  *Employment - Skill Upgrading without US Tariff;
	proc reg data=diff4;
	  model ddsk = ddctar1 ddska;
	  title 'Regression on Employment - Skill Upgrading (without US Tariff) - II(ii)';
	  run;

**********Modification II - iii. Time periods (1994-1988)-(1984-1980)**********;
*Create measures *;
  data diff5;
    set diff1;

*Double Differencing:;

  * Canada Employment Variables;
  ddl=(log(l94)-log(l88))/(1994-1988)-(log(l84)-log(l80))/(1984-1980);*Canadian Total Employment;
  ddln=(log(ln94)-log(ln88))/(1994-1988)-(log(ln84)-log(ln80))/(1984-1980);*Canadian Non-Production Employment;
  ddlp=(log(lp94)-log(lp88))/(1994-1988)-(log(lp84)-log(lp80))/(1984-1980);*Canadian Production Employment;
  ddsk=(log(lp94/ln94)-log(lp88/ln88))/(1994-1988)-(log(lp84/ln84)-log(lp80/ln80))/(1984-1980);*Canadian Skill Upgrading;

  * U.S Employment Variables;
  ddla=(log(la94)-log(la88))/(1994-1988)-(log(la84)-log(la80))/(1984-1980);*U.S Total Employment;
  ddlna=(log(lna94)-log(lna88))/(1994-1988)-(log(lna84)-log(lna80))/(1984-1980);*U.S Non-Production Employment;
  ddlpa=(log(lpa94)-log(lpa88))/(1994-1988)-(log(lpa84)-log(lpa80))/(1984-1980);*U.S Production Employment;
  ddska=(log(lpa94/lna94)-log(lpa88/lna88))/(1994-1988)-(log(lpa84/lna84)-log(lpa80/lna80))/(1984-1980);*U.S Skill Upgrading;

  *Tariffs;
  ddctar=(ctar94-ctar88)/(1994-1988) - (ctar84-ctar80)/(1984-1980);*Change in Canadian tariff;
  ddutar=(utar94-utar88)/(1994-1988) - (utar84-utar80)/(1984-1980);*Change in U.S tariff;

  *set pre-FTA tariff changes to zero;
  ddctar1=(ctar94-ctar88)/(1994-1988);*Change in Canadian tariff;
  ddutar1=(utar94-utar88)/(1994-1988);*Change in U.S tariff;

  run;


  proc means data=diff5 N mean stddev;
    var ddl ddla ddln ddlna ddlp ddlpa ddctar ddsk ddska ddctar1 ddutar ddutar1;
	title 'Double-Differencing Statistical Output - II(iii)';
	run;

*Determine correlation between variables;
  proc corr data=diff5;
    var ddl ddla ddln ddlna ddlp ddlpa ddctar ddsk ddska ddctar1 ddutar ddutar1;
	title 'Correlation between Variables - II(iii)';
	run;
      
***Regressions - II(i)*;
  *Employment - All Workers;
    proc reg data=diff5;
      model ddl = ddctar1 ddutar1 ddla;
	  title 'Regression on Employment - All Workers - II(iii)';
	  run;

  *Employment - All Workers without US Tariff;
    proc reg data=diff5;
      model ddl = ddctar1 ddla;
	  title 'Regression on Employment - All Workers (without US Tariff) - II(iii)';
	  run;

  *Employment - Production Workers;
	proc reg data=diff5;
	  model ddlp = ddctar1 ddutar1 ddlpa;
	  title 'Regression on Employment - Production Workers - II(iii)';
	  run;

  *Employment - Production Workers without US Tariff;
	proc reg data=diff5;
	  model ddlp = ddctar1 ddlpa;
	  title 'Regression on Employment - Production Workers (without US Tariff) - II(iii)';
	  run;

  *Employment - Non-Production Workers;
	proc reg data=diff5;
	  model ddln = ddctar1 ddutar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers - II(iii)';
	  run;

  *Employment - Non-Production Workers without US Tariff;
	proc reg data=diff5;
	  model ddln = ddctar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers (without US Tariff) - II(iii)';
	  run;
	  
  *Employment - Skill Upgrading;
	proc reg data=diff5;
	  model ddsk = ddctar1 ddutar1 ddska;
	  title 'Regression on Employment - Skill Upgrading Workers - II(iii)';
	  run;

  *Employment - Skill Upgrading without US Tariff;
	proc reg data=diff5;
	  model ddsk = ddctar1 ddska;
	  title 'Regression on Employment - Skill Upgrading (without US Tariff) - II(iii)';
	  run;



**********;
* Part C *;
**********;

**********Modification III - Ranking industries into 3 groups - based on the sizes of the Canadian tariff cut**********;
    proc sort data=diff2;
	  by ddctar1;
	  run;

	proc rank data=diff2   out=rank   groups=3;
	  var ddctar1;
	  ranks rank_ddctar1;
	  run;

	proc sort data=rank;
	  by rank_ddctar1;
	  run;

  *Set up new data sets for each group;
	data group0;
	  set rank;
	  if (rank_ddctar1=0) then output;
	  run;

	data group1;
	  set rank;
	  if (rank_ddctar1=1) then output;
	  run;

	data group2;
	  set rank;
	  if (rank_ddctar1=2) then output;
	  run;

  *Run regression for all three groups;

	*Group 0*;
	proc reg data=group0;
	  model ddl = ddctar1 ddutar1 ddla;
      title 'Regression on Employment - All Workers (Group 0)';
	  run;

	proc reg data=group0;
	  model ddlp = ddctar1 ddutar1 ddlpa;
	  title 'Regression on Employment - Production Workers (Group 0)';
	  run;

  *Employment - Non-Production Workers;
	proc reg data=group0;
	  model ddln = ddctar1 ddutar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers (Group 0)';
	  run;

   *Employment - Skill Upgrading;
	proc reg data=group0;
	  model ddsk = ddctar1 ddutar1 ddska;
	  title 'Regression on Employment - Skill Upgrading (Group 0)';
	  run;
******************

    *Group 1*;
	proc reg data=group1;
	  model ddl = ddctar1 ddutar1 ddla;
      title 'Regression on Employment - All Workers (Group 1)';
	  run;

	proc reg data=group1;
	  model ddlp = ddctar1 ddutar1 ddlpa;
	  title 'Regression on Employment - Production Workers (Group 1)';
	  run;

  *Employment - Non-Production Workers;
	proc reg data=group1;
	  model ddln = ddctar1 ddutar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers (Group 1)';
	  run;
	  
  *Employment - Skill Upgrading;
	proc reg data=group1;
	  model ddsk = ddctar1 ddutar1 ddska;
	  title 'Regression on Employment - Skill Upgrading (Group 1)';
	  run;
******************

    *Group 2*;
	proc reg data=group2;
	  model ddl = ddctar1 ddutar1 ddla;
      title 'Regression on Employment - All Workers (Group 2)';
	  run;

	proc reg data=group2;
	  model ddlp = ddctar1 ddutar1 ddlpa;
	  title 'Regression on Employment - Production Workers (Group 2)';
	  run;

  *Employment - Non-Production Workers;
	proc reg data=group2;
	  model ddln = ddctar1 ddutar1 ddlna;
	  title 'Regression on Employment - Non-Production Workers (Group 2)';
	  run;

  *Employment - Skill Upgrading;
	proc reg data=group2;
	  model ddsk = ddctar1 ddutar1 ddska;
	  title 'Regression on Employment - Skill Upgrading (Group 2)';
	  run;



	
