/* -----------------------------------------------------------------------------
Proj: Polypharmacy
Author: Evan Flack (evanjflack@gmail.com)
Desc: Identifies OASI beneficiaries that are dually eligible for medicaid,
  have 12 full months of coverage, and do not die during the year. This will
  become the age 66+ dual sample for spending/risk prediction.
----------------------------------------------------------------------------- */

* librefs;
%include "~/baicker-DUA50589/eflack-dua50589/bottle/00_pre_process/set_librefs.sas";

* Parameters from bash script;
/* %let pct = %scan(&sysparm, 1);
%let first_year = %scan(&sysparm, 2);
%let last_year = %scan(&sysparm, 3); */

* Dev parameters (comment out when inheriting from bash script);
%let pct=0001pct;
%let first_year = 2007;
%let last_year = 2013;

/* -----------------------------------------------------------------------------
Macro: subset_dual
Desc: Subsets bsf file to oasi beneficiaries that that are dually eligible for
  medicaid, have 12 full months of coverage, and do not die during the year
Arg:
  pct: sample percentage
  year: sample year
Out:
  dual_bsf_&year._&pct: subsetted bsf file
----------------------------------------------------------------------------- */
%macro subset_sample(pct, year);
  data bt.sample_&year._&pct
       (keep = bene_id rfrnc_yr death_dt birth_mo age1 race sex
               part_d_mo pdp_mo death_mo death_yr end_mo bene_zip
               mort state_cd cntrct01--cntrct12 pbpid01--pbpid12
               dual_ind1-dual_ind12 cstshr_ind1-cstshr_ind12
               part_d_ind1--part_d_ind12);
    set bsf.bsf_&year._&pct;

    * Create indicators for part D coverage;
    array cntrct cntrct01--cntrct12;
    array  part_d_ind{12} part_d_ind1-part_d_ind12;
    do i = 1 to 12;
      part_d_ind{i} = ((rfrnc_yr <= 2009 & cntrct{i} ^= "" & cntrct{i} ^= "0" &
                       cntrct{i} ^= "N" & cntrct{i} ^= "*" & cntrct{i} ^= "X") |
                       (rfrnc_yr >= 2010 & cntrct{i} ^= "" & cntrct{i} ^= "0" &
                        cntrct{i} ^= "N" & cntrct{i} ^= "*"));
    end;
    part_d_mo = sum(of part_d_ind{*});

    * Months of same cntrct;
    array  same_cntrct{12} same_cntrct1-same_cntrct12;
    do i = 1 to 12;
      same_cntrct{i} = (cntrct{i} = cntrct{1});
    end;
    same_cntrct_mo = sum(of same_cntrct{*});

    * PDP indicators;
    array pdp_ind{12} pdp_ind1-pdp_ind12;
    do i = 1 to 12;
      pdp_ind{i} = (substr(cntrct{i}, 1, 1) = "S");
    end;
    pdp_mo = sum(of pdp_ind{*});

    * Dual eligibility indicators;
    array dual dual_01--dual_12;
    array  dual_ind{12} dual_ind1-dual_ind12;
    do i = 1 to 12;
      dual_ind{i} = (dual{i} = "02" | dual{i} = "04" | dual{i} = "08" |
                     dual{i} = "01" | dual{i} = "03" | dual{i} = "05" |
                     dual{i} = "06");
    end;
    dual_mo = sum(of dual_ind{*});

    * LIS eligibility indicators;
    array cstshr cstshr01--cstshr12;
    array  cstshr_ind{12} cstshr_ind1-cstshr_ind12;
    do i = 1 to 12;
      cstshr_ind{i} = (cstshr{i} = "01" | cstshr{i} = "02" | cstshr{i} = "03" |
                       cstshr{i} = "04" | cstshr{i} = "05" | cstshr{i} = "06" |
                       cstshr{i} = "07" | cstshr{i} = "08");
    end;
    cstshr_mo = sum(of cstshr_ind{*});

    * Indicator for any dual/LIS eligibility;
    any_dual_cstshr = (dual_mo > 0 | cstshr_mo > 0);

    * Indicator for whether or not beneficary dies during the year;
    death_mo = month(death_dt);
    death_yr = year(death_dt);
    if death_yr = rfrnc_yr then end_mo = death_mo; else end_mo = 12;
    if death_yr = rfrnc_yr then mort = 1; else mort = 0;

    * Keep only benes that are OASI, 66 or older, and have at least one month in
    a PDP in that year;

    if age1 >= 66;
    if orec = 0;
    if pdp_mo > 0;
  run;

  proc print data = bt.sample_&year._&pct (obs = 10);
  run;

  * Export subsetted bsf file to CSV;
  proc export
    data = bt.sample_&year._&pct dbms=csv
    outfile = "~/baicker-DUA50589/eflack-dua50589/data1/bottle/sample_&year._&pct..csv"
    replace;
  run;
%mend;

/* -----------------------------------------------------------------------------
Macro: loop_years
Desc: loops over subset_dual and makes a simple list of beneficiaries and
      calendar present in the subsetted bsf file
----------------------------------------------------------------------------- */
%macro loop_years(pct, first_year, last_year);
  * Loop over subset_dual and combine;
  %do year = &first_year %to &last_year;
    * Run the sample subset macro;
    %subset_sample(&pct, &year);
    * Make a list of all benes/rfrnc_yrs potentially in the sample;
    %if &year = &first_year %then %do;
      data bt.sample_benes_&pct;
        set bt.sample_&year._&pct (keep = bene_id rfrnc_yr);
      run;
    %end;
    %else %do;
      data bt.sample_benes_&pct;
        set bt.sample_benes_&pct
        bt.sample_&year._&pct (keep = bene_id rfrnc_yr);
      run;
    %end;
  %end;

  proc sort data = bt.sample_benes_&pct;
    by bene_id rfrnc_yr;
  run;

  * Export list of benes/years to csv;
  proc export
    data = bt.sample_benes_&pct dbms=csv
    outfile = "~/baicker-DUA50589/eflack-dua50589/bottle_data/sample_benes_&pct..csv"
    replace;
  run;

%mend;

%loop_years(&pct, &first_year, &last_year);
