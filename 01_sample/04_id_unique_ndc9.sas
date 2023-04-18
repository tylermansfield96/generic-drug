/* -----------------------------------------------------------------------------
Proj: Polypharmacy
Author: Evan Flack (evanjflack@gmail.com)
Desc: Subsets parts A-D medicare claims to new enrollees identified in
      01a_id_new_enrollees.
----------------------------------------------------------------------------- */

* librefs;
%include "~/baicker-DUA50589/eflack-dua50589/bottle/00_pre_process/set_librefs.sas";

* Dev parameters (comment out when inheriting from bash script);
%let pct = 20pct;
%let first_year = 2007;
%let last_year = 2013;

/* -----------------------------------------------------------------------------
Macro: subset_claims_to_sample
Desc: Subsets parts A-D medicare claims to new enrollees identified in
  01a_id_new_enrollees
Arg:
  pct: sample percentage
  year: sample year
  type: type of claims (bsf, pde, ip, op, car)
Out:
  sample_&type._&year._&pct: subsetted claims file
----------------------------------------------------------------------------- */
%macro id_unique_ndc9(pct, year, type);

  data u_lab_prod;
    set bt.sample_pde_&year._&pct (keep = prdsrvid lab_prod);
  run;

  proc sort data = u_lab_prod nodupkey;
    by lab_prod;
  run;

  proc print data = u_lab_prod (obs = 10);
  run;

  proc export
    data = u_lab_prod dbms=csv
    outfile = "~/baicker-DUA50589/eflack-dua50589/bottle_data/u_lab_prod_&year._&pct..csv"
    replace;
  run;
%mend;

/* -----------------------------------------------------------------------------
Macro: loop_years
Desc: loops over subset_claims_to_sample by calendar year
Arg:
  pct
  first_year
  last_year
  type
Out:
  output from subset_claims_to_sample
----------------------------------------------------------------------------- */
%macro loop_years(pct, first_year, last_year);
  %do year = &first_year %to &last_year;
    %id_unique_ndc9(&pct, &year);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year);
