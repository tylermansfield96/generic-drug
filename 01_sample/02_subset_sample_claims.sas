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
%let type = pde;


* Unique beneficiaries in the sample;
data sample_benes;
  set bt.sample_benes_&pct (keep = bene_id);
run;

proc sort data = sample_benes nodupkey;
  by bene_id;
run;

proc print data = sample_benes (obs = 10);
run;

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
%macro subset_claims_to_sample(pct, year, type);

  * Part D Event (PDE);
  %if &type = pde %then %do;
    data bt.sample_pde_&year._&pct;
      merge pde.pde_&year._&pct (in = pde) sample_benes (in = bsf);
      by bene_id;
      if pde & bsf;
    run;
  %end;

  * Inpatient (IP);
  %if &type = ip %then %do;
    data bt.sample_ip_&year._&pct;
      merge ip.ip_&year._&pct (in = ip) sample_benes (in = bsf);
      by bene_id;
      if ip & bsf;
    run;
  %end;

  * OUtpatient (OP);
  %if &type = op %then %do;
    data bt.sample_op_&year._&pct;
      merge op.op_&year._&pct (in = op) sample_benes (in = bsf);
      by bene_id;
      if op & bsf;
    run;
  %end;

  * Carrier (Car);
  %if &type = car %then %do;
    data bt.sample_car_&year._&pct;
      merge car.car_&year._&pct (in = car) sample_benes (in = bsf);
      by bene_id;
      if car & bsf;
    run;
  %end;

  proc print data = bt.sample_&type._&year._&pct (obs = 10);
  run;

  proc export
    data = bt.sample_&type._&year._&pct dbms=csv
    outfile = "~/baicker-DUA50589/eflack-dua50589/bottle_data/sample_&type._&year._&pct..csv"
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
%macro loop_years(pct, first_year, last_year, type);
  %do year = &first_year %to &last_year;
    %subset_claims_to_sample(&pct, &year, &type);
  %end;
%mend;

%loop_years(&pct, &first_year, &last_year, &type);
