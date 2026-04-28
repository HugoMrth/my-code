/* Count */
proc sql;
	select count(*) from data_consult_analyse;
	select count(distinct(&id_pat.)) from data_consult_analyse;
quit;

/* Freq */
proc freq data=verif_doublon;
	tables PB;
run;

/* Selection non NA */
data qssv4;
	set qssv3;
	where not missing(new_num_enq_ano);
run;

/* Sort */
proc sort data=pop_analyse; by &id_pat.; run;

/* Doublons */
proc sort data=eccm nodupkey out=eccm2;
    by _all_;
run;

/* Merge */
data hosp_meta2;
	merge hosp_meta(in=a) data_desc(in=b);
	by &id_pat.;
	if a ;
run;
