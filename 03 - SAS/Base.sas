proc sql;
	select count(*) from data_consult_analyse;
	select count(distinct(&id_pat.)) from data_consult_analyse;
quit;

proc freq data=verif_doublon;
	tables PB;
run;

data qssv4;
	set qssv3;
	where not missing(new_num_enq_ano);
run;

proc sort data=pop_analyse; by &id_pat.; run;
