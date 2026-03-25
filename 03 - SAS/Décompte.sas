proc sql;
	select count(*) from data_consult_analyse;
	select count(distinct(&id_pat.)) from data_consult_analyse;
quit;

proc freq data=verif_doublon;
	tables PB;
run;
