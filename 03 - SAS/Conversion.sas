/* Numeric to char */
proc sql;
    create table data_consult_analyse as
    select *,
		put(prs_nat_ref, 8.) as prs_nat_ref
    from hcru.&nom_projet._CONSULT
    where &id_pat. in (select &id_pat. from &pop_oskar.);
quit;

/* Char to numeric */
proc sql;
    create table med_cancer as
    select *
    from med_all
    where input(code_ucd, comma12.) in (select code_ucd from ref_ATC_UCD2);

    create table med_autre as
    select *
    from med_all
    where input(code_ucd, comma12.) not in (select code_ucd from ref_ATC_UCD2);
quit;
