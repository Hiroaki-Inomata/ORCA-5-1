\set ON_ERROR_STOP

alter table tbl_ptnyuinrrk
add column ksnsrycd1   character varying(9) default '',
add column ksnsrycd2   character varying(9) default '',
add column ksnsrycd3   character varying(9) default '',
add column skjkbn_asa  character varying(1) default '',
add column skjkbn_hiru character varying(1) default '',
add column skjkbn_yu   character varying(1) default ''
;
update tbl_ptnyuinrrk set
ksnsrycd1   = '', 
ksnsrycd2   = '',   
ksnsrycd3   = '', 
skjkbn_asa  = '', 
skjkbn_hiru = '', 
skjkbn_yu   = '';

