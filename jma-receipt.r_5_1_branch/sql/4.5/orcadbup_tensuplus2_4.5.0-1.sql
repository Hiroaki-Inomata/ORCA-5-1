\set ON_ERROR_STOP

alter table tbl_tensuplus2  add column tukijgnkaisu numeric(3);
alter table tbl_tensuplus2  add column tukijgntouyoryo numeric(10,5);
update  tbl_tensuplus2 set tukijgnkaisu=0,tukijgntouyoryo=0 ;

