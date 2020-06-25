\set ON_ERROR_STOP

alter table tbl_syunou_prv add column hospnum numeric(2,0);
alter table tbl_syunou_prv drop column hospid;
update tbl_syunou_prv set hospnum = 1;
alter table tbl_syunou_prv alter column hospnum set not null;
alter table tbl_syunou_prv add constraint tbl_syunou_prv_primary_key primary key (hospnum, nyugaikbn, ptid, denpnum, creymd);
