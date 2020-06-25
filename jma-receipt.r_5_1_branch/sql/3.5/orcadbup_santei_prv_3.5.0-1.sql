\set ON_ERROR_STOP

alter table tbl_santei_prv add column hospnum numeric(2,0);
alter table tbl_santei_prv drop column hospid;
update tbl_santei_prv set hospnum = 1;
alter table tbl_santei_prv alter column hospnum set not null;
alter table tbl_santei_prv add constraint tbl_santei_prv_primary_key primary key (hospnum, ptid, sryym, srycd, nyugaikbn, sryka, hkncombinum);
