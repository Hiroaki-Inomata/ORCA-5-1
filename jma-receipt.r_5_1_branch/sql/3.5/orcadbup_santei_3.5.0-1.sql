\set ON_ERROR_STOP

alter table tbl_santei add column hospnum numeric(2,0);
alter table tbl_santei drop column hospid;
update tbl_santei set hospnum = 1;
alter table tbl_santei alter column hospnum set not null;
alter table tbl_santei add constraint tbl_santei_primary_key primary key (hospnum, ptid, sryym, srycd, nyugaikbn, sryka, hkncombinum);
