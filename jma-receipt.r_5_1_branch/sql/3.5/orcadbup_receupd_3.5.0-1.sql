\set ON_ERROR_STOP

alter table tbl_receupd add column hospnum numeric(2,0);
alter table tbl_receupd drop column hospid;
update tbl_receupd set hospnum = 1;
alter table tbl_receupd alter column hospnum set not null;
alter table tbl_receupd add constraint tbl_receupd_primary_key primary key (hospnum, ptid, nyugaikbn, sryka, sryym, hknkbn, hknkbn2);
create index idx_receupd_upymd on tbl_receupd(hospnum, upymd, nyugaikbn);
