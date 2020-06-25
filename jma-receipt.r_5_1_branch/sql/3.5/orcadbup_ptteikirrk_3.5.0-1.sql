\set ON_ERROR_STOP

alter table tbl_ptteikirrk add column hospnum numeric(2,0);
alter table tbl_ptteikirrk drop column hospid;
update tbl_ptteikirrk set hospnum = 1;
alter table tbl_ptteikirrk alter column hospnum set not null;
alter table tbl_ptteikirrk add constraint tbl_ptteikirrk_primary_key primary key (hospnum, ptid, sryym, sakkbn, hkncombinum, denpnum);
