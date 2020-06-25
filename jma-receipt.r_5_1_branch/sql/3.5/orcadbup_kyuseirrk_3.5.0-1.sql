\set ON_ERROR_STOP

alter table tbl_kyuseirrk add column hospnum numeric(2,0);
alter table tbl_kyuseirrk drop column hospid;
update tbl_kyuseirrk set hospnum = 1;
alter table tbl_kyuseirrk alter column hospnum set not null;
alter table tbl_kyuseirrk add constraint tbl_kyuseirrk_primary_key primary key (hospnum, ptid, chgymd);
