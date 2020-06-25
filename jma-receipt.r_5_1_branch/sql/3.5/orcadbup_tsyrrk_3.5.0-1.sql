\set ON_ERROR_STOP

alter table tbl_tsyrrk add column hospnum numeric(2,0);
alter table tbl_tsyrrk add column skjninstymd char(8);
alter table tbl_tsyrrk add column hkntekkbn char(1);
alter table tbl_tsyrrk drop column hospid;
update tbl_tsyrrk set hospnum = 1,skjninstymd = ninstymd,hkntekkbn = '0';
alter table tbl_tsyrrk alter column hospnum set not null;
alter table tbl_tsyrrk add constraint tbl_tsyrrk_primary_key primary key (hospnum, ptid, ninstymd);
