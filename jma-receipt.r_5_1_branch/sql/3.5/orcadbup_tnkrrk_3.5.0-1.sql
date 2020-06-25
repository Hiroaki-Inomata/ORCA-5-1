\set ON_ERROR_STOP

alter table tbl_tnkrrk add column hospnum numeric(2,0);
alter table tbl_tnkrrk add column skjninstymd char(8);
alter table tbl_tnkrrk add column hkntekkbn char(1);
alter table tbl_tnkrrk drop column hospid;
update tbl_tnkrrk set hospnum = 1,skjninstymd = stymd,hkntekkbn = '0';
alter table tbl_tnkrrk alter column hospnum set not null;
alter table tbl_tnkrrk add constraint tbl_tnkrrk_primary_key primary key (hospnum, ptid, stymd);
