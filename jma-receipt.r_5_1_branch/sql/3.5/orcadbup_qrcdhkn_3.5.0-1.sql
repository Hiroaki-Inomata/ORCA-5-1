\set ON_ERROR_STOP

alter table tbl_qrcdhkn add column hospnum numeric(2,0);
alter table tbl_qrcdhkn drop column hospid;
update tbl_qrcdhkn set hospnum = 1;
alter table tbl_qrcdhkn alter column hospnum set not null;
alter table tbl_qrcdhkn add constraint tbl_qrcdhkn_primary_key primary key (hospnum, registymd, registid);
