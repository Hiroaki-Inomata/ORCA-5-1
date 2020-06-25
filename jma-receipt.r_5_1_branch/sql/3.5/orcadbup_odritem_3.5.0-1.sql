\set ON_ERROR_STOP

alter table tbl_odritem add column hospnum numeric(2,0);
alter table tbl_odritem drop column hospid;
update tbl_odritem set hospnum = 1;
alter table tbl_odritem alter column hospnum set not null;
alter table tbl_odritem add constraint tbl_odritem_primary_key primary key (hospnum, karte_key, ordernum, itemnum);
