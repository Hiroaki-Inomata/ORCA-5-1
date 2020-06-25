\set ON_ERROR_STOP

alter table tbl_order add column hospnum numeric(2,0);
alter table tbl_order drop column hospid;
update tbl_order set hospnum = 1;
alter table tbl_order alter column hospnum set not null;
alter table tbl_order add constraint tbl_order_primary_key primary key (hospnum, karte_key, ordernum);
drop index idx_order_ptnum;
create index idx_order_ptnum on tbl_order(hospnum, ptnum);
drop index idx_order_sryymd;
create index idx_order_sryymd on tbl_order(hospnum, sryymd);
drop index idx_order_ukeymd;
create index idx_order_ukeymd on tbl_order(hospnum, ukeymd);
