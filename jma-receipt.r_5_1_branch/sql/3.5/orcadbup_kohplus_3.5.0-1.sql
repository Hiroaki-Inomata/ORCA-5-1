\set ON_ERROR_STOP

alter table tbl_kohplus add column hospnum numeric(2,0);
alter table tbl_kohplus drop column hospid;
update tbl_kohplus set hospnum = 1;
alter table tbl_kohplus alter column hospnum set not null;
alter table tbl_kohplus add constraint tbl_kohplus_primary_key primary key (hospnum, prefnum, citynum, kohnum, ptkbn, yukostymd);
