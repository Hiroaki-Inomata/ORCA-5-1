\set ON_ERROR_STOP

alter table tbl_help add column hospnum numeric(2,0);
update tbl_help set hospnum = 1;
alter table tbl_help alter column hospnum set not null;
alter table tbl_help drop constraint tbl_help_primary_key;
alter table tbl_help add constraint tbl_help_primary_key primary key (hospnum, helpkbn, key_opid, ldname, menunum1, menunum2, edanum);
drop index idx_help_key_opid;
create index idx_help_key_opid on tbl_help(hospnum, key_opid);
drop index idx_help_menu;
create index idx_help_menu on tbl_help(hospnum, menu);
