\set ON_ERROR_STOP

alter table tbl_ptkinki add column kinkstymd char(8);
alter table tbl_ptkinki add column touyoymd char(8);
update tbl_ptkinki set kinkstymd = ' ';
update tbl_ptkinki set touyoymd = ' ';
