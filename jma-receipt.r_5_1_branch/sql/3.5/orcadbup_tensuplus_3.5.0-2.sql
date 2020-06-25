\set ON_ERROR_STOP

alter table tbl_tensuplus add column jgncntetcm numeric(2,0);
alter table tbl_tensuplus add column jgncntetc numeric(3,0);
alter table tbl_tensuplus alter jgncntetcm set default 0;
alter table tbl_tensuplus alter jgncntetc set default 0;
update tbl_tensuplus set jgncntetcm = 0 , jgncntetc= 0;
