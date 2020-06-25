\set ON_ERROR_STOP

alter table tbl_seikyu_main add column tokkinm1 varchar(12);
alter table tbl_seikyu_main add column tokkinm2 varchar(12);
alter table tbl_seikyu_main add column tokkinm3 varchar(12);
alter table tbl_seikyu_main add column tokkinm4 varchar(12);
alter table tbl_seikyu_main add column tokkinm5 varchar(12);
update tbl_seikyu_main set tokkinm1 = ' ';
update tbl_seikyu_main set tokkinm2 = ' ';
update tbl_seikyu_main set tokkinm3 = ' ';
update tbl_seikyu_main set tokkinm4 = ' ';
update tbl_seikyu_main set tokkinm5 = ' ';
