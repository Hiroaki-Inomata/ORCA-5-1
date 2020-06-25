\set ON_ERROR_STOP

alter table tbl_jyurrk add column hospnum numeric(2,0);
alter table tbl_jyurrk drop column hospid;
update tbl_jyurrk set hospnum = 1;
alter table tbl_jyurrk alter column hospnum set not null;
alter table tbl_jyurrk add constraint tbl_jyurrk_primary_key primary key (hospnum, ptid, nyugaikbn, sryka, sryymd, rennum, douji_rennum, kaikei_rennum, edanum);
create index idx_jyurrk_sryymd on tbl_jyurrk(hospnum, sryymd, nyugaikbn);
create index idx_jyurrk_upymd on tbl_jyurrk(hospnum, upymd, nyugaikbn);
