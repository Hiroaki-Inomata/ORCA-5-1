\set ON_ERROR_STOP

alter table tbl_jyurrk_prv add column hospnum numeric(2,0);
alter table tbl_jyurrk_prv drop column hospid;
update tbl_jyurrk_prv set hospnum = 1;
alter table tbl_jyurrk_prv alter column hospnum set not null;
alter table tbl_jyurrk_prv add constraint tbl_jyurrk_prv_primary_key primary key (hospnum, ptid, nyugaikbn, sryka, sryymd, rennum, douji_rennum, kaikei_rennum, edanum);
create index idx_jyurrk_prv_sryymd on tbl_jyurrk_prv(hospnum, sryymd, nyugaikbn);
create index idx_jyurrk_prv_upymd on tbl_jyurrk_prv(hospnum, upymd, nyugaikbn);
