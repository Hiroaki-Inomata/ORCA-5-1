\set ON_ERROR_STOP

alter table tbl_rece_putb add column hospnum numeric(2,0);
alter table tbl_rece_putb drop column hospid;
update tbl_rece_putb set hospnum = 1;
alter table tbl_rece_putb alter column hospnum set not null;
alter table tbl_rece_putb add constraint tbl_rece_putb_primary_key primary key (hospnum, prefkbn, syubetu_srt1, grp_no1, syubetu_srt2, grp_no2, kouhi_no, styukymd, edyukymd);
