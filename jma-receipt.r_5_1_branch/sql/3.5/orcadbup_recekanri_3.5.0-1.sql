\set ON_ERROR_STOP

alter table tbl_recekanri add column hospnum numeric(2,0);
update tbl_recekanri set hospnum = 1;
alter table tbl_recekanri alter column hospnum set not null;
alter table tbl_recekanri drop constraint  tbl_recekanri_primary_key;
alter table tbl_recekanri add constraint tbl_recekanri_primary_key primary key (hospnum, prtid, syokbn, creymd, crehms, teisyutusaki, sryka, prefkbn_srt, hknjanum_srt, hknjanum, recesyubetu, recesyubetu_srt, zaitaku, chouki);
drop index idx_recekanri_sryym;
create index idx_recekanri_sryym on tbl_recekanri(hospnum, sryym);
