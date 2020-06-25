\set ON_ERROR_STOP

alter table tbl_receprt add column hospnum numeric(2,0);
update tbl_receprt set hospnum = 1;
alter table tbl_receprt alter column hospnum set not null;
alter table tbl_receprt drop constraint tbl_receprt_primary_key;
alter table tbl_receprt add constraint tbl_receprt_primary_key primary key (hospnum, prtid, receym, creymd, crehms, teisyutusaki, sryka, prefkbn_srt, hknjanum_srt, hknjanum, recesyubetu, tekstymd, zaitaku, chouki, ptnum, rennum, recenum);
