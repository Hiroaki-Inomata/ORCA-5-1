\set ON_ERROR_STOP

-- 管理コードORCADB00以外は削除
delete from tbl_dbkanri where kanricd <> 'ORCADB00';
--
--alter table tbl_dbkanri drop constraint  tbl_dbkanri_primary_key;
alter table tbl_dbkanri drop column dbrversion1;
alter table tbl_dbkanri drop column dbrversion2;
--
-- version,dbsversion1,dbsversion2の先頭に空白があるので取り除く
update tbl_dbkanri set version = trim(version);
update tbl_dbkanri set dbsversion1 = trim(dbsversion1);
update tbl_dbkanri set dbsversion2 = trim(dbsversion2);

update tbl_dbkanri set upymd = to_char(now(),'yyyymmdd'), uphms = to_char(now(),'hh24miss');
--
vacuum full tbl_dbkanri;
