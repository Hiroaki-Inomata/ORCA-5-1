--                                      
-- 病名マスタテーブル                        
--                                    
-- ICD10-2追加
-- 難病外来追加
-- 整数項目のデータ型変更
--                                   
-- Create Date : 2011/2/21       

\set ON_ERROR_STOP

alter table tbl_byomei alter column byomeimoji type smallint;
alter table tbl_byomei alter column tanbyomeimoji type smallint;
alter table tbl_byomei alter column ikosakicd type character varying(7);
alter table tbl_byomei alter column tokskncd type smallint;
alter table tbl_byomei alter column tandokukbn type smallint;
alter table tbl_byomei alter column hknskykbn type smallint;
alter table tbl_byomei alter column hyojuncd type character varying(8);
alter table tbl_byomei rename column icd10 to icd10_1;
alter table tbl_byomei alter column syusaiymd type character varying(8);
alter table tbl_byomei alter column chgymd type character varying(8);
alter table tbl_byomei alter column haisiymd type character varying(8);
alter table tbl_byomei alter column creymd type character varying(8);
alter table tbl_byomei alter column upymd type character varying(8);
alter table tbl_byomei alter column uphms type character varying(6);
alter table tbl_byomei add column icd10_2 character varying(5);
alter table tbl_byomei add column nanbyocd smallint default 0;
update tbl_byomei set icd10_2 = '';
update tbl_byomei set nanbyocd = 0;
