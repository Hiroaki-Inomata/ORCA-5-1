--                                      
-- 病名マスタテーブル                        
--                                    
-- ICD10-1(2013)追加
-- ICD10-2(2013)追加
--                                   
-- Create Date : 2017/1/6       

\set ON_ERROR_STOP

alter table master.tbl_byomei add column icd10_1_2 character varying(5);
alter table master.tbl_byomei add column icd10_2_2 character varying(5);
update master.tbl_byomei set icd10_1_2 = '';
update master.tbl_byomei set icd10_2_2 = '';

-- public.tbl_byomei(view)の再作成
drop view tbl_byomei;
create view tbl_byomei as select * from master.tbl_byomei;
