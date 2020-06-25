--                                      
-- ワーク診療行為テーブル                        
--                                    
-- 入力コードｎ桁数変更
-- 入力値ｎー５追加
--                                   
-- Create Date : 2011/2/10       

\set ON_ERROR_STOP

alter table tbl_wksryact alter column inputcd_1 type character varying(54);
alter table tbl_wksryact alter column inputcd_2 type character varying(54);
alter table tbl_wksryact alter column inputcd_3 type character varying(54);
alter table tbl_wksryact alter column inputcd_4 type character varying(54);
alter table tbl_wksryact alter column inputcd_5 type character varying(54);
alter table tbl_wksryact add column inputchi1_5 character varying(8);
alter table tbl_wksryact add column inputchi2_5 character varying(8);
alter table tbl_wksryact add column inputchi3_5 character varying(8);
alter table tbl_wksryact add column inputchi4_5 character varying(8);
alter table tbl_wksryact add column inputchi5_5 character varying(8);
