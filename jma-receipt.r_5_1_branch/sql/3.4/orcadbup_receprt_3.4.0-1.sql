--                                    --
-- レセプト明細テーブルの変更         --
--                                    --
-- 項目の追加                         --
--               (SYUBETU_SRT1)       --
--               (GRP_NO1)            --
--               (HONKZKKBN)          --
--               (KOUHI_NO)           --
--               (SYUBETU_UNDER_1)    --
--               (GRP_NO2)            --
--               (SYUBETU_UNDER_2)    --
--               (SYUBETU_SRT2)       --
--               (KOUGAKU)            --
--                                    --
-- Create Date : 2007/03/01           --
--                                    --
\set ON_ERROR_STOP

--    項目追加                        --
alter table tbl_receprt
   add column KOUGAKU     char(1);
alter table tbl_receprt
   add column SYUBETU_SRT1 integer;
alter table tbl_receprt
   add column GRP_NO1     numeric(3);
alter table tbl_receprt
   add column HONKZKKBN    char(1);
alter table tbl_receprt
   add column KOUHI_NO     numeric(3);
alter table tbl_receprt
   add column SYUBETU_UNDER_1 varchar(80);
alter table tbl_receprt
   add column GRP_NO2      char(3);
alter table tbl_receprt
   add column SYUBETU_UNDER_2 varchar(40);
alter table tbl_receprt
   add column SYUBETU_SRT2 integer;

--    ゼロ設定                       --
alter table tbl_receprt
   alter SYUBETU_SRT1 set default 0;
alter table tbl_receprt
   alter KOUHI_NO set default 0;
alter table tbl_receprt
   alter  SYUBETU_SRT2 set default 0;
alter table tbl_receprt
   alter  GRP_NO1 set default 0;
alter table tbl_receprt
   alter  KOUGAKU  set default '9';

update tbl_receprt set SYUBETU_SRT1 = 0,SYUBETU_SRT2 = 0,KOUHI_NO = 0,GRP_NO1 = 0,KOUGAKU = '9';
