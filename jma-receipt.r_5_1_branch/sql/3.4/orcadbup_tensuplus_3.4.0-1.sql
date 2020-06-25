--                                    --
-- 点数附加情報テーブルの変更         --
--                                    --
-- 項目の追加                         --
--   算定履歴区分(SANTEIRRKKBN)       --
--   月上限回数(JGNCNT)               --
--   日上限回数(JGNCNT1D)             --
--   エラー処理(JGNCNTERR)            --
--   総量編集区分(SOURYOHENKBN)            --
--   一般名記載区分(IPNKISAIKBN)      --
--   一般名記載単位(IPNKANZANTANICD)  --
--   一般名記載単位名(IPNKANZANTANINAME) --
--   一般名記載換算値(IPNKANZANCHI)   --
--                                    --
-- Create Date : 2006/12/18           --
--                                    --
\set ON_ERROR_STOP

--    項目追加                        --
alter table tbl_tensuplus
   add column SANTEIRRKKBN  numeric(1);
alter table tbl_tensuplus
   add column JGNCNT	  numeric(3);
alter table tbl_tensuplus
   add column JGNCNT1D	  numeric(3);
alter table tbl_tensuplus
   add column JGNCNTERR	  numeric(1);
alter table tbl_tensuplus
   add column SOURYOHENKBN	  numeric(1);
alter table tbl_tensuplus
   add column IPNKISAIKBN	  numeric(1);
alter table tbl_tensuplus
   add column IPNKANZANTANICD	  char(3);
alter table tbl_tensuplus
   add column IPNKANZANTANINAME   varchar(24);
alter table tbl_tensuplus
   add column IPNKANZANCHI   numeric(10,5);

--    ゼロ設定                       --
alter table tbl_tensuplus
   alter SANTEIRRKKBN set default 0;
alter table tbl_tensuplus
   alter JGNCNT set default 0;
alter table tbl_tensuplus
   alter JGNCNT1D set default 0;
alter table tbl_tensuplus
   alter JGNCNTERR set default 0;
alter table tbl_tensuplus
   alter SOURYOHENKBN set default 0;
alter table tbl_tensuplus
   alter IPNKISAIKBN set default 0;
alter table tbl_tensuplus
   alter IPNKANZANCHI set default 0;

update tbl_tensuplus set SANTEIRRKKBN='0',JGNCNT='000', JGNCNT1D='000' , JGNCNTERR='0', SOURYOHENKBN='0' , IPNKISAIKBN='0' , IPNKANZANCHI='0';

