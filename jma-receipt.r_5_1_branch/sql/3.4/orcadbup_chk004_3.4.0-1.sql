\set ON_ERROR_STOP
--
-- 患者番号(PTID)追加
-- 
-- Create Date : 2007/2/21          --
--
--  項目追加                        --
alter table TBL_CHK004
   add column PTID		numeric(10,0);


--  ゼロ設定                       --
update TBL_CHK004 set ptid = 0;

alter table TBL_CHK004
   alter ptid	set default 0;
