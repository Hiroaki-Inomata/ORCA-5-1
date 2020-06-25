-- 点数テーブル(TBL_TENSU)
-- 平成３０年４月改定対応
--                                      
\set ON_ERROR_STOP

-- SET client_encoding = 'EUC_JP';

ALTER TABLE public.tbl_tensu_org ADD COLUMN akuseibyoriksn smallint default 0;
