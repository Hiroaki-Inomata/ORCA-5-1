--                            --
-- マスタ管理テーブルの分離   --
--                            --
-- Create Date : 2017/06/08   --
--                            --
-- 点数                       --
--                            --

\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';

INSERT INTO
    master.tbl_mstkanri_master
SELECT
    *
FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB00'
;

DELETE FROM
    tbl_mstkanri_public
WHERE
    kanricd = 'ORCADB00'
;
