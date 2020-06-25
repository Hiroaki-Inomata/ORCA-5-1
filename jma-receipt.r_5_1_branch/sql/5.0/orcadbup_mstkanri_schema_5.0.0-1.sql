--                            --
-- マスタ管理テーブルの分離   --
--                            --
-- Create Date : 2017/01/20   --
--                            --

\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';

CREATE TABLE
    tbl_mstkanri_public
AS
SELECT
    *
FROM
    tbl_mstkanri
WHERE
    kanricd IN (
      'ORCADB00','ORCADB01','ORCADB02',
      'ORCADB04','ORCADB05','ORCADB09',
      'ORCADB10','ORCADB15','ORCADB16'
    )
;

DELETE FROM
    tbl_mstkanri
WHERE
    kanricd IN (
      'ORCADB00','ORCADB01','ORCADB02',
      'ORCADB04','ORCADB05','ORCADB09',
      'ORCADB10','ORCADB15','ORCADB16'
    )
;

ALTER TABLE ONLY tbl_mstkanri_public
    ADD CONSTRAINT tbl_mstkanri_public_primary_key PRIMARY KEY (hospnum, kanricd);

ALTER TABLE IF EXISTS tbl_mstkanri SET SCHEMA master;
ALTER TABLE IF EXISTS master.tbl_mstkanri RENAME TO tbl_mstkanri_master;

ALTER TABLE master.tbl_mstkanri_master DROP CONSTRAINT tbl_mstkanri_primary_key;
ALTER TABLE master.tbl_mstkanri_master
    ADD CONSTRAINT tbl_mstkanri_master_primary_key PRIMARY KEY (hospnum, kanricd);

CREATE VIEW
    tbl_mstkanri
AS
(
SELECT
    *
FROM
    tbl_mstkanri_public
UNION ALL
SELECT
    *
FROM
    master.tbl_mstkanri_master
ORDER BY
    hospnum,
    kanricd
)
;
