\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';
INSERT INTO master.tbl_mstkanri_master
SELECT 1,'ORCADB31','R-050000-1-00000000-0','R-050000-1-00000000-0','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),'特材品目算定マスタ'
WHERE NOT EXISTS (SELECT 1 FROM master.tbl_mstkanri_master WHERE hospnum = 1 AND kanricd = 'ORCADB31');
