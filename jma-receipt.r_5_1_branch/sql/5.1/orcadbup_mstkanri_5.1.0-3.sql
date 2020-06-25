\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';
INSERT INTO master.tbl_mstkanri_master
SELECT 1,'ORCADB32','R-040800-1-00000000-0','R-040800-1-00000000-0','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),'レセプト記載マスタ'
WHERE NOT EXISTS (SELECT 1 FROM master.tbl_mstkanri_master WHERE hospnum = 1 AND kanricd = 'ORCADB32');
