\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';
INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB29','R-040800-1-00000000-0','R-040800-1-00000000-0','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),'医薬品分類マスタ'
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB29');
INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB30','R-040800-1-00000000-0','R-040800-1-00000000-0','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),'向精神薬成分コードマスタ'
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB30');
