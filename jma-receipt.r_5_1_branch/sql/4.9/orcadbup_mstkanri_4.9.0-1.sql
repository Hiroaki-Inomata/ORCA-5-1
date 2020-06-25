\set ON_ERROR_STOP

SET client_encoding = 'EUC_JP';
INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB28','R-040700-1-00000000-0','R-040700-1-00000000-0','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),'医薬品傷病名マスタ'
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB28');
