\set ON_ERROR_STOP

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB26','R-040700-1-00000000-0','R-040700-1-00000000-0','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB26');
