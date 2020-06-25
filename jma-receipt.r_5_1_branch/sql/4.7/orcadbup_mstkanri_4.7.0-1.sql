\set ON_ERROR_STOP

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB23','R-040700-1-20111001-1','R-040700-1-20111001-1','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB23');
