\set ON_ERROR_STOP

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB24','R-040500-1-20120318-1','R-040500-1-20120318-1','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB24');
