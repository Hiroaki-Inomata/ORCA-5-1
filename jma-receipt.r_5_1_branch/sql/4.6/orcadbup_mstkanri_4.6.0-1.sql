\set ON_ERROR_STOP

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB17','R-040500-1-20110101-1','R-040500-1-20110101-1','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB17');

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB18','R-040500-1-20110315-1','R-040500-1-20110315-1','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB18');

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB19','R-040500-1-20110315-1','R-040500-1-20110315-1','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB19');

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB20','R-040500-1-20110315-1','R-040500-1-20110315-1','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB20');

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB21','R-040600-1-20101027-1','R-040600-1-20101027-1','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB21');

INSERT INTO tbl_mstkanri
SELECT 1,'ORCADB22','R-040600-1-20110311-1','R-040600-1-20110311-1','','','',
to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss')
WHERE NOT EXISTS (SELECT 1 FROM tbl_mstkanri WHERE hospnum = 1 AND kanricd = 'ORCADB22');
