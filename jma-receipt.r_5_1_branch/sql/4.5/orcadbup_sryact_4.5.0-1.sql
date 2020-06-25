-- tbl_sryact --

\set ON_ERROR_STOP
begin;
ALTER TABLE tbl_sryact DROP CONSTRAINT tbl_sryact_primary_key;
ALTER TABLE tbl_sryact RENAME TO wrk_sryact;
CREATE TABLE tbl_sryact (
nyugaikbn character(1) NOT NULL,
ptid bigint NOT NULL,
sryka character(2) NOT NULL,
sryym character(6) NOT NULL,
zainum integer NOT NULL,
rennum smallint NOT NULL,
srysyukbn character(3),
srykbn character(2),
jihimoneytotal integer DEFAULT 0,
srycd1 character(9),
srysuryo1 numeric(10,5) DEFAULT 0,
srykaisu1 smallint DEFAULT 0,
meiskyflg1 character(1),
autokbn1 character(1),
inputnum1 smallint DEFAULT 0,
jihimoney1 integer DEFAULT 0,
srycd2 character(9),
srysuryo2 numeric(10,5) DEFAULT 0,
srykaisu2 smallint DEFAULT 0,
meiskyflg2 character(1),
autokbn2 character(1),
inputnum2 smallint DEFAULT 0,
jihimoney2 integer DEFAULT 0,
srycd3 character(9),
srysuryo3 numeric(10,5) DEFAULT 0,
srykaisu3 smallint DEFAULT 0,
meiskyflg3 character(1),
autokbn3 character(1),
inputnum3 smallint DEFAULT 0,
jihimoney3 integer DEFAULT 0,
srycd4 character(9),
srysuryo4 numeric(10,5) DEFAULT 0,
srykaisu4 smallint DEFAULT 0,
meiskyflg4 character(1),
autokbn4 character(1),
inputnum4 smallint DEFAULT 0,
jihimoney4 integer DEFAULT 0,
srycd5 character(9),
srysuryo5 numeric(10,5) DEFAULT 0,
srykaisu5 smallint DEFAULT 0,
meiskyflg5 character(1),
autokbn5 character(1),
inputnum5 smallint DEFAULT 0,
jihimoney5 integer DEFAULT 0,
termid character varying(16),
opid character varying(16),
creymd character(8),
upymd character(8),
uphms character(6),
hospnum smallint NOT NULL
);
INSERT INTO tbl_sryact SELECT * FROM wrk_sryact;
ALTER TABLE ONLY tbl_sryact
ADD CONSTRAINT tbl_sryact_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, sryka, sryym, zainum, rennum);
DROP TABLE wrk_sryact;
commit;
