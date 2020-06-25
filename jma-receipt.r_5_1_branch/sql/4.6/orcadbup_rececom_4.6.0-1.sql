-- tbl_rececom --

\set ON_ERROR_STOP
begin;
ALTER TABLE tbl_rececom DROP CONSTRAINT tbl_rececom_primary_key;
ALTER TABLE tbl_rececom RENAME TO wrk_rececom;
CREATE TABLE tbl_rececom (
ptid bigint NOT NULL,
nyugaikbn character(1) DEFAULT '0' NOT NULL,
sryka character(2) NOT NULL,
sryym character(6) NOT NULL,
hkncombi smallint NOT NULL,
srydd character(2) NOT NULL,
sjkbn character(2) NOT NULL,
comment character varying(4800),
termid character varying(16),
opid character varying(16),
creymd character(8),
upymd character(8),
uphms character(6),
hospnum smallint NOT NULL
);
INSERT INTO tbl_rececom SELECT * FROM wrk_rececom;
alter table tbl_rececom add column rennum smallint;
update tbl_rececom set rennum = 1;
alter table tbl_rececom alter column rennum set not null;
ALTER TABLE ONLY tbl_rececom
ADD CONSTRAINT tbl_rececom_primary_key PRIMARY KEY (hospnum, ptid, nyugaikbn, sryka, sryym, hkncombi, srydd, sjkbn, rennum);
DROP TABLE wrk_rececom;
commit;
