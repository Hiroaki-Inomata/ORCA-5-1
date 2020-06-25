-- tbl_ptnum --

\set ON_ERROR_STOP
DROP INDEX idx_ptnum_ptnum;
ALTER TABLE tbl_ptnum DROP CONSTRAINT tbl_ptnum_primary_key;
ALTER TABLE tbl_ptnum RENAME TO wrk_ptnum;
CREATE TABLE tbl_ptnum (
ptid bigint NOT NULL,
ptnum character(20) NOT NULL,
hknid bigint DEFAULT 0,
kohid bigint DEFAULT 0,
autocombinum smallint DEFAULT 0,
manucombinum smallint DEFAULT 0,
tstptnumkbn character(1),
termid character varying(16),
opid character varying(16),
creymd character(8),
upymd character(8),
uphms character(6),
hospnum smallint NOT NULL
);
INSERT INTO tbl_ptnum SELECT * FROM wrk_ptnum;
ALTER TABLE ONLY tbl_ptnum
ADD CONSTRAINT tbl_ptnum_primary_key PRIMARY KEY (hospnum, ptid);
CREATE INDEX idx_ptnum_ptnum ON tbl_ptnum USING btree (hospnum, ptnum);
DROP TABLE wrk_ptnum;
