-- tbl_inputset --

\set ON_ERROR_STOP
begin;
ALTER TABLE tbl_inputset DROP CONSTRAINT tbl_inputset_primary_key;
ALTER TABLE tbl_inputset RENAME TO wrk_inputset;
CREATE TABLE tbl_inputset (
setcd character(6) NOT NULL,
yukostymd character(8) NOT NULL,
yukoedymd character(8) NOT NULL,
setseq smallint NOT NULL,
inputcd character(9),
suryo1 numeric(12,5) DEFAULT 0,
suryo2 numeric(12,5) DEFAULT 0,
kaisu smallint DEFAULT 0,
coment character varying(80),
atai1 character varying(8),
atai2 character varying(8),
atai3 character varying(8),
atai4 character varying(8),
termid character varying(16),
opid character varying(16),
creymd character(8),
upymd character(8),
uphms character(6),
hospnum smallint NOT NULL
);
INSERT INTO tbl_inputset SELECT * FROM wrk_inputset;
ALTER TABLE ONLY tbl_inputset
ADD CONSTRAINT tbl_inputset_primary_key PRIMARY KEY (hospnum, setcd, yukostymd, yukoedymd, setseq);
DROP TABLE wrk_inputset;
commit;
