CREATE TABLE tbl_monthlynum (
    ptid numeric(10,0) NOT NULL,
    kohnum character(3) NOT NULL,
    sryym character(6) NOT NULL,
    nyugaikbn character(1) NOT NULL,
    jkysnum character(20),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    kohid numeric(10,0) DEFAULT 0 NOT NULL,
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_monthlynum
    ADD CONSTRAINT tbl_monthlynum_primary_key PRIMARY KEY (hospnum, ptid, kohnum, kohid, sryym, nyugaikbn);

