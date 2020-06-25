CREATE TABLE tbl_kyuseirrk (
    ptid numeric(10,0) NOT NULL,
    chgymd character(8) NOT NULL,
    kananame character varying(100),
    name character varying(100),
    nickname character varying(100),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_kyuseirrk
    ADD CONSTRAINT tbl_kyuseirrk_primary_key PRIMARY KEY (hospnum, ptid, chgymd);

