CREATE TABLE tbl_pttainfuka (
    ptid numeric(10,0) NOT NULL,
    rrknum numeric(3,0) NOT NULL,
    byomei1 character varying(80),
    byomei2 character varying(80),
    byomei3 character varying(80),
    byomei4 character varying(80),
    byomei5 character varying(80),
    khnryo character varying(80),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_pttainfuka
    ADD CONSTRAINT tbl_pttainfuka_primary_key PRIMARY KEY (hospnum, ptid, rrknum);

