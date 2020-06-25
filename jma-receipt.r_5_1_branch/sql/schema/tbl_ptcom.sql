CREATE TABLE tbl_ptcom (
    ptid bigint NOT NULL,
    zainum integer NOT NULL,
    srycd character(9) NOT NULL,
    rennum smallint NOT NULL,
    inputcoment character varying(80),
    inputchi1 character varying(8),
    inputchi2 character varying(8),
    inputchi3 character varying(8),
    inputchi4 character varying(8),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum smallint NOT NULL,
    inputchi5 character varying(8)
);

ALTER TABLE ONLY tbl_ptcom
    ADD CONSTRAINT tbl_ptcom_primary_key PRIMARY KEY (hospnum, ptid, zainum, srycd, rennum);

