CREATE TABLE tbl_opecom (
    ptid numeric(10,0) NOT NULL,
    sryymd character(8) NOT NULL,
    srycd character(9) NOT NULL,
    rennum numeric(3,0) NOT NULL,
    opename character varying(100),
    opecomment character varying(100),
    nyugaikbn character(1),
    sryka character(2),
    hknnum character(3),
    hknkbn character(1),
    hknid numeric(10,0),
    hkncombi numeric(4,0),
    tekstym character(6),
    tekedym character(6),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_opecom
    ADD CONSTRAINT tbl_opecom_primary_key PRIMARY KEY (hospnum, ptid, sryymd, srycd, rennum);

