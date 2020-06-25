CREATE TABLE tbl_ptmemoinf (
    hospnum numeric(2,0) NOT NULL,
    ptid numeric(10,0) NOT NULL,
    sysymd character(8) NOT NULL,
    memokbn numeric(1,0) NOT NULL,
    rennum numeric(2,0) NOT NULL,
    sryka character(2) NOT NULL,
    memo character varying(1000),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_ptmemoinf
    ADD CONSTRAINT tbl_ptmemoinf_key PRIMARY KEY (hospnum, ptid, sysymd, memokbn, rennum, sryka);

