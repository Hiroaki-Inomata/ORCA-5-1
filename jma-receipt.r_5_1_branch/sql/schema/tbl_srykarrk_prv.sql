CREATE TABLE tbl_srykarrk_prv (
    ptid numeric(10,0) NOT NULL,
    sryka character(2) NOT NULL,
    syosinymd1 character(8),
    syosinymd2 character(8),
    lastymd character(8),
    termid character varying(16),
    opid character varying(16),
    creymd character(8) NOT NULL,
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_srykarrk_prv
    ADD CONSTRAINT tbl_srykarrk_prv_primary_key PRIMARY KEY (hospnum, ptid, sryka, creymd);

