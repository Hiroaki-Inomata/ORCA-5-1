CREATE TABLE tbl_santeiplus (
    hospnum smallint NOT NULL,
    ptid bigint DEFAULT 0 NOT NULL,
    sryym character(6) NOT NULL,
    srycd character(9) NOT NULL,
    nyugaikbn character(1) NOT NULL,
    sryka character(2) NOT NULL,
    hkncombinum smallint NOT NULL,
    daykey smallint NOT NULL,
    rennum smallint NOT NULL,
    comment character varying(80),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_santeiplus
    ADD CONSTRAINT tbl_santeiplus_primary_key PRIMARY KEY (hospnum, ptid, sryym, srycd, nyugaikbn, sryka, hkncombinum, daykey, rennum);

