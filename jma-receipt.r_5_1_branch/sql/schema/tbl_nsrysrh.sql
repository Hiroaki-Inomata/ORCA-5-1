CREATE TABLE tbl_nsrysrh (
    hospnum smallint NOT NULL,
    ptid bigint NOT NULL,
    zainum integer NOT NULL,
    sryym character(6) NOT NULL,
    sryka character(2),
    hkncombinum character(4),
    srykbn character(2),
    srycd character(9),
    brmnum character(8),
    rm_sagaku character(2),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    srysuryo numeric(10,5)
);

ALTER TABLE ONLY tbl_nsrysrh
    ADD CONSTRAINT tbl_nsrysrh_primary_key PRIMARY KEY (hospnum, ptid, zainum, sryym);

