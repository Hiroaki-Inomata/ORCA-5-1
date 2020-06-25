CREATE TABLE tbl_ptteikirrk (
    ptid numeric(10,0) NOT NULL,
    sryym character(6) NOT NULL,
    sakkbn character(1) NOT NULL,
    hkncombinum numeric(4,0) NOT NULL,
    denpnum numeric(7,0) NOT NULL,
    delflg numeric(1,0),
    syokbn character(1),
    kbt character(1),
    skystymd character(8),
    skyedymd character(8),
    skyten numeric(9,0),
    skymoney_tax_sai numeric(7,0),
    skymoney numeric(9,0),
    rrknum numeric(3,0),
    denpedanum numeric(2,0),
    skyupdflg numeric(1,0),
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_ptteikirrk
    ADD CONSTRAINT tbl_ptteikirrk_primary_key PRIMARY KEY (hospnum, ptid, sryym, sakkbn, hkncombinum, denpnum);

