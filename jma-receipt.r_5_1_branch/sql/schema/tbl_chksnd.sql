CREATE TABLE tbl_chksnd (
    chkkbn character(1) NOT NULL,
    srycd character(9) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    cdkbn character(1) NOT NULL,
    rennum numeric(3,0) NOT NULL,
    cd character varying(170),
    byomei character(160),
    compkbn character(1),
    byomeihenflg character(1),
    nyugaikbn character(1),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL,
    utagaikbn character(1)
);

ALTER TABLE ONLY tbl_chksnd
    ADD CONSTRAINT tbl_chksnd_primary_key PRIMARY KEY (hospnum, chkkbn, srycd, yukostymd, yukoedymd, cdkbn, rennum);

