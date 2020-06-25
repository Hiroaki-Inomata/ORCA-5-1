CREATE TABLE tbl_syuka (
    nyugaikbn character(1) NOT NULL,
    ptid numeric(10,0) NOT NULL,
    ten_kbn character(1) NOT NULL,
    sryym character(6) NOT NULL,
    teisyutusaki character(1) NOT NULL,
    hknjanum character(8) NOT NULL,
    hojokbn character(1) NOT NULL,
    recesyubetu character(4) NOT NULL,
    sryka character(2) NOT NULL,
    hknnum character(3),
    sryka_input character(2),
    sryka_auto character(2),
    hkncombi_1 numeric(4,0),
    hkncombi_2 numeric(4,0),
    hkncombi_3 numeric(4,0),
    hkncombi_4 numeric(4,0),
    hkncombi_5 numeric(4,0),
    hkncombi_6 numeric(4,0),
    hkncombi_7 numeric(4,0),
    hkncombi_8 numeric(4,0),
    hkncombi_9 numeric(4,0),
    hkncombi_10 numeric(4,0),
    tensu numeric(9,0),
    kaisu numeric(3,0),
    nissu numeric(3,0),
    edday numeric(2,0),
    miseikyu_flg character(2),
    dlt_flg character(1),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL,
    kohid bigint DEFAULT 0 NOT NULL
);

ALTER TABLE ONLY tbl_syuka
    ADD CONSTRAINT tbl_syuka_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, ten_kbn, sryym, teisyutusaki, hknjanum, hojokbn, kohid, recesyubetu, sryka);

CREATE INDEX idx_syuka_sryym ON tbl_syuka USING btree (hospnum, nyugaikbn, sryym);

