CREATE TABLE tbl_qrcdhkn (
    registymd character(8) NOT NULL,
    registid numeric(5,0) NOT NULL,
    registtime character(6),
    seidokbn character(3),
    hknjanum character(8),
    kigonum character varying(40),
    honkzkkbn character(1),
    kananame character varying(100),
    sex character(1),
    birthday character(7),
    kanrinum character varying(100),
    name character varying(100),
    kyurate character varying(2),
    saihakku character varying(50),
    ptid numeric(10,0),
    kigo character varying(80),
    num character varying(80),
    birthdays character(8),
    errcd character(2),
    mod_flg numeric(1,0),
    mod_flg2 numeric(1,0),
    delkbn character(1),
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_qrcdhkn
    ADD CONSTRAINT tbl_qrcdhkn_primary_key PRIMARY KEY (hospnum, registymd, registid);

