CREATE TABLE tbl_hknjainf (
    hknjanum character(8) NOT NULL,
    hknjaname character varying(200),
    hknjaname_tan1 character varying(100),
    hknjaname_tan2 character varying(40),
    hknjaname_tan3 character varying(40),
    hknnum character(3),
    hon_gaikyurate numeric(3,0) DEFAULT 0,
    hon_nyukyurate numeric(3,0) DEFAULT 0,
    kzk_gaikyurate numeric(3,0) DEFAULT 0,
    kzk_nyukyurate numeric(3,0) DEFAULT 0,
    post character varying(7),
    adrs character varying(200),
    banti character varying(200),
    tel character varying(15),
    kigo character varying(80),
    idokbn character varying(1),
    idoymd character varying(8),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_hknjainf
    ADD CONSTRAINT tbl_hknjainf_primary_key PRIMARY KEY (hospnum, hknjanum);

