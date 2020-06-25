
CREATE TABLE tbl_hknjainf_care (
    hknjanum character varying(8) NOT NULL,
    hknjaname character varying(200),
    post character varying(7),
    adrs character varying(200),
    banti character varying(200),
    tel character varying(15),
    idokbn character varying(1),
    idoymd character varying(8),
    termid character varying(16),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);

ALTER TABLE ONLY tbl_hknjainf_care
    ADD CONSTRAINT tbl_hknjainf_care_primary_key PRIMARY KEY (hknjanum);
