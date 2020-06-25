CREATE TABLE tbl_tnkrrk (
    ptid bigint NOT NULL,
    stymd character(8) NOT NULL,
    edymd character(8) NOT NULL,
    rri_fukushi_kbn character(1) NOT NULL,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum smallint NOT NULL,
    skjninstymd character(8),
    hkntekkbn character(1)
);

ALTER TABLE ONLY tbl_tnkrrk
    ADD CONSTRAINT tbl_tnkrrk_primary_key PRIMARY KEY (hospnum, ptid, stymd);

