CREATE TABLE tbl_syskanri (
    kanricd character(4) NOT NULL,
    kbncd character(8) NOT NULL,
    styukymd character(8) NOT NULL,
    edyukymd character(8) NOT NULL,
    kanritbl character varying(500),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_syskanri
    ADD CONSTRAINT tbl_syskanri_primary_key PRIMARY KEY (hospnum, kanricd, kbncd, styukymd, edyukymd);

