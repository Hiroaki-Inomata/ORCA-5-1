CREATE TABLE tbl_dbkanri (
    kanricd character(8) NOT NULL,
    version character varying(64),
    dbsversion1 character varying(64),
    dbsversion2 character varying(64),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_dbkanri
    ADD CONSTRAINT tbl_dbkanri_primary_key PRIMARY KEY (kanricd);

