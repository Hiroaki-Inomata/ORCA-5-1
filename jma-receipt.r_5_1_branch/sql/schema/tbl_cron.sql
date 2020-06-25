CREATE TABLE tbl_cron (
    cronid numeric(4,0) NOT NULL,
    run character(1),
    name character varying(30),
    crnmin character(2),
    crnhour character(2),
    crndom character(2),
    crnmon character(2),
    crndow character(1),
    script character varying(100),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_cron
    ADD CONSTRAINT tbl_cron_primary_key PRIMARY KEY (cronid);

