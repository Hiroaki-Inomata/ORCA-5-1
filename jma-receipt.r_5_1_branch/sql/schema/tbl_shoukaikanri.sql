CREATE TABLE tbl_shoukaikanri (
    hospnum numeric(2,0) NOT NULL,
    uid character(36) NOT NULL,
    rennum numeric(5,0),
    name character varying(100),
    nyugaikbn character(1),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_shoukaikanri
    ADD CONSTRAINT tbl_shoukaikanri_primary_key PRIMARY KEY (hospnum, uid);

