CREATE TABLE tbl_teikikanri (
    btunum character(2) NOT NULL,
    syokbn character(1),
    creymd character(8),
    crehms character(6),
    sryym character(6),
    page numeric(5,0),
    kensu numeric(5,0),
    termid character varying(16),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_teikikanri
    ADD CONSTRAINT tbl_teikikanri_primary_key PRIMARY KEY (hospnum, btunum);

