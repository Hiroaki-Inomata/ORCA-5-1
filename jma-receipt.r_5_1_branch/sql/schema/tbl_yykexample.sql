CREATE TABLE tbl_yykexample (
    hospnum smallint NOT NULL,
    rennum smallint NOT NULL,
    hyokbn smallint NOT NULL,
    comment character varying(100),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_yykexample
    ADD CONSTRAINT tbl_yykexample_primary_key PRIMARY KEY (hospnum, rennum);

