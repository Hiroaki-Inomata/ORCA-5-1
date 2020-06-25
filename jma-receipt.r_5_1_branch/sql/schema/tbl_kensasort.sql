CREATE TABLE tbl_kensasort (
    knsbunrui numeric(2,0) NOT NULL,
    srycd character(9) NOT NULL,
    dspseq numeric(4,0),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_kensasort
    ADD CONSTRAINT tbl_kensasort_primary_key PRIMARY KEY (knsbunrui, srycd);

