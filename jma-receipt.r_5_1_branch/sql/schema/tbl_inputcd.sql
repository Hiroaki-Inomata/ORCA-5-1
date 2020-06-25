CREATE TABLE tbl_inputcd (
    cdsyu character(1) NOT NULL,
    inputcd character(20) NOT NULL,
    srykbn character(2),
    srycd character(9) NOT NULL,
    dspseq numeric(2,0) DEFAULT 0,
    dspname character varying(200),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_inputcd
    ADD CONSTRAINT tbl_inputcd_primary_key PRIMARY KEY (hospnum, cdsyu, inputcd);

CREATE INDEX idx_inputcd_key2 ON tbl_inputcd USING btree (srycd, hospnum);

