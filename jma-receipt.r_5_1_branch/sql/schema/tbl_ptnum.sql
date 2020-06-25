CREATE TABLE tbl_ptnum (
    ptid bigint NOT NULL,
    ptnum character(20) NOT NULL,
    hknid bigint DEFAULT 0,
    kohid bigint DEFAULT 0,
    autocombinum smallint DEFAULT 0,
    manucombinum smallint DEFAULT 0,
    tstptnumkbn character(1),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum smallint NOT NULL
);

ALTER TABLE ONLY tbl_ptnum
    ADD CONSTRAINT tbl_ptnum_primary_key PRIMARY KEY (hospnum, ptid);

CREATE INDEX idx_ptnum_ptnum ON tbl_ptnum USING btree (hospnum, ptnum);

