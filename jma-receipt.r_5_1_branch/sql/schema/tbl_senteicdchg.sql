CREATE TABLE tbl_senteicdchg (
    ipnsrycd character(9) NOT NULL,
    senteisrycd character(9) NOT NULL,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_senteicdchg
    ADD CONSTRAINT tbl_senteicdchg_primary_key PRIMARY KEY (ipnsrycd, senteisrycd);

CREATE INDEX idx_senteicdchg_senteisrycd ON tbl_senteicdchg USING btree (senteisrycd);

