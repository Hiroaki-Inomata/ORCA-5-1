CREATE TABLE tbl_interact (
    drugcd character(9) NOT NULL,
    drugcd2 character(9) NOT NULL,
    syojyoucd character(7) NOT NULL,
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

CREATE INDEX idx_interact_drugcd ON tbl_interact USING btree (drugcd);

CREATE INDEX idx_interact_drugcd2 ON tbl_interact USING btree (drugcd2);

