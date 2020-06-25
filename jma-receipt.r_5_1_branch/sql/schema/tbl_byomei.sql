CREATE TABLE tbl_byomei (
    byomeicd character(7) NOT NULL,
    byomei character varying(200) NOT NULL,
    byomeimoji smallint DEFAULT 0,
    tanbyomei character varying(200) NOT NULL,
    tanbyomeimoji smallint DEFAULT 0,
    byomeikana character varying(200) NOT NULL,
    ikosakicd character varying(7),
    tokskncd smallint DEFAULT 0,
    utagaiflg character(1),
    tandokukbn smallint DEFAULT 0,
    hknskykbn smallint DEFAULT 0,
    hyojuncd character varying(8),
    saitakukbn character(1),
    byomeichgcd character varying(4),
    icd10_1 character varying(5),
    syusaiymd character varying(8),
    chgymd character varying(8),
    haisiymd character varying(8),
    termid character varying(32),
    opid character varying(16),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6),
    icd10_2 character varying(5),
    nanbyocd smallint DEFAULT 0
);

ALTER TABLE ONLY tbl_byomei
    ADD CONSTRAINT tbl_byomei_primary_key PRIMARY KEY (byomeicd);

CREATE INDEX idx_byomei_byomei ON tbl_byomei USING btree (byomei);

CREATE INDEX idx_byomei_byomeikana ON tbl_byomei USING btree (byomeikana);

