CREATE TABLE tbl_jinmei (
    kanji character varying(50) NOT NULL,
    kana character varying(50) NOT NULL,
    upymd character varying(8) NOT NULL,
    uphms character varying(6) NOT NULL,
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_jinmei
    ADD CONSTRAINT tbl_jinmei_primary_key PRIMARY KEY (hospnum, kanji, kana);

CREATE INDEX idx_jinmei_kana ON tbl_jinmei USING btree (hospnum, kana);

