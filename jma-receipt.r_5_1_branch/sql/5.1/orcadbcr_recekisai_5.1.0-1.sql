\set ON_ERROR_STOP
--
-- レセプト記載事項
--
-- Create Date : 2018/ 8/28
--
CREATE TABLE master.tbl_recekisai (
    itemno smallint DEFAULT 0 NOT NULL,
    edano smallint DEFAULT 0 NOT NULL,
    stym character varying(6) NOT NULL,
    edym character varying(6) NOT NULL,
    srycd character varying(9) NOT NULL,
    rennum smallint DEFAULT 0 NOT NULL,
    c_cd character varying(9) NOT NULL,
    jyotaicd character varying(3),
    keisaikbn smallint DEFAULT 0,
    kubun character varying(64),
    jyoken smallint DEFAULT 0,
    hisantei smallint DEFAULT 0,
    syorikbn smallint DEFAULT 0,
    meisyoetc character varying(300),
    chgymd character varying(8),
    haisiymd character varying(8),
    creymd character varying(8),
    upymd character varying(8),
    uphms character varying(6)
);

ALTER TABLE ONLY master.tbl_recekisai
    ADD CONSTRAINT tbl_recekisai_primary_key PRIMARY KEY (itemno, edano, stym, edym, srycd, rennum);

CREATE VIEW tbl_recekisai AS SELECT * FROM master.tbl_recekisai;
