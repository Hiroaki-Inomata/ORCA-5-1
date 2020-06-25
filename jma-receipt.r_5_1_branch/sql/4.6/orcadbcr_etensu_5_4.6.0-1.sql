\set ON_ERROR_STOP
--
-- 電子点数表　算定回数テーブル
-- Create Date : 2011/01/17
--

CREATE TABLE tbl_etensu_5 (
    srycd character varying(9) NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    tanicd smallint DEFAULT 0,
    taniname character varying(6),
    kaisu smallint DEFAULT 0,
    tokurei smallint DEFAULT 0,
    chgymd character varying(8)
);

ALTER TABLE ONLY tbl_etensu_5
    ADD CONSTRAINT tbl_etensu_5_primary_key PRIMARY KEY (srycd,yukostymd,yukoedymd);
