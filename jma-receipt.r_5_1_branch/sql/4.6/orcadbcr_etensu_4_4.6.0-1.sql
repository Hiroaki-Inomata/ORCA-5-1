\set ON_ERROR_STOP
--
-- 電子点数表　入院基本料テーブル
-- Create Date : 2011/01/17
--

CREATE TABLE tbl_etensu_4 (
    n_group smallint NOT NULL,
    srycd character varying(9) NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    kasan smallint DEFAULT 0,
    chgymd character varying(8)
);

ALTER TABLE ONLY tbl_etensu_4
    ADD CONSTRAINT tbl_etensu_4_primary_key PRIMARY KEY (n_group,srycd,yukostymd,yukoedymd);
