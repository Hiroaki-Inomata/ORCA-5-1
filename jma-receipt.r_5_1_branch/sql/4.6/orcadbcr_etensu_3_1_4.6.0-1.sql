\set ON_ERROR_STOP
--
-- 電子点数表　背反関連テーブル
-- Create Date : 2011/01/17
--

CREATE TABLE tbl_etensu_3_1 (
    srycd1 character varying(9) NOT NULL,
    srycd2 character varying(9) NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    haihan smallint DEFAULT 0,
    tokurei smallint DEFAULT 0,
    chgymd character varying(8)
);

ALTER TABLE ONLY tbl_etensu_3_1
    ADD CONSTRAINT tbl_etensu_3_1_primary_key PRIMARY KEY (srycd1,srycd2,yukostymd,yukoedymd);
