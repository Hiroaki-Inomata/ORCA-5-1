\set ON_ERROR_STOP
--
-- 電子点数表　検体コメントテーブル
-- Create Date : 2011/02/03
--

CREATE TABLE tbl_etensu_2_sample (
    h_group character varying(7) NOT NULL,
    srycd character varying(9) NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    rennum smallint NOT NULL,
    samplecd character varying(9),
    chgymd character varying(8)
);

ALTER TABLE ONLY tbl_etensu_2_sample
    ADD CONSTRAINT tbl_etensu_2_sample_primary_key PRIMARY KEY (h_group,srycd,yukostymd,yukoedymd,rennum);
