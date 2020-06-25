CREATE TABLE tbl_etensu_2_jma (
    h_group character varying(7) NOT NULL,
    srycd character varying(9) NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    chgymd character varying(8)
);

ALTER TABLE ONLY tbl_etensu_2_jma
    ADD CONSTRAINT tbl_etensu_2_jma_primary_key PRIMARY KEY (h_group, srycd, yukostymd, yukoedymd);

