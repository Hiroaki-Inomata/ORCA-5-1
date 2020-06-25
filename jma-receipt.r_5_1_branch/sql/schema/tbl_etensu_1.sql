CREATE TABLE tbl_etensu_1 (
    srycd character varying(9) NOT NULL,
    yukostymd character varying(8) NOT NULL,
    yukoedymd character varying(8) NOT NULL,
    h_tani1 smallint DEFAULT 0,
    h_group1 character varying(7),
    h_tani2 smallint DEFAULT 0,
    h_group2 character varying(7),
    h_tani3 smallint DEFAULT 0,
    h_group3 character varying(7),
    r_day smallint DEFAULT 0,
    r_month smallint DEFAULT 0,
    r_same smallint DEFAULT 0,
    r_week smallint DEFAULT 0,
    n_group smallint DEFAULT 0,
    c_kaisu smallint DEFAULT 0,
    chgymd character varying(8)
);

ALTER TABLE ONLY tbl_etensu_1
    ADD CONSTRAINT tbl_etensu_1_primary_key PRIMARY KEY (srycd, yukostymd, yukoedymd);

