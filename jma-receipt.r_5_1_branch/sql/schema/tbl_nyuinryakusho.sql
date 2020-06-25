CREATE TABLE tbl_nyuinryakusho (
    srycd character(9) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    ryakusho character varying(20),
    name character varying(200),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_nyuinryakusho
    ADD CONSTRAINT tbl_nyuinryakusho_primary_key PRIMARY KEY (srycd, yukostymd, yukoedymd);

