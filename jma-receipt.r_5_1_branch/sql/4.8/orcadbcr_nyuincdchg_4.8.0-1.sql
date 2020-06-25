DROP TABLE if exists tbl_nyuincdchg;
CREATE TABLE tbl_nyuincdchg (
    srycd_bf character(9) NOT NULL,
    srycd_af character(9) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    nyuinryocd character(3) NOT NULL,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_nyuincdchg
    ADD CONSTRAINT tbl_nyuincdchg_primary_key PRIMARY KEY (srycd_bf,srycd_af,yukostymd,yukoedymd);

