CREATE TABLE tbl_hktsrycd (
    kanricd numeric(3,0) NOT NULL,
    rennum numeric(2,0) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    srycd character(9),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_hktsrycd
    ADD CONSTRAINT tbl_hktsrycd_pkey PRIMARY KEY (kanricd, rennum, yukostymd, yukoedymd);

