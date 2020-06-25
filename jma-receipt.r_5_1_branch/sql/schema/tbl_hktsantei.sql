CREATE TABLE tbl_hktsantei (
    kanricd numeric(3,0) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    srycd character(9) NOT NULL,
    santeikbn character(1),
    kentaikbn character(1),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_hktsantei
    ADD CONSTRAINT tbl_hktsantei_pkey PRIMARY KEY (kanricd, yukostymd, yukoedymd, srycd);

