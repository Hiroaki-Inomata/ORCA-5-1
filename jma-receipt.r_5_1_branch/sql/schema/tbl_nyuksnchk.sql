CREATE TABLE tbl_nyuksnchk (
    nyuinkbn character(3) NOT NULL,
    ksnkbn character(3) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    chkkbn character(1),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_nyuksnchk
    ADD CONSTRAINT tbl_nyuksnchk_primary_key PRIMARY KEY (nyuinkbn, ksnkbn, yukostymd, yukoedymd);

