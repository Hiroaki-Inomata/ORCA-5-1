CREATE TABLE tbl_kohplus (
    prefnum character(2) NOT NULL,
    citynum character(3) NOT NULL,
    kohnum character(3) NOT NULL,
    ptkbn character(1) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8),
    ftnkbn character(1),
    nyuskjryykbn character(1),
    receskykbn character(1),
    seidoname character varying(100),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_kohplus
    ADD CONSTRAINT tbl_kohplus_primary_key PRIMARY KEY (hospnum, prefnum, citynum, kohnum, ptkbn, yukostymd);

