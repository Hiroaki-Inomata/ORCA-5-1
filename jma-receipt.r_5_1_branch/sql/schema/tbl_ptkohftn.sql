CREATE TABLE tbl_ptkohftn (
    ptid numeric(10,0) NOT NULL,
    kohid numeric(10,0) NOT NULL,
    rennum numeric(3,0) NOT NULL,
    tekstymd character(8),
    tekedymd character(8),
    kohnum character(3),
    paykbn character(2),
    gaijgngak numeric(7,0),
    nyujgngak numeric(7,0),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_ptkohftn
    ADD CONSTRAINT tbl_ptkohftn_primary_key PRIMARY KEY (hospnum, ptid, kohid, rennum);

