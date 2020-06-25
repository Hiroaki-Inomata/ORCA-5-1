CREATE TABLE tbl_pttokki (
    hospnum numeric(2,0) NOT NULL,
    nyugaikbn character(1) NOT NULL,
    ptid numeric(10,0) NOT NULL,
    rennum numeric(2,0) NOT NULL,
    cd character(2),
    naiyo character(12),
    stym character(6),
    edym character(6),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_pttokki
    ADD CONSTRAINT tbl_pttokki_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, rennum);

