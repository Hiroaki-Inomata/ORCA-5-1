CREATE TABLE tbl_prtkanri (
    tbl_key character(8) NOT NULL,
    rennum numeric(4,0) DEFAULT 0 NOT NULL,
    tbl_group character(14) NOT NULL,
    shori_rennum numeric(4,0) DEFAULT 0 NOT NULL,
    shellid character varying(20),
    sryym character varying(6),
    skyymd character varying(8),
    priority numeric(4,0) DEFAULT 0,
    page numeric(5,0) DEFAULT 0,
    prtflg character(1),
    title character varying(100),
    termid character varying(32),
    opid character varying(16),
    prtnm character varying(16),
    hospnum numeric(2,0) NOT NULL,
    tbl_uuid character(36) NOT NULL,
    gyoumu_cd numeric(3,0),
    creymd character varying(8)
);

ALTER TABLE ONLY tbl_prtkanri
    ADD CONSTRAINT tbl_prtkanri_primary_key PRIMARY KEY (hospnum, tbl_key, rennum, tbl_group, tbl_uuid, shori_rennum);

