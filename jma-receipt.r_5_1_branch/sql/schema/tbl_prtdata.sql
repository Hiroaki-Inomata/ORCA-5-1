CREATE TABLE tbl_prtdata (
    tbl_key character(8) NOT NULL,
    rennum numeric(4,0) DEFAULT 0 NOT NULL,
    tbl_group character(14) NOT NULL,
    shori_rennum numeric(4,0) DEFAULT 0 NOT NULL,
    shellid character varying(20),
    prtid character varying(20),
    sryym character varying(6),
    skyymd character varying(8),
    ptid numeric(10,0) DEFAULT 0,
    priority numeric(4,0) DEFAULT 0,
    page numeric(5,0) DEFAULT 0 NOT NULL,
    prtflg character(1) NOT NULL,
    sitekbn character(1) DEFAULT '0'::bpchar NOT NULL,
    prtnm character(16) DEFAULT ' '::bpchar NOT NULL,
    prtdata character varying(20000),
    hospnum numeric(2,0) NOT NULL,
    tbl_uuid character(36) NOT NULL,
    gyoumu_cd numeric(3,0),
    creymd character varying(8)
);

ALTER TABLE ONLY tbl_prtdata
    ADD CONSTRAINT tbl_prtdata_primary_key PRIMARY KEY (hospnum, tbl_key, rennum, tbl_group, tbl_uuid, shori_rennum, page);

