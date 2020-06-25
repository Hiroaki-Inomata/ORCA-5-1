CREATE TABLE tbl_prgoption (
    hospnum numeric(2,0) NOT NULL,
    prgid character(25) NOT NULL,
    kbncd character(16) NOT NULL,
    option character varying(44000),
    addkbn character(1),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_prgoption
    ADD CONSTRAINT tbl_prgoption_primary_key PRIMARY KEY (hospnum, prgid, kbncd);

