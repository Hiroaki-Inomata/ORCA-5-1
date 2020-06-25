CREATE TABLE tbl_btpara (
    shellid character(8) NOT NULL,
    run_opid character(16) NOT NULL,
    dsp_rennum numeric(3,0) DEFAULT 0 NOT NULL,
    rennum numeric(4,0) DEFAULT 0 NOT NULL,
    jobid numeric(3,0),
    scriptid character(20),
    info_kbn character(2),
    info_para character varying(500),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_btpara
    ADD CONSTRAINT tbl_btpara_primary_key PRIMARY KEY (hospnum, shellid, run_opid, dsp_rennum, rennum);

