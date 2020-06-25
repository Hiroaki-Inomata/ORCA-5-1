CREATE TABLE tbl_jobkanri (
    jobid numeric(7,0) DEFAULT 0 NOT NULL,
    shellid character(8) NOT NULL,
    run_opid character(16) NOT NULL,
    shellmsg character(100),
    termid character varying(32),
    opid character varying(16),
    pgid character(20),
    startymd character(8),
    endymd character(8),
    jobstarttime character(8),
    jobendtime character(8),
    stepstarttime character(8),
    stependtime character(8),
    param character(256),
    errcd character(4),
    syokbn character(1),
    sryym character(6),
    nyugaikbn character(1),
    sryymd character(8),
    ptid numeric(10,0),
    updcnt numeric(10,0),
    stepcnt numeric(3,0) DEFAULT 0,
    stopflg numeric(1,0) DEFAULT 0,
    pid1 numeric(5,0) DEFAULT 0,
    pid2 numeric(5,0) DEFAULT 0,
    pid3 numeric(5,0) DEFAULT 0,
    pid4 numeric(5,0) DEFAULT 0,
    yobi character varying(500),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_jobkanri
    ADD CONSTRAINT tbl_jobkanri_primary_key PRIMARY KEY (hospnum, jobid, shellid, run_opid);

