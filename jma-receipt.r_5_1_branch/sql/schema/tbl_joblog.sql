CREATE TABLE tbl_joblog (
    logymd character(8) NOT NULL,
    logtime character(6) NOT NULL,
    progname character varying(20) NOT NULL,
    kbnid character(1) NOT NULL,
    objectyym character(6),
    result character(1),
    errcd character(4),
    msg character varying(200),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_joblog
    ADD CONSTRAINT tbl_joblog_primary_key PRIMARY KEY (hospnum, logymd, logtime, progname, kbnid);

