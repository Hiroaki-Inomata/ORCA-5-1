CREATE TABLE tbl_pgkanri (
    debianversion character varying(24) NOT NULL,
    systemversion character varying(10) NOT NULL,
    pgno character(3) NOT NULL,
    releaseymd character(8),
    pgname character varying(64),
    updateflg character(1),
    restartflg character(1),
    installdir character varying(256),
    comment character varying(400),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_pgkanri
    ADD CONSTRAINT tbl_pgkanri_primary_key PRIMARY KEY (debianversion, systemversion, pgno);

