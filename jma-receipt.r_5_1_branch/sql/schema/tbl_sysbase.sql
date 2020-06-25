CREATE TABLE tbl_sysbase (
    grpnum numeric(2,0) NOT NULL,
    hospnum numeric(2,0) NOT NULL,
    hospname character varying(120),
    kigen character(8) DEFAULT '99999999'::bpchar,
    honbungrp numeric(2,0) DEFAULT 0,
    honbunkbn character(1) DEFAULT '0'::bpchar,
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_sysbase
    ADD CONSTRAINT tbl_sysbase_primary_key PRIMARY KEY (grpnum);

