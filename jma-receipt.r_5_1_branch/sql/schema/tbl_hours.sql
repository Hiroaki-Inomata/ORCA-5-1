CREATE TABLE tbl_hours (
    sryymd character(8) NOT NULL,
    "time" character(4) NOT NULL,
    id character(12),
    hourskbn character(1),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_hours
    ADD CONSTRAINT tbl_hours_primary_key PRIMARY KEY (hospnum, sryymd, "time");

