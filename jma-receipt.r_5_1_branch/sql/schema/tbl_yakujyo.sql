CREATE TABLE tbl_yakujyo (
    srycd character(9) NOT NULL,
    teknum numeric(2,0) DEFAULT 0 NOT NULL,
    name character varying(170),
    effect character varying(2000),
    caution character varying(4000),
    photo character varying(256),
    color character varying(360),
    shape character varying(360),
    mark character varying(360),
    termid character varying(32),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_yakujyo
    ADD CONSTRAINT tbl_yakujyo_primary_key PRIMARY KEY (hospnum, srycd, teknum);

