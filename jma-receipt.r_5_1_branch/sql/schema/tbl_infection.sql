CREATE TABLE tbl_infection (
    hospnum smallint DEFAULT 0 NOT NULL,
    sendymd character(8) NOT NULL,
    ptid bigint DEFAULT 0 NOT NULL,
    disease smallint DEFAULT 0 NOT NULL,
    sryymd character(8) NOT NULL,
    srycd character varying(9),
    medsrycd character varying(9),
    syosai smallint DEFAULT 0,
    syosinymd character varying(8),
    byomeicd character varying(7),
    byosryymd character varying(8),
    tenkikbn character varying(1),
    tenkiymd character varying(8),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_infection
    ADD CONSTRAINT tbl_infection_primary_key PRIMARY KEY (hospnum, sendymd, ptid, disease, sryymd);

