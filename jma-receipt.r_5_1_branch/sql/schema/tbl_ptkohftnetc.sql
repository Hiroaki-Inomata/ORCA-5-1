CREATE TABLE tbl_ptkohftnetc (
    hospnum smallint NOT NULL,
    ptid bigint DEFAULT 0 NOT NULL,
    kohid bigint DEFAULT 0 NOT NULL,
    sryymd character(8) NOT NULL,
    rennum integer DEFAULT 0 NOT NULL,
    kohnum character(3),
    nyugaikbn character(1),
    ftnmoney integer,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_ptkohftnetc
    ADD CONSTRAINT tbl_kohftnetc_primary_key PRIMARY KEY (hospnum, ptid, kohid, sryymd, rennum);

