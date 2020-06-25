CREATE TABLE tbl_nrrksrh (
    hospnum smallint NOT NULL,
    ptid bigint NOT NULL,
    sryymd character(8) NOT NULL,
    zainum integer NOT NULL,
    rrknum smallint NOT NULL,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_nrrksrh
    ADD CONSTRAINT tbl_nrrksrh_primary_key PRIMARY KEY (hospnum, ptid, sryymd, zainum, rrknum);

