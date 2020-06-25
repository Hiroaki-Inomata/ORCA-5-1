CREATE TABLE tbl_chkreigai (
    hospnum smallint NOT NULL,
    ptid bigint NOT NULL,
    rennum integer NOT NULL,
    chknum smallint,
    errkbn character(4),
    nyugaikbn character(1),
    srycd character(9),
    styukymd character(8),
    edyukymd character(8),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_chkreigai
    ADD CONSTRAINT tbl_chkreigai_primary_key PRIMARY KEY (hospnum, ptid, rennum);

