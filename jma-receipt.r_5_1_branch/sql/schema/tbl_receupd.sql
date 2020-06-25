CREATE TABLE tbl_receupd (
    ptid numeric(10,0) NOT NULL,
    nyugaikbn character(1) NOT NULL,
    sryka character(2) NOT NULL,
    sryym character(6) NOT NULL,
    hknkbn character(1) NOT NULL,
    hknkbn2 character(1) NOT NULL,
    receymd character(8),
    onlupd character(1),
    onlupd1 character(1),
    onlupd2 character(1),
    onlupd3 character(1),
    onlupd4 character(1),
    onlupd5 character(1),
    onlupd6 character(1),
    onlupd7 character(1),
    onlupd8 character(1),
    onlupd9 character(1),
    onlupd10 character(1),
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_receupd
    ADD CONSTRAINT tbl_receupd_primary_key PRIMARY KEY (hospnum, ptid, nyugaikbn, sryka, sryym, hknkbn, hknkbn2);

CREATE INDEX idx_receupd_upymd ON tbl_receupd USING btree (hospnum, upymd, nyugaikbn);

