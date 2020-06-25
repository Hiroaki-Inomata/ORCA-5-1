CREATE TABLE tbl_yykcom (
    hospnum smallint NOT NULL,
    srynaiyo character(2) NOT NULL,
    drcd character(5) NOT NULL,
    yykymd character(8) NOT NULL,
    keyyyktime smallint NOT NULL,
    yykid smallint NOT NULL,
    rennum smallint NOT NULL,
    comment character varying(100),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_yykcom
    ADD CONSTRAINT tbl_yykcom_primary_key PRIMARY KEY (hospnum, srynaiyo, drcd, yykymd, keyyyktime, yykid, rennum);

