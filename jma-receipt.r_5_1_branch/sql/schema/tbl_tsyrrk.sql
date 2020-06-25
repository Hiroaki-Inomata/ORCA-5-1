CREATE TABLE tbl_tsyrrk (
    ptid bigint NOT NULL,
    ninstymd character(8) NOT NULL,
    ninedymd character(8),
    skjgnstymd character(8),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum smallint NOT NULL,
    skjninstymd character(8),
    hkntekkbn character(1)
);

ALTER TABLE ONLY tbl_tsyrrk
    ADD CONSTRAINT tbl_tsyrrk_primary_key PRIMARY KEY (hospnum, ptid, ninstymd);

