CREATE TABLE tbl_ptsame (
    hospnum smallint NOT NULL,
    ptid bigint NOT NULL,
    same_hospnum smallint NOT NULL,
    same_ptid bigint NOT NULL,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_ptsame
    ADD CONSTRAINT tbl_ptsame_primary_key PRIMARY KEY (hospnum, ptid);

