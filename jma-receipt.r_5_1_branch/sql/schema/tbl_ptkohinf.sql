CREATE TABLE tbl_ptkohinf (
    ptid bigint NOT NULL,
    kohid bigint NOT NULL,
    kohnum character(3),
    paykbn character(2),
    ftnjanum character(8),
    jkysnum character(20),
    tekstymd character(8),
    tekedymd character(8),
    kakuninymd character(8),
    sakjokbn character(1),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum smallint NOT NULL,
    tekedchkkbn character(1)
);

ALTER TABLE ONLY tbl_ptkohinf
    ADD CONSTRAINT tbl_ptkohinf_primary_key PRIMARY KEY (hospnum, ptid, kohid);

