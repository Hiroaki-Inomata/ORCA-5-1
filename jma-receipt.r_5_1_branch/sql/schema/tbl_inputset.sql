CREATE TABLE tbl_inputset (
    setcd character(6) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    setseq smallint NOT NULL,
    inputcd character(9),
    suryo1 numeric(12,5) DEFAULT 0,
    suryo2 numeric(12,5) DEFAULT 0,
    kaisu smallint DEFAULT 0,
    coment character varying(80),
    atai1 character varying(8),
    atai2 character varying(8),
    atai3 character varying(8),
    atai4 character varying(8),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum smallint NOT NULL,
    inputkbn character(1),
    kansuryo numeric(10,5) DEFAULT 0,
    atai5 character varying(8)
);

ALTER TABLE ONLY tbl_inputset
    ADD CONSTRAINT tbl_inputset_primary_key PRIMARY KEY (hospnum, setcd, yukostymd, yukoedymd, setseq);

