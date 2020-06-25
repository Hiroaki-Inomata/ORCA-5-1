CREATE TABLE tbl_rece_putb (
    prefkbn character(1) NOT NULL,
    syubetu_srt1 integer NOT NULL,
    grp_no1 character(3) NOT NULL,
    syubetu_srt2 integer NOT NULL,
    grp_no2 character(3) NOT NULL,
    syubetu_gai character(4),
    syubetu_nyu character(4),
    syubetu_under_no1 character(2),
    syubetu_under_no2 character(2),
    kouhi_no numeric(3,0) NOT NULL,
    kouhi_num character(3),
    styukymd character(8) NOT NULL,
    edyukymd character(8) NOT NULL,
    termid character varying(64),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    syubetu_gai_srt1 character(4),
    syubetu_nyu_srt1 character(4),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_rece_putb
    ADD CONSTRAINT tbl_rece_putb_primary_key PRIMARY KEY (hospnum, prefkbn, syubetu_srt1, grp_no1, syubetu_srt2, grp_no2, kouhi_no, styukymd, edyukymd);

