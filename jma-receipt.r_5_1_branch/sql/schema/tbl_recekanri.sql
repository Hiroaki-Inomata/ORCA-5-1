CREATE TABLE tbl_recekanri (
    prtid character(4) NOT NULL,
    syokbn character(1) NOT NULL,
    creymd character(8) NOT NULL,
    crehms character(6) NOT NULL,
    teisyutusaki character(1) NOT NULL,
    sryka character(2) DEFAULT '00'::bpchar NOT NULL,
    prefkbn_srt character(2) DEFAULT '99'::bpchar NOT NULL,
    hknjanum_srt character(1) DEFAULT '9'::bpchar NOT NULL,
    hknjanum character(8) DEFAULT '99999999'::bpchar NOT NULL,
    recesyubetu character(4) NOT NULL,
    recesyubetu_srt character(3) DEFAULT '999'::bpchar NOT NULL,
    zaitaku character(1) DEFAULT '9'::bpchar NOT NULL,
    chouki character(1) DEFAULT '9'::bpchar NOT NULL,
    nyugaikbn character(1),
    sryym character(6),
    page numeric(5,0) DEFAULT 0,
    page_zaitaku numeric(5,0) DEFAULT 0,
    page_chouki numeric(5,0) DEFAULT 0,
    page_dup numeric(5,0) DEFAULT 0,
    kensu numeric(5,0) DEFAULT 0,
    recekbn character(1),
    syorisbt character(1),
    stymd character(8),
    edymd character(8),
    hseqnum numeric(8,0) DEFAULT 0,
    termid character varying(64),
    opid character varying(16),
    kougaku character(1) DEFAULT '9'::bpchar,
    page_kougaku numeric(5,0) DEFAULT 0,
    syubetu_srt1 character(4),
    prefkbn character(1),
    zaitaku_kbn character(1),
    chouki_kbn character(1),
    srt_last character(1),
    kougaku_ten numeric(7,0) DEFAULT 0,
    hospnum numeric(2,0) NOT NULL,
    prtkbn character(1)
);

ALTER TABLE ONLY tbl_recekanri
    ADD CONSTRAINT tbl_recekanri_primary_key PRIMARY KEY (hospnum, prtid, syokbn, creymd, crehms, teisyutusaki, sryka, prefkbn_srt, hknjanum_srt, hknjanum, recesyubetu, recesyubetu_srt, zaitaku, chouki);

CREATE INDEX idx_recekanri_sryym ON tbl_recekanri USING btree (hospnum, sryym);

