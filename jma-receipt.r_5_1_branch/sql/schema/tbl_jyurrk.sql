CREATE TABLE tbl_jyurrk (
    ptid bigint NOT NULL,
    nyugaikbn character(1) NOT NULL,
    sryka character(2) NOT NULL,
    sryymd character(8) NOT NULL,
    rennum smallint NOT NULL,
    douji_rennum smallint NOT NULL,
    kaikei_rennum smallint NOT NULL,
    edanum smallint NOT NULL,
    drcd character(5),
    hkncombinum character(4),
    hknkbn character(1),
    denpnum integer,
    grp_denpnum integer DEFAULT 0,
    grp_rennum smallint DEFAULT 0,
    syu_updkbn character(1) DEFAULT ' '::bpchar,
    srykbn1 character(2),
    srykbn2 character(2),
    srykbn3 character(2),
    srykbn4 character(2),
    srykbn5 character(2),
    srykbn6 character(2),
    srykbn7 character(2),
    srykbn8 character(2),
    srykbn9 character(2),
    srykbn10 character(2),
    srykbn11 character(2),
    zainum1 integer,
    zainum2 integer,
    zainum3 integer,
    zainum4 integer,
    zainum5 integer,
    zainum6 integer,
    zainum7 integer,
    zainum8 integer,
    zainum9 integer,
    zainum10 integer,
    zainum11 integer,
    zainum12 integer,
    zainum13 integer,
    zainum14 integer,
    zainum15 integer,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum smallint NOT NULL
);

ALTER TABLE ONLY tbl_jyurrk
    ADD CONSTRAINT tbl_jyurrk_primary_key PRIMARY KEY (hospnum, ptid, nyugaikbn, sryka, sryymd, rennum, douji_rennum, kaikei_rennum, edanum);

CREATE INDEX idx_jyurrk_sryymd ON tbl_jyurrk USING btree (hospnum, sryymd, nyugaikbn);

CREATE INDEX idx_jyurrk_upymd ON tbl_jyurrk USING btree (hospnum, upymd, nyugaikbn);

