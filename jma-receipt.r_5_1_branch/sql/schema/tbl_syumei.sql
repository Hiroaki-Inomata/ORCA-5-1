CREATE TABLE tbl_syumei (
    nyugaikbn character(1) NOT NULL,
    ptid bigint NOT NULL,
    denpnum integer NOT NULL,
    denpedanum smallint NOT NULL,
    nyukinrennum smallint DEFAULT 0,
    sryka character(2),
    meisaijoutaikbn character(1),
    sky_printymd character(8),
    sky_reprintymd character(8),
    ryosyu_printymd character(8),
    ryosyu_reprintymd character(8),
    skymoney integer DEFAULT 0,
    nyuhen_kbn character(1),
    nyuhen_money integer DEFAULT 0,
    nyuhen_ymd character(8),
    nyukin_hoho character(2),
    joutaikbn character(1),
    dailykey character(12),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    crehms character(6),
    upymd character(8),
    uphms character(6),
    syurrkupdkbn character(1),
    hospnum smallint NOT NULL,
    syurrknum smallint,
    syuedanum smallint
);

ALTER TABLE ONLY tbl_syumei
    ADD CONSTRAINT tbl_syumei_primary_key PRIMARY KEY (hospnum, nyugaikbn, ptid, denpnum, denpedanum);

