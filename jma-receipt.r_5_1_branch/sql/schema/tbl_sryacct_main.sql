CREATE TABLE tbl_sryacct_main (
    hospnum smallint NOT NULL,
    nyugaikbn character(1) NOT NULL,
    ptid bigint NOT NULL,
    sryka character(2) NOT NULL,
    sryym character(6) NOT NULL,
    srykbn character(2) NOT NULL,
    zainum integer NOT NULL,
    hkncombi smallint DEFAULT 0,
    jihokbn character(1),
    zaiten integer DEFAULT 0,
    srycdtotal bigint DEFAULT 0,
    suryoutotal numeric(12,5) DEFAULT 0,
    meisaisu integer DEFAULT 0,
    zaikaisu integer DEFAULT 0,
    syuten1 integer DEFAULT 0,
    syuten2 integer DEFAULT 0,
    ykzten integer DEFAULT 0,
    kizaiten integer DEFAULT 0,
    drcd character(5),
    zaireqflg character(1),
    teigenrate smallint DEFAULT 0,
    rennummax smallint DEFAULT 0,
    day_1 smallint DEFAULT 0,
    day_2 smallint DEFAULT 0,
    day_3 smallint DEFAULT 0,
    day_4 smallint DEFAULT 0,
    day_5 smallint DEFAULT 0,
    day_6 smallint DEFAULT 0,
    day_7 smallint DEFAULT 0,
    day_8 smallint DEFAULT 0,
    day_9 smallint DEFAULT 0,
    day_10 smallint DEFAULT 0,
    day_11 smallint DEFAULT 0,
    day_12 smallint DEFAULT 0,
    day_13 smallint DEFAULT 0,
    day_14 smallint DEFAULT 0,
    day_15 smallint DEFAULT 0,
    day_16 smallint DEFAULT 0,
    day_17 smallint DEFAULT 0,
    day_18 smallint DEFAULT 0,
    day_19 smallint DEFAULT 0,
    day_20 smallint DEFAULT 0,
    day_21 smallint DEFAULT 0,
    day_22 smallint DEFAULT 0,
    day_23 smallint DEFAULT 0,
    day_24 smallint DEFAULT 0,
    day_25 smallint DEFAULT 0,
    day_26 smallint DEFAULT 0,
    day_27 smallint DEFAULT 0,
    day_28 smallint DEFAULT 0,
    day_29 smallint DEFAULT 0,
    day_30 smallint DEFAULT 0,
    day_31 smallint DEFAULT 0,
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6)
);

ALTER TABLE ONLY tbl_sryacct_main
    ADD CONSTRAINT tbl_sryacct_main_primary_key PRIMARY KEY (hospnum, ptid, zainum);

CREATE INDEX idx_sryacct_main_ptid_sryym ON tbl_sryacct_main USING btree (hospnum, nyugaikbn, ptid, sryym);

