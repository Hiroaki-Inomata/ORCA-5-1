CREATE TABLE tbl_tensuplus (
    srycd character(9) NOT NULL,
    yukostymd character(8) NOT NULL,
    yukoedymd character(8) NOT NULL,
    shoyukoketa smallint,
    shoname character varying(200),
    saiketukbn smallint,
    inpchkkbn smallint,
    tenpuyakkbn character varying(2),
    allsuryokbn character varying(1),
    kanzantanicd character varying(3),
    kanzantaniname character varying(24),
    kanzanchi numeric(10,5),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    santeirrkkbn smallint DEFAULT 0,
    jgncnt smallint DEFAULT 0,
    jgncnt1d smallint DEFAULT 0,
    jgncnterr smallint DEFAULT 0,
    souryohenkbn smallint DEFAULT 0,
    ipnkisaikbn smallint DEFAULT 0,
    ipnkanzantanicd character varying(3),
    ipnkanzantaniname character varying(24),
    ipnkanzanchi numeric(10,5) DEFAULT 0,
    hospnum smallint NOT NULL,
    jgncntetcm smallint DEFAULT 0,
    jgncntetc smallint DEFAULT 0,
    zenknskbn smallint DEFAULT 0,
    jgncnt1w smallint DEFAULT 0
);

ALTER TABLE ONLY tbl_tensuplus
    ADD CONSTRAINT tbl_tensuplus_primary_key PRIMARY KEY (hospnum, srycd, yukostymd, yukoedymd);

