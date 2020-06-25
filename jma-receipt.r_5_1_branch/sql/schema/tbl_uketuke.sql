CREATE TABLE tbl_uketuke (
    ukeymd character(8) NOT NULL,
    ukeid numeric(5,0) NOT NULL,
    uketime character(6),
    ptid numeric(10,0),
    name character varying(100),
    sryka character(2),
    drcd character(5),
    srynaiyo character(2),
    hkncombi numeric(4,0) DEFAULT 0,
    kaikeitime numeric(4,0) DEFAULT 0,
    keyyyktime numeric(4,0) DEFAULT 0,
    yyktime numeric(4,0) DEFAULT 0,
    yykid numeric(2,0) DEFAULT 0,
    hyojijun numeric(4,0) DEFAULT 0,
    kakren character(2),
    srflg character(2),
    termid character varying(16),
    opid character varying(16),
    creymd character(8),
    upymd character(8),
    uphms character(6),
    hospnum numeric(2,0) NOT NULL
);

ALTER TABLE ONLY tbl_uketuke
    ADD CONSTRAINT tbl_uketuke_primary_key PRIMARY KEY (hospnum, ukeymd, ukeid);

