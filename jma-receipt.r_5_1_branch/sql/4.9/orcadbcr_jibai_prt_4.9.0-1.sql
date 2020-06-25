
CREATE TABLE tbl_jibai_prt (
    HOSPNUM		smallint NOT NULL,
    SRYYM numeric(6,0) DEFAULT 0 NOT NULL,
    NYUGAIKBN character(1) NOT NULL,
    PTID numeric(10,0) DEFAULT 0 NOT NULL,
    RECEKA character(2) NOT NULL,
    TEISYUTUSAKI numeric(1,0) DEFAULT 0 NOT NULL,
    RECESYUBETU character(4) NOT NULL,
    HKNJANUM character varying(8) NOT NULL,
    HOJOKBN_KEY character(1) NOT NULL,
    KOHID_KEY bigint DEFAULT 0 NOT NULL,
    TEKSTYMD character varying(8) NOT NULL,
    RECEIPT_KBN	character varying(10)  NOT NULL,
    RENNUM numeric(3,0) DEFAULT 0 NOT NULL,
    PTNUM  character varying(20),
    HKNNUM character varying(3),
    RECEKBN character varying(1),
    PRTKBN character varying(3),
    CREYMD character varying(8),
    CREHMS character varying(6),
    FORMID character varying(20),
    PRTDATA character varying(10000)
);

ALTER TABLE ONLY tbl_jibai_prt
    ADD CONSTRAINT tbl_jibai_prt_primary_key PRIMARY KEY (HOSPNUM,SRYYM,NYUGAIKBN,PTID,RECEKA,TEISYUTUSAKI,RECESYUBETU,HKNJANUM,HOJOKBN_KEY,KOHID_KEY,TEKSTYMD,RECEIPT_KBN,RENNUM);
