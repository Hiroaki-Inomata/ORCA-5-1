\set ON_ERROR_STOP
--
--       °åÎÅ¾ðÊóÏ¢·¸´ðÈ×¥Ç¡¼¥¿
--
-- Create Date : 2013/12/10 --
--
CREATE TABLE tbl_renkei_tmp (
    hospnum numeric(2,0) NOT NULL,
    sryym numeric(6,0) DEFAULT 0 NOT NULL,
    nyugaikbn character(1) NOT NULL,
    ptid numeric(10,0) DEFAULT 0 NOT NULL,
    receka character(2) NOT NULL,
    teisyutusaki numeric(1,0) DEFAULT 0 NOT NULL,
    recesyubetu character(4) NOT NULL,
    hknjanum character(8) NOT NULL,
    hojokbn_key character(1) NOT NULL,
    kohid_key bigint DEFAULT 0 NOT NULL,
    tekstymd character(8) NOT NULL,
    receipt_kbn character(10) NOT NULL,
    reckbn character(1) NOT NULL,
    rennum numeric(4,0) DEFAULT 0 NOT NULL,
    totalten numeric(10,0) DEFAULT 0,
    sjkbn character(2),
    recedata character varying(2500),
    creymd character(8),
    crehms character(6)
);

ALTER TABLE ONLY tbl_renkei_tmp
    ADD CONSTRAINT tbl_renkei_tmp_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd,receipt_kbn,reckbn,rennum);