\set ON_ERROR_STOP
--
-- レセプト電算データ
-- 診療情報システム提供用
--
-- Create Date : 2017/11/28
--
CREATE TABLE tbl_receden_tmp (
    hospnum smallint DEFAULT 0 NOT NULL,
    sryym integer DEFAULT 0 NOT NULL,
    nyugaikbn character varying(1) NOT NULL,
    ptid bigint DEFAULT 0 NOT NULL,
    receka character varying(2) NOT NULL,
    teisyutusaki smallint DEFAULT 0 NOT NULL,
    recesyubetu character varying(4) NOT NULL,
    hknjanum character varying(8) NOT NULL,
    hojokbn_key character varying(1) NOT NULL,
    kohid_key bigint DEFAULT 0 NOT NULL,
    tekstymd character varying(8) NOT NULL,
    receipt_kbn character varying(10) NOT NULL,
    reckbn character varying(1) NOT NULL,
    rennum smallint DEFAULT 0 NOT NULL,
    totalten bigint DEFAULT 0,
    sjkbn character varying(2),
    recedata character varying(2500),
    creymd character varying(8),
    crehms character varying(6)
);

ALTER TABLE ONLY tbl_receden_tmp
    ADD CONSTRAINT tbl_receden_tmp_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, kohid_key, tekstymd,receipt_kbn,reckbn,rennum);
