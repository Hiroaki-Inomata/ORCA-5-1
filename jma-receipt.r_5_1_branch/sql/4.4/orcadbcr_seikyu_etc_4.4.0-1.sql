\set ON_ERROR_STOP

CREATE TABLE tbl_seikyu_etc (
    hospnum smallint DEFAULT 0 NOT NULL,
    sryym integer DEFAULT 0 NOT NULL,
    nyugaikbn character(1) NOT NULL,
    ptid bigint DEFAULT 0 NOT NULL,
    receka character(2) NOT NULL,
    teisyutusaki smallint DEFAULT 0 NOT NULL,
    recesyubetu character(4) NOT NULL,
    hknjanum character(8) NOT NULL,
    hojokbn_key character(1) NOT NULL,
    tekstymd character(8) NOT NULL,
    etc_kohnum1 character(3) DEFAULT '',
    etc_kohnum2 character(3) DEFAULT '',
    etc_kohnum3 character(3) DEFAULT '',
    etc_kohnum4 character(3) DEFAULT '',
    etc_jnissu1 smallint DEFAULT 0,
    etc_jnissu2 smallint DEFAULT 0,
    etc_jnissu3 smallint DEFAULT 0,
    etc_jnissu4 smallint DEFAULT 0,
    etc_totalten1 integer DEFAULT 0,
    etc_totalten2 integer DEFAULT 0,
    etc_totalten3 integer DEFAULT 0,
    etc_totalten4 integer DEFAULT 0,
    etc_ftnmoney1 integer DEFAULT 0,
    etc_ftnmoney_ftn1 integer DEFAULT 0,
    etc_ftnmoney_koj1 integer DEFAULT 0,
    etc_ftnmoney2 integer DEFAULT 0,
    etc_ftnmoney_ftn2 integer DEFAULT 0,
    etc_ftnmoney_koj2 integer DEFAULT 0,
    etc_ftnmoney3 integer DEFAULT 0,
    etc_ftnmoney_ftn3 integer DEFAULT 0,
    etc_ftnmoney_koj3 integer DEFAULT 0,
    etc_ftnmoney4 integer DEFAULT 0,
    etc_ftnmoney_ftn4 integer DEFAULT 0,
    etc_ftnmoney_koj4 integer DEFAULT 0,
    etc_ykzftn1 integer DEFAULT 0,
    etc_ykzftn2 integer DEFAULT 0,
    etc_ykzftn3 integer DEFAULT 0,
    etc_ykzftn4 integer DEFAULT 0,
    etc_shokujinissu1 smallint DEFAULT 0,
    etc_shokujiryoyohi1 integer DEFAULT 0,
    etc_shokujiftn1 integer DEFAULT 0,
    etc_shokujinissu2 smallint DEFAULT 0,
    etc_shokujiryoyohi2 integer DEFAULT 0,
    etc_shokujiftn2 integer DEFAULT 0,
    etc_shokujinissu3 smallint DEFAULT 0,
    etc_shokujiryoyohi3 integer DEFAULT 0,
    etc_shokujiftn3 integer DEFAULT 0,
    etc_shokujinissu4 smallint DEFAULT 0,
    etc_shokujiryoyohi4 integer DEFAULT 0,
    etc_shokujiftn4 integer DEFAULT 0
)
;
insert into tbl_seikyu_etc
select hospnum ,sryym,nyugaikbn ,ptid,receka,teisyutusaki,recesyubetu,hknjanum,hojokbn_key,tekstymd,etc_kohnum1,etc_kohnum2,etc_kohnum3,etc_kohnum4,etc_jnissu1,etc_jnissu2,etc_jnissu3,etc_jnissu4,etc_totalten1,etc_totalten2,etc_totalten3,etc_totalten4,etc_ftnmoney1,etc_ftnmoney_ftn1,etc_ftnmoney_koj1,etc_ftnmoney2,etc_ftnmoney_ftn2,etc_ftnmoney_koj2,etc_ftnmoney3,etc_ftnmoney_ftn3,etc_ftnmoney_koj3,etc_ftnmoney4,etc_ftnmoney_ftn4,etc_ftnmoney_koj4,etc_ykzftn1,etc_ykzftn2,etc_ykzftn3,etc_ykzftn4,etc_shokujinissu1,etc_shokujiryoyohi1,etc_shokujiftn1,etc_shokujinissu2,etc_shokujiryoyohi2,etc_shokujiftn2,etc_shokujinissu3,etc_shokujiryoyohi3,etc_shokujiftn3,etc_shokujinissu4,etc_shokujiryoyohi4,etc_shokujiftn4
from tbl_seikyu
where
   etc_kohnum1 <> ''
or etc_kohnum2 <> ''
or etc_kohnum3 <> ''
or etc_kohnum4 <> ''
or etc_jnissu1 <> 0
or etc_jnissu2 <> 0
or etc_jnissu3 <> 0
or etc_jnissu4 <> 0
or etc_totalten1 <> 0
or etc_totalten2 <> 0
or etc_totalten3 <> 0
or etc_totalten4 <> 0
or etc_ftnmoney1 <> 0
or etc_ftnmoney_ftn1 <> 0
or etc_ftnmoney_koj1 <> 0
or etc_ftnmoney2 <> 0
or etc_ftnmoney_ftn2 <> 0
or etc_ftnmoney_koj2 <> 0
or etc_ftnmoney3 <> 0
or etc_ftnmoney_ftn3 <> 0
or etc_ftnmoney_koj3 <> 0
or etc_ftnmoney4 <> 0
or etc_ftnmoney_ftn4 <> 0
or etc_ftnmoney_koj4 <> 0
or etc_ykzftn1 <> 0
or etc_ykzftn2 <> 0
or etc_ykzftn3 <> 0
or etc_ykzftn4 <> 0
or etc_shokujinissu1 <> 0
or etc_shokujiryoyohi1 <> 0
or etc_shokujiftn1 <> 0
or etc_shokujinissu2 <> 0
or etc_shokujiryoyohi2 <> 0
or etc_shokujiftn2 <> 0
or etc_shokujinissu3 <> 0
or etc_shokujiryoyohi3 <> 0
or etc_shokujiftn3 <> 0
or etc_shokujinissu4 <> 0
or etc_shokujiryoyohi4 <> 0
or etc_shokujiftn4 <> 0
;

ALTER TABLE ONLY tbl_seikyu_etc
    ADD CONSTRAINT tbl_seikyu_etc_primary_key PRIMARY KEY (hospnum, sryym, nyugaikbn, ptid, receka, teisyutusaki, recesyubetu, hknjanum, hojokbn_key, tekstymd);
