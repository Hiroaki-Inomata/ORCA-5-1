\set ON_ERROR_STOP
--
-- 感染症サーベイ
--
-- Create Date : 2009/11/04         --
--
create table tbl_infection (
 HOSPNUM smallint NOT NULL,
 SENDYMD char(8) NOT NULL,
 PTID bigint NOT NULL,
 DISEASE smallint NOT NULL,
 SRYYMD char(8) NOT NULL,
 SRYCD varchar(9),
 MEDSRYCD varchar(9),
 SYOSAI smallint,
 SYOSINYMD varchar(8),
 BYOMEICD varchar(7),
 BYOSRYYMD varchar(8),
 TENKIKBN varchar(1),
 TENKIYMD varchar(8),
 TERMID varchar(16),
 OPID varchar(16),
 CREYMD char(8),
 UPYMD char(8),
 UPHMS char(6)
);
ALTER TABLE ONLY tbl_infection
      ADD CONSTRAINT tbl_infection_primary_key PRIMARY KEY (hospnum,sendymd,ptid,disease,sryymd);
