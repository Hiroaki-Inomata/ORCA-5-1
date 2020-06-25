\set ON_ERROR_STOP
--
-- 定点設定テーブル
-- Create Date : 2011/05/13        --
--
create table tbl_das_conf (
  HOSPNUM       smallint NOT NULL,
  SINSEIYMD     character varying(8),
  KANANAME      character varying(50),
  NAME          character varying(50),
  EMAIL         character varying(256),
  HOSPSBT1      character varying(1),
  HOSPSBT2      character varying(1),
  BEDSUIPPAN    smallint default 0,
  BEDSURYOYO    smallint default 0,
  BEDSUSEISIN   smallint default 0,
  BEDSUKEKKAKU  smallint default 0,
  BEDSU         smallint default 0,
  SETTI         character varying(1),
  SYUKA         character varying(2),
  CHIIKI        character varying(1),
  DPCKBN        character varying(1),
  ZAITAKU       character varying(1),
  KAIINYM       character varying(6),
  POST          character varying(7),
  USERID        character varying(15),
  PASSWORD      character varying(128),
  AGREEMENT     character varying(1),
  SENDRESULT    character varying(3),
  SENDDATE      character varying(8),
  TERMID        character varying(16),
  OPID          character varying(16),
  CREYMD        character varying(8),
  UPYMD         character varying(8),
  UPHMS         character varying(6)
);

ALTER TABLE ONLY tbl_das_conf
    ADD CONSTRAINT tbl_das_conf_primary_key PRIMARY KEY (hospnum);
