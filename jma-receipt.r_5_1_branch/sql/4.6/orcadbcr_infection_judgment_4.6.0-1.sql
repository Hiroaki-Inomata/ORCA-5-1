\set ON_ERROR_STOP
--
-- ´¶À÷¾É¥µ¡¼¥Ù¥¤
-- È½Äê
--
-- Create Date : 2011/01/21  --
--
create table tbl_infection_judgment (
 ID int NOT NULL,
 DISEASE smallint NOT NULL,
 JUDGMENT_CODE varchar(9),
 CODE_KBN smallint,
 SRYKBN varchar(2),
 CREYMD varchar(8),
 UPYMD varchar(8),
 UPHMS varchar(6)
);

ALTER TABLE ONLY tbl_infection_judgment
  ADD CONSTRAINT tbl_infection_judgment_primary_key PRIMARY KEY (id);

CREATE INDEX idx_infection_judgment_judgment_code ON tbl_infection_judgment USING btree (disease, judgment_code);
