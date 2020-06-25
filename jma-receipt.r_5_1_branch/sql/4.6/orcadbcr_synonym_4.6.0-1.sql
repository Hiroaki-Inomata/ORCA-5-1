\set ON_ERROR_STOP
--
-- 同義語テーブル
-- Create Date : 2011/02/21
--

CREATE TABLE tbl_synonym (
  byomei character varying(200) NOT NULL,
  byomeicd character varying(7) NOT NULL,
  creymd character varying(8),
  upymd character varying(8),
  uphms character varying(6)
);

CREATE INDEX idx_synonym_byomei ON tbl_synonym USING btree (byomei);
