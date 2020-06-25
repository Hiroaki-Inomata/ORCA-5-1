\set ON_ERROR_STOP
--
-- 医薬品傷病名ＤＢ適応傷病名リンクテーブル
-- Create Date : 2015/03/31        --
--

CREATE TABLE tbl_tekiou_disease (
  KONOCD          character varying(5) NOT NULL,
  BYOMEI          character varying(200),
  BEFORE_BYOMEI   character varying(100),
  AFTER_BYOMEI    character varying(100),
  BYOMEICD        character varying(7) NOT NULL,
  BEFORE_BYOMEICD character varying(4),
  AFTER_BYOMEICD  character varying(4),
  ICD             character varying(5),
  KAITEIYM        character varying(6),
  HOJYOKBN        smallint default 0,
  KONOMEINO       integer default 0,
  CREYMD          character varying(8),
  UPYMD           character varying(8),
  UPHMS           character varying(6)
);

ALTER TABLE ONLY tbl_tekiou_disease
  ADD CONSTRAINT tbl_tekiou_disease_primary_key PRIMARY KEY (KONOCD, BYOMEICD);
