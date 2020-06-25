\set ON_ERROR_STOP
--
-- °åÌôÉÊ½ıÉÂÌ¾£Ä£Â°åÌôÉÊ¥Æ¡¼¥Ö¥ë
-- Create Date : 2015/03/31        --
--

CREATE TABLE tbl_tekiou_medicine (
  SRYCD    character varying(9) NOT NULL,
  KONOCD   character varying(5) NOT NULL,
  KAITEIYM character varying(6),
  CREYMD   character varying(8),
  UPYMD    character varying(8),
  UPHMS    character varying(6)
);

ALTER TABLE ONLY tbl_tekiou_medicine
  ADD CONSTRAINT tbl_tekiou_medicine_primary_key PRIMARY KEY (SRYCD);
