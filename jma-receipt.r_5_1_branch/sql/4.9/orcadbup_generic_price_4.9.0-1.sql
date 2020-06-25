-- tbl_generic_price --
-- price numeric(11,2) -> numeric(12,3) --
-- gecode character verying(12)
--

\set ON_ERROR_STOP

ALTER TABLE tbl_generic_price ALTER price TYPE numeric(12,3);
ALTER TABLE tbl_generic_price ADD COLUMN gecode character varying(12);
UPDATE tbl_generic_price SET gecode='';
