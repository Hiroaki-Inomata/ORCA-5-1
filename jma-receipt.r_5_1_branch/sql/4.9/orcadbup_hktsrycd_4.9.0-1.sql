-- tbl_hktsrycd --
-- kanricd,rennum --
-- numeric -> smallint --
--

\set ON_ERROR_STOP

ALTER TABLE tbl_hktsrycd ALTER kanricd TYPE smallint;
ALTER TABLE tbl_hktsrycd ALTER rennum TYPE smallint;
