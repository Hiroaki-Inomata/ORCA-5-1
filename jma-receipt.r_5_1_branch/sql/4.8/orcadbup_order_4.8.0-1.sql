-- tbl_order --


-- $BF~1!%*!<%@!<(B
-- UKEHMS$BJQ99(B

\set ON_ERROR_STOP

ALTER TABLE tbl_order alter column ukehms type char(8);
update tbl_order set ukehms = trim(ukehms) || '00';

