\set ON_ERROR_STOP

--				--
-- �ģ´�������ǡ���ʣ��	--
--				--
-- Create Date : 2007/01/26	--
--				--

INSERT INTO tbl_mstkanri (
	hospnum,
	kanricd,
	dbrversion1,
	dbrversion2,
	termid,
	opid,
	creymd,
	upymd,
	uphms
)
SELECT
	'1',
	kanricd,
	dbrversion1,
	dbrversion2,
	termid,
	opid,
	creymd,
	upymd,
	uphms
FROM tbl_dbkanri ;
