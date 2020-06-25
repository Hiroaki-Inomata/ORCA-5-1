\set ON_ERROR_STOP

--				--
-- £Ä£Â´ÉÍý¤«¤é¥Ç¡¼¥¿Ê£¼Ì	--
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
