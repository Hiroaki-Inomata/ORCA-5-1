\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '5015';

COPY "tbl_syskanri" FROM stdin;
5015	*       	00000000	99999999	1			20070820	20070820	162554	1
\.
