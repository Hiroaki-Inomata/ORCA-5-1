\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid in ('ORCBG013V02') and kanricd = '0000';
delete from tbl_toukeimemo where pgid in ('ORCBHYOUKETU') and kanricd = '0000';

COPY "tbl_toukeimemo" FROM stdin;
ORCBHYOUKETU	0000	000     	00000000	99999999									������ʬ\n����ɸ���׹���������ʬ�Σ���\n����ɸ���׹���������ʬ�Σ���\n����ɸ���׹���������ʬ�Σ�\n����ɸ���׹���������ʬ�Σ�\n����ɸ���׹����ʼ�ä���\n			20060818	20060818	153238
\.
