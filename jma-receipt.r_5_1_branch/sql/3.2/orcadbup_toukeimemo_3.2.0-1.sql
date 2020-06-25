\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid in ('ORCBG013V02') and kanricd = '0000';
delete from tbl_toukeimemo where pgid in ('ORCBHYOUKETU') and kanricd = '0000';

COPY "tbl_toukeimemo" FROM stdin;
ORCBHYOUKETU	0000	000     	00000000	99999999									½èÍý¶èÊ¬\n£±¡§É¸·ç²ñ·×¹¹¿·£±£°£°Ê¬¤Î£±£°\n£²¡§É¸·ç²ñ·×¹¹¿·£±£°£°Ê¬¤Î£±£µ\n£³¡§É¸·ç²ñ·×¹¹¿·£±£°£°Ê¬¤Î£²\n£´¡§É¸·ç²ñ·×¹¹¿·£±£°£°Ê¬¤Î£³\n£µ¡§É¸·ç²ñ·×¹¹¿·¡Ê¼è¾Ã¤·¡Ë\n			20060818	20060818	153238
\.
