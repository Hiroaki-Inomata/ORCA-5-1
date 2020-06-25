--                                    --
-- ¥·¥¹¥Æ¥à´ÉÍý¥Ç¡¼¥¿¹¹¿·             --
-- Æü¼¡¡¦·î¼¡Åý·×¥×¥í¥°¥é¥à           --
-- ¥Ñ¥é¥á¥¿¾ðÊó¹¹¿·SQL                --
--                                    --
-- Create Date : 2008/07/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0043' and kbncd in ('1010') and styukymd = '00000000' and  edyukymd = '99999999';
delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1023','1024','1030','1031') and styukymd = '00000000' and  edyukymd = '99999999';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0043	1010    	00000000	99999999	¼ýÇ¼ÆüÊó                                                                                                                                                        ORCBD010                ½èÍýÆü              YMD       ½ªÎ»½èÍýÆü          YMD       Æþ³°¶èÊ¬            PSN1      ½¸·×¶èÊ¬            PSN1      ÌÀºÙ¶èÊ¬            PSN1      ¹ç·×É½°õºþ¶èÊ¬      PSN1                                                                                                                              121111    1			20070209	20080710	160049	0
0044	1023    	00000000	99999999	»ØÄê¿ÇÎÅ¹Ô°Ù·ï¿ôÄ´                                                                                                                                              ORCBG016                ¿ÇÎÅÇ¯·î            YM        Æþ³°¶èÊ¬            PSN1      ¿ÇÎÅ²Ê              SRYKA     ¿ÇÎÅ¹Ô°Ù£±          SRYCD     ¿ÇÎÅ¹Ô°Ù£²          SRYCD     ¿ÇÎÅ¹Ô°Ù£³          SRYCD     ¿ÇÎÅ¹Ô°Ù£´          SRYCD     ¿ÇÎÅ¹Ô°Ù£µ          SRYCD     ¿ÇÎÅ¹Ô°Ù£¶          SRYCD     ¿ÇÎÅ¹Ô°Ù£·          SRYCD     1121222222			20060622	20080905	143956	0
0044	1024    	00000000	99999999	¿ÇÎÅ¹Ô°ÙÊÌ½¸·×É½                                                                                                                                                ORCBG017                ¿ÇÎÅÇ¯·î            YM        Æþ³°¶èÊ¬            PSN1      ¿ÇÎÅ²Ê              SRYKA     ¿ÇÎÅ¶èÊ¬£±          SRYKBN    ¿ÇÎÅ¶èÊ¬£²          SRYKBN    ¿ÇÎÅ¶èÊ¬£³          SRYKBN    ¿ÇÎÅ¶èÊ¬£´          SRYKBN    ¿ÇÎÅ¶èÊ¬£µ          SRYKBN    ¿ÇÎÅ¶èÊ¬£¶          SRYKBN    ¿ÇÎÅ¶èÊ¬£·          SRYKBN    1121222222			20060628	20080905	144546	0
0044	1030    	00000000	99999999	¿ÇÎÅ¹Ô°ÙÌ¤ÆþÎÏ´µ¼Ô°ìÍ÷É½                                                                                                                                        ORCBG020                ³«»Ï¿ÇÎÅÇ¯·î        YM        ½ªÎ»¿ÇÎÅÇ¯·î        YM        ÊÂ¤Ó½ç              PSN1                                                                                                                                                                                                                        221			20080905	20080905	145524	0
0044	1031    	00000000	99999999	Æþ±¡¿ÇÎÅ¹Ô°Ù¾È²ñ¾ðÊóºîÀ®                                                                                                                                        ORCBNSRYSRH             ³«»Ï¿ÇÎÅÇ¯·î        YM                                                                                                                                                                                                                                                                                      2			20080905	20080905	150042	0
\.

insert into tbl_syskanri
select
a.kanricd,
a.kbncd,
a.styukymd,
a.edyukymd,
a.kanritbl,
a.termid,
a.opid,
a.creymd,
a.upymd,
a.uphms,
b.hospnum
from tbl_syskanri a,tbl_sysbase b
where
    a.hospnum = 0 ;

delete from tbl_syskanri where hospnum = 0;
