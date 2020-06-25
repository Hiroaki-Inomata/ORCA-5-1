--                                                         --
-- °åÎÅ´Ñ»¡Ë¡Æþ±¡Î¬¾Î¥Æ¡¼¥Ö¥ë                              --
--                                                         --
-- µÞÀ­´üÆþ±¡ÂÐ¾Ý¼ÔÆþ±¡°å³Ø´ÉÍýÎÁ   (188000110)            --
-- ²óÉü´üÆþ±¡ÂÐ¾Ý¼ÔÆþ±¡°å³Ø´ÉÍýÎÁ   (188000510)            --
-- ¼Ò²ñÉüµ¢´üÆþ±¡ÂÐ¾Ý¼ÔÆþ±¡°å³Ø´ÉÍýÎÁ   (188000610)        --
--                                                         --
--                                              --
-- Create Date : 2010/08/02                     --
--                                              --
\set ON_ERROR_STOP

delete from tbl_nyuinryakusho where srycd = '188000110';
delete from tbl_nyuinryakusho where srycd = '188000510';
delete from tbl_nyuinryakusho where srycd = '188000610';

COPY "tbl_nyuinryakusho" FROM stdin;
188000110	20100401	99999999	´Ñ»¡µÞÀ­´ü				        	        	      
188000510	20100401	99999999	´Ñ»¡²óÉü´ü				        	        	      
188000610	20100401	99999999	´Ñ»¡Éüµ¢´ü				        	        	      
\.
