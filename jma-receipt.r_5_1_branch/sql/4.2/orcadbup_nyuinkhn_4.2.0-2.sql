--                                    --
-- Æş±¡´ğËÜÎÁ¥Æ¡¼¥Ö¥ë¥³¡¼¥ÉÄÉ²Ã       --
-- 190130810 °ìÈÌÉÂÅï£±£°ÂĞ£±Æş±¡´ğËÜÎÁ¡Ê£È£²£°Ç¯£³·î£³£±Æü»şÅÀ£·ÂĞ£±¡Ë
-- 190131010 ·ë³ËÉÂÅï£±£°ÂĞ£±Æş±¡´ğËÜÎÁ¡Ê£È£²£°Ç¯£³·î£³£±Æü»şÅÀ£·ÂĞ£±¡Ë
-- 190131110 ÀìÌçÉÂ±¡£±£°ÂĞ£±Æş±¡´ğËÜÎÁ¡Ê£È£²£°Ç¯£³·î£³£±Æü»şÅÀ£·ÂĞ£±¡Ë
--                                    --
-- Create Date : 2008/06/25           --
--                                    --
\set ON_ERROR_STOP




delete from tbl_nyuinkhn where khn_srycd = '190130810' and yukostymd  = '20080701';
delete from tbl_nyuinkhn where khn_srycd = '190131010' and yukostymd  = '20080701';
delete from tbl_nyuinkhn where khn_srycd = '190131110' and yukostymd  = '20080701';

COPY tbl_nyuinkhn (khn_srycd, yukostymd, yukoedymd, iproukbn, sstkijunkbn, btusbtkbn, heikinzaikbn, kangohaichikbn, kangohiritukbn, kangohojohaichikbn, khn_ten, kagen_srycd1, kagen_srycd2, kagen_srycd3, kagen_srycd4, kagen_srycd5, kagen_srycd6, kagen_srycd7, kagen_ten1, kagen_ten2, kagen_ten3, kagen_ten4, kagen_ten5, kagen_ten6, kagen_ten7, kettei_ten1, kettei_ten2, kettei_ten3, kettei_ten4, kettei_ten5, kettei_ten6, kettei_ten7, termid, opid, creymd, upymd, uphms) FROM stdin;
190130810	20080701	99999999	1	 	  	  	  	  	  	1269	190079470	190079670	         	         	         	         	         	428	192	0	0	0	0	0	1697	1461	1269	1269	1269	1269	1269			        	        	      
190131010	20080701	99999999	1	 	  	  	  	  	  	1161	190118070	190083370	190083570	         	         	         	         	400	300	100	0	0	0	0	1561	1461	1261	1161	1161	1161	1161			        	        	      
190131110	20080701	99999999	1	 	  	  	  	  	  	1269	190092270	190092470	         	         	         	         	         	452	207	0	0	0	0	0	1721	1476	1269	1269	1269	1269	1269			        	        	      
