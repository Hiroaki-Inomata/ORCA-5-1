delete from tbl_syskanri where kanricd = '5110' and kbncd = '05' and styukymd = '20040401';
delete from tbl_syskanri where kanricd = '5110' and kbncd = '06';
delete from tbl_syskanri where kanricd = '5110' and kbncd = '15';

COPY "tbl_syskanri" FROM stdin;
5110	05      	20040401	99999999	Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆş±¡ÎÁ£±        Ï·Ç§¼££±			        	        	      
5110	06      	00000000	99999999	Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µÎÅÍÜÉÂÅïÆş±¡ÎÁ          Ï·Ç§ÎÅ			        	        	      
5110	15      	20040401	99999999	Ï·¿ÍÀ­Ç§ÃÎ¾É¼À´µ¼£ÎÅÉÂÅïÆş±¡ÎÁ£²        Ï·Ç§¼££²			        	        	      
\.
