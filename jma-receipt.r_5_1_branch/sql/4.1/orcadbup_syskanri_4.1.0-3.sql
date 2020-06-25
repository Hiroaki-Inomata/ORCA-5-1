\set ON_ERROR_STOP

COPY tbl_syskanri (kanricd, kbncd, styukymd, edyukymd, kanritbl, termid, opid, creymd, upymd, uphms, hospnum) FROM stdin;
2003	214     	00000000	99999999	못犬불남                                37051                                    2009			        	        	      	0
2003	215     	00000000	99999999	못犬불남                                37061                                    1007			        	        	      	0
2003	260     	00000000	99999999	못남                                    57200                                    2001			        	        	      	0
2003	261     	00000000	99999999	못남                                    57210                                    1001			        	        	      	0
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
    a.hospnum = 0
and a.kanricd = '2003';

delete from tbl_syskanri
where 
    hospnum = 0
and kanricd = '2003';
