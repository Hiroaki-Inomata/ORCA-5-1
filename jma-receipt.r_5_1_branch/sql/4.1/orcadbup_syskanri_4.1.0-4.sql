\set ON_ERROR_STOP

COPY tbl_syskanri (kanricd, kbncd, styukymd, edyukymd, kanritbl, termid, opid, creymd, upymd, uphms, hospnum) FROM stdin;
1028	01      	00000000	99999999	¥µº‘§À§Ë§ÎÕΩÃÛ                ¥µº‘ÕΩÃÛ  1                                     			        	        	      	0
1028	02      	00000000	99999999	∞Âª’§À§Ë§ÎÕΩÃÛ                ∞Âª’ÕΩÃÛ                                        			        	        	      	0
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
and a.kanricd = '1028';

delete from tbl_syskanri
where 
    hospnum = 0
and kanricd = '1028';
