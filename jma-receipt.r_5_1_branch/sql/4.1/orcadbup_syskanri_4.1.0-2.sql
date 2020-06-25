\set ON_ERROR_STOP

delete from tbl_syskanri
where
    kanricd = '0044'
and kbncd   = '1001'
and styukymd = '00000000'
and edyukymd = '99999999' ;


COPY tbl_syskanri FROM stdin;
0044	1001    	00000000	99999999	≤Ò∑◊•´°º•…                                                                                                                                                      ORCBG007                ø«Œ≈«Ø∑Ó            YM        ¥µº‘»÷πÊ            PTNUM     ∆˛≥∞∂Ë ¨            PSN1      …¬≈Ô»÷πÊ            BTUNUM     ¬§”ΩÁ              PSN1      ø«Œ≈≤ ∂Ë ¨          PSN1      ø«Œ≈≤               SRYKA      ›∏±∂Ë ¨            PSN1                                                                  12122222			20041006	20071112	171423	0
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
and a.kanricd = '0044'
and a.kbncd   = '1001'
and a.styukymd = '00000000'
and a.edyukymd = '99999999' ;

delete from tbl_syskanri
where 
    hospnum = 0
and kanricd = '0044'
and kbncd   = '1001'
and styukymd = '00000000'
and edyukymd = '99999999' ;
