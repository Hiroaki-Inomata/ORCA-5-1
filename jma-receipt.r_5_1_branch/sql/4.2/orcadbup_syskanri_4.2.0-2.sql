\set ON_ERROR_STOP

delete from tbl_syskanri
where
    kanricd = '0044'
and kbncd   = '1028'
and styukymd = '00000000'
and edyukymd = '99999999' ;


COPY tbl_syskanri FROM stdin;
0044	1028    	00000000	99999999	チェックマスタ・薬剤情報マスタ経過措置薬剤更新リスト                                                                                                            ORCBG018                有効終了日          YMD                                                                                                                                                                                                                                                                                     1			20070907	20070907	145453	0
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
and a.kbncd   = '1028'
and a.styukymd = '00000000'
and a.edyukymd = '99999999' ;

delete from tbl_syskanri
where 
    hospnum = 0
and kanricd = '0044'
and kbncd   = '1028'
and styukymd = '00000000'
and edyukymd = '99999999' ;
