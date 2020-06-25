--                                    --
-- 点数マスタ更新                     --
--                                    --
-- Create Date : 2009/03/09           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_tensu where srycd = '099999903' or srycd = '099999904';
delete from tbl_tensu where hospnum = 0;

COPY tbl_tensu (srycd, yukostymd, yukoedymd, srykbn, srysyukbn, yukoketa, name, kanayukoketa, kananame, formalyukoketa, formalname, tensikibetu, ten, tanicd, tanimojisu, taniname, datakbn, hkntekkbn, nyugaitekkbn, routekkbn, hospsrykbn, osinkbn, houksnkbn, byokanrenkbn, sdokanryo, jituday, daycnt, ykzknrkbn, kzmcompsikibetu, kzmkgn, kzmjgn, kzm, kzmten, kzmerr, jgncnt, jgncnt1d, jgncnt1w, jgncntetcm, jgncntetc, jgncnterr, chuksncd, chuksntsuban, tsusokuage, kgnage, jgnage, timeksnkbn, kijunteigenkbn, kijunteigencd, laserksn, chpmesuksn, micmesuksn, donorkbn, knsjiskbn, knsjisgrpkbn, teigenkbn, sekizuisokutei, keibujyutu, autohougo, gaikanrikbn, tusokugaikbn, hokatuteigenkbn, chpnaisiksn, autofungo, chiikiksn, byosyokbn, chpgyokosotiksn, shortstayope, buikbn, santeirrkkbn, sstkijuncd1, sstkijuncd2, sstkijuncd3, sstkijuncd4, sstkijuncd5, sstkijuncd6, sstkijuncd7, sstkijuncd8, sstkijuncd9, sstkijuncd10, ageksnkgn1, ageksnjgn1, ageksnsrycd1, ageksnkgn2, ageksnjgn2, ageksnsrycd2, ageksnkgn3, ageksnjgn3, ageksnsrycd3, ageksnkgn4, ageksnjgn4, ageksnsrycd4, kentaicomment, nyukhnkbn, nyukhnksnkbn, kangoksn, oldtenskb, oldten, madokukbn, sinkeihakaikbn, seibutugakukbn, zoeizaikbn, csyyouryo, ykzkbn, zaigatakbn, kouhatukbn, longtoyokbn, meiuseskb, tokukizaiageksnkbn, sansokbn, tokukizaisbt1, tokukizaisbt2, jgnten, gaitenttlkbn, nyutenttlkbn, cdkbn_kbn, cdkbn_syo, cdkbn_bu, cdkbn_kbnnum, cdkbn_kbnnum_eda, cdkbn_kouban, kokuchi_kbn, kokuchi_syo, kokuchi_bu, kokuchi_kbnnum, kokuchi_kbnnum_eda, kokuchi_kouban, kokujiskbkbn1, kokujiskbkbn2, namechgkbn, kananamechgkbn, idokanren, kouhyojyunnum, yakkakjncd, syusaiskb, syomeikanren, chgymd, haisiymd, keikasochiymd, termid, opid, creymd, upymd, uphms, hospnum, gazoopesup, iryokansatukbn, masuiskbkbn) FROM stdin;
099999903	20080401	99999999	99	990	8	出来高算定（剤）	10	デキダカサンテイザイ	0		0	0.00	000	0		0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0.00	0	0	0	0	0	0	0	0000	0	0	00	00	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	00	00	         	00	00	         	00	00	         	00	00	         	0	0	0	0	0	0.00	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	 	0	0	0	0	0	 	0	0	0	0	0	0	0	0	0	         	0	            	0	         	        	        	        			20090309	20090309	114251	0	0	0	0
099999904	20080401	99999999	99	990	15	出来高算定（日）（入院料包括）	21	デキダカサンテイビニュウインリョウホウカツ	0		0	0.00	000	0		0	0	1	0	0	0	0	0	0	0	0	0	0	0	0	0	0.00	0	0	1	0	0	0	0	0000	0	0	00	00	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	1	0	0	0	0	0	0	0	0	0	0	00	00	         	00	00	         	00	00	         	00	00	         	0	0	0	0	0	0.00	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	 	0	0	0	0	0	 	0	0	0	0	0	0	0	0	0	         	0	            	0	         	        	        	        			20090309	20090309	114251	0	0	0	0
\.

insert into tbl_tensu
select
a.srycd,a.yukostymd,a.yukoedymd,a.srykbn,a.srysyukbn,a.yukoketa,a.name,a.kanayukoketa,a.kananame,a.formalyukoketa,a.formalname,
a.tensikibetu,a.ten,a.tanicd,a.tanimojisu,a.taniname,a.datakbn,a.hkntekkbn,a.nyugaitekkbn,a.routekkbn,a.hospsrykbn,a.osinkbn,a.houksnkbn,
a.byokanrenkbn,a.sdokanryo,a.jituday,a.daycnt,a.ykzknrkbn,a.kzmcompsikibetu,a.kzmkgn,a.kzmjgn,a.kzm,a.kzmten,a.kzmerr,a.jgncnt,a.jgncnt1d,
a.jgncnt1w,a.jgncntetcm,a.jgncntetc,a.jgncnterr,a.chuksncd,a.chuksntsuban,a.tsusokuage,a.kgnage,a.jgnage,a.timeksnkbn,a.kijunteigenkbn,
a.kijunteigencd,a.laserksn,a.chpmesuksn,a.micmesuksn,a.donorkbn,a.knsjiskbn,a.knsjisgrpkbn,a.teigenkbn,a.sekizuisokutei,a.keibujyutu,
a.autohougo,a.gaikanrikbn,a.tusokugaikbn,a.hokatuteigenkbn,a.chpnaisiksn,a.autofungo,a.chiikiksn,a.byosyokbn,a.chpgyokosotiksn,a.shortstayope,
a.buikbn,a.santeirrkkbn,a.sstkijuncd1,a.sstkijuncd2,a.sstkijuncd3,a.sstkijuncd4,a.sstkijuncd5,a.sstkijuncd6,a.sstkijuncd7,a.sstkijuncd8,
a.sstkijuncd9,a.sstkijuncd10,a.ageksnkgn1,a.ageksnjgn1,a.ageksnsrycd1,a.ageksnkgn2,a.ageksnjgn2,a.ageksnsrycd2,a.ageksnkgn3,a.ageksnjgn3,
a.ageksnsrycd3,a.ageksnkgn4,a.ageksnjgn4,a.ageksnsrycd4,a.kentaicomment,a.nyukhnkbn,a.nyukhnksnkbn,a.kangoksn,a.oldtenskb,a.oldten,a.madokukbn,
a.sinkeihakaikbn,a.seibutugakukbn,a.zoeizaikbn,a.csyyouryo,a.ykzkbn,a.zaigatakbn,a.kouhatukbn,a.longtoyokbn,a.meiuseskb,a.tokukizaiageksnkbn,
a.sansokbn,a.tokukizaisbt1,a.tokukizaisbt2,a.jgnten,a.gaitenttlkbn,a.nyutenttlkbn,a.cdkbn_kbn,a.cdkbn_syo,a.cdkbn_bu,a.cdkbn_kbnnum,
a.cdkbn_kbnnum_eda,a.cdkbn_kouban,a.kokuchi_kbn,a.kokuchi_syo,a.kokuchi_bu,a.kokuchi_kbnnum,a.kokuchi_kbnnum_eda,a.kokuchi_kouban,
a.kokujiskbkbn1,a.kokujiskbkbn2,a.namechgkbn,a.kananamechgkbn,a.idokanren,a.kouhyojyunnum,a.yakkakjncd,a.syusaiskb,a.syomeikanren,
a.chgymd,a.haisiymd,a.keikasochiymd,a.termid,a.opid,a.creymd,a.upymd,a.uphms,b.hospnum,a.gazoopesup,a.iryokansatukbn,a.masuiskbkbn
from tbl_tensu a,tbl_sysbase b
where
    a.hospnum = 0 ;

delete from tbl_tensu where hospnum = 0;
