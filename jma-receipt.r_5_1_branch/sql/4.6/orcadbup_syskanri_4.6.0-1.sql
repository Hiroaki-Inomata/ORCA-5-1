--                                    --
-- システム管理データ追加             --
-- 医療観察法の対応                   --
--   病棟情報の病棟種別               --
--   入退院登録の入院料選択           --
--                                    --
-- Create Date : 2011/01/07           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '5101' and kbncd in ('10') and styukymd = '20100401' and  edyukymd = '99999999';
delete from tbl_syskanri where kanricd = '5112';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
5101	10      	20100401	99999999	医療観察病棟						0
5112	01      	20100401	99999999	         急性期入院						0
5112	02      	20100401	99999999	         急性期入院（未適合１年以内）						0
5112	03      	20100401	99999999	         急性期入院（未適合１年超）						0
5112	04      	20100401	99999999	         回復期入院						0
5112	05      	20100401	99999999	         社会復帰期入院						0
5112	06      	20100401	99999999	         社会復帰期入院（１８１日以上１年以内）						0
5112	07      	20100401	99999999	         社会復帰期入院（１年超１年１８０日以内）						0
5112	08      	20100401	99999999	         社会復帰期入院（１年１８０日超）						0
5112	09      	20100401	99999999	         社会復帰期入院（未経過１年超）						0
5112	10      	20100401	99999999	         社会復帰期入院（未経過１年１８０日超）						0
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
