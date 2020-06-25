\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid in ('ORCBG018') and kanricd = '0000';

COPY "tbl_toukeimemo" FROM stdin;
ORCBG018	0000	000     	00000000	99999999									有効終了日　薬剤の有効終了日を入力します。\n\n※注意：薬剤と病名の区分のみを対象としています。他の区分については対応していません。\n			20070907	20070907	145453	1
\.
