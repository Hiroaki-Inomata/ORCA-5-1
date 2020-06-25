\set ON_ERROR_STOP
--
-- ÇÓÂ¾À©¸æ¶ÈÌ³´ÉÍý
-- Update Date : 2011/07/11
--

delete from tbl_ldmap where map_ldname in ('ORCA21','ORCA23','ORCA24','ORCA31','ORCA32','ORCA33','ORCA34');

COPY tbl_ldmap (map_ldname, map_groupnum, map_edanum, map_ldmsg, map_dsp_flg, map_haita_flg, map_termid, map_opid, ldmap_chk_ldname1, ldmap_chk_ldname2, ldmap_chk_ldname3, ldmap_chk_ldname4, ldmap_chk_ldname5, ldmap_chk_ldname6, ldmap_chk_ldname7, ldmap_chk_ldname8, ldmap_chk_ldname9, ldmap_chk_ldname10, ldmap_chk_ldname11, ldmap_chk_ldname12, ldmap_chk_ldname13, ldmap_chk_ldname14, ldmap_chk_ldname15, ldmap_chk_ldname16, ldmap_chk_ldname17, ldmap_chk_ldname18, ldmap_chk_ldname19, ldmap_chk_ldname20, ldmap_chk_ldname21, ldmap_chk_ldname22, ldmap_chk_ldname23, ldmap_chk_ldname24, ldmap_chk_ldname25, ldmap_chk_ldname26, ldmap_chk_ldname27, ldmap_chk_ldname28, ldmap_chk_ldname29, ldmap_chk_ldname30, ldmap_chk_ldname31, ldmap_chk_ldname32, ldmap_chk_ldname33, ldmap_chk_ldname34, ldmap_chk_ldname35, ldmap_chk_ldname36, ldmap_chk_ldname37, ldmap_chk_ldname38, ldmap_chk_ldname39, ldmap_chk_ldname40, ldmap_chk_ldname41, ldmap_chk_ldname42, ldmap_chk_ldname43, ldmap_chk_ldname44, ldmap_chk_ldname45, ldmap_chk_ldname46, ldmap_chk_ldname47, ldmap_chk_ldname48, ldmap_chk_ldname49, ldmap_chk_ldname50, hospnum) FROM stdin;
ORCA21	1	4	21 ¿ÇÎÅ¹Ô°Ù	1	1			ORCA12	ORCA21	ORCA23	ORCA24	ORCA31	ORCA32	ORCA33	ORCA34																																											1
ORCA23	1	6	23 ¼ýÇ¼	1	1			ORCA12	ORCA21	ORCA23	ORCA24	ORCA31	ORCA32	ORCA33	ORCA34																																											1
ORCA24	1	7	24 ²ñ·×¾È²ñ	1	1			ORCA12	ORCA21	ORCA23	ORCA24	ORCA31	ORCA32	ORCA33	ORCA34																																											1
ORCA31	2	0	31 ÆþÂà±¡ÅÐÏ¿	1	1			ORCA12	ORCA21	ORCA23	ORCA24	ORCA31	ORCA32	ORCA33	ORCA34																																											1
ORCA32	2	1	32 Æþ±¡²ñ·×¾È²ñ	1	1			ORCA12	ORCA21	ORCA23	ORCA24	ORCA31	ORCA32	ORCA33	ORCA34																																											1
ORCA33	2	2	33 Æþ±¡Äê´üÀÁµá	1	1			ORCA12	ORCA21	ORCA23	ORCA24	ORCA31	ORCA32	ORCA33	ORCA34																																											1
ORCA34	2	3	34 Âà±¡»þ²¾·×»»	1	1			ORCA12	ORCA21	ORCA23	ORCA24	ORCA31	ORCA32	ORCA33	ORCA34																																											1
\.

