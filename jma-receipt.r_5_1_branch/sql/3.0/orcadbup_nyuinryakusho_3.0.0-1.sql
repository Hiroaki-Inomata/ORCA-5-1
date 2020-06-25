\set ON_ERROR_STOP
delete from tbl_nyuinryakusho where srycd = '190121310' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121410' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121510' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121610' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121710' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190121810' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122010' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122110' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122210' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122310' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122410' and yukostymd = '20060701' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190122510' and yukostymd = '20060701' and yukoedymd = '99999999';
COPY "tbl_nyuinryakusho" FROM stdin;
190121310	20060701	99999999	闻Ａ						
190121410	20060701	99999999	闻Ｂ						
190121510	20060701	99999999	闻Ｃ						
190121610	20060701	99999999	闻Ｄ						
190121710	20060701	99999999	闻Ｅ						
190121810	20060701	99999999	闻泼						
190122010	20060701	99999999	壳闻Ａ						
190122110	20060701	99999999	壳闻Ｂ						
190122210	20060701	99999999	壳闻Ｃ						
190122310	20060701	99999999	壳闻Ｄ						
190122410	20060701	99999999	壳闻Ｅ						
190122510	20060701	99999999	壳闻２泼						
\.
