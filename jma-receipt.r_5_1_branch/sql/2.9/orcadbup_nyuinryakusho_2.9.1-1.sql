delete from tbl_nyuinryakusho where srycd = '190123250' and yukostymd = '20060401' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190123350' and yukostymd = '20060401' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190123450' and yukostymd = '20060401' and yukoedymd = '99999999';
delete from tbl_nyuinryakusho where srycd = '190123550' and yukostymd = '20060401' and yukoedymd = '99999999';
COPY "tbl_nyuinryakusho" FROM stdin;
190123250	20060401	99999999	���̷в�						
190123350	20060401	99999999	��˷в�						
190123450	20060401	99999999	�����в�						
190123550	20060401	99999999	�㳲�в�						
\.
