--                                                --
-- êóáì´µ¼Ô´ÉÍı²Ã»»ÂĞ±ş                           --
--                                                -- 
-- Create Date : 2008/07/24                       --
--                                                --
\set ON_ERROR_STOP

delete from tbl_nyuksnchk where nyuinkbn = '013' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '014' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '020' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '021' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '085' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '086' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '100' and ksnkbn = '410';
delete from tbl_nyuksnchk where nyuinkbn = '101' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '102' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '108' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '109' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '132' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '133' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '143' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '144' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '145' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '146' and ksnkbn = '410' and yukostymd = '20060401';
delete from tbl_nyuksnchk where nyuinkbn = '219' and ksnkbn = '410' and yukostymd = '20060401';

COPY "tbl_nyuksnchk" FROM stdin;
013	410	20060401	20060630	1			        	        	      
014	410	20060401	20060630	1			        	        	      
020	410	20060401	20060630	1			        	        	      
021	410	20060401	20060630	1			        	        	      
085	410	20060401	20060630	1			        	        	      
086	410	20060401	20060630	1			        	        	      
100	410	00000000	99999999	1			        	        	      
101	410	20060401	20060630	1			        	        	      
102	410	20060401	20060630	1			        	        	      
108	410	20060401	20060630	1			        	        	      
109	410	20060401	20060630	1			        	        	      
132	410	20060401	20060630	1			        	        	      
133	410	20060401	20060630	1			        	        	      
143	410	20060401	20060930	1			        	        	      
144	410	20060401	20060930	1			        	        	      
145	410	20060401	20060930	1			        	        	      
146	410	20060401	20060930	1			        	        	      
219	410	20060401	20080331	1			        	        	      
\.
