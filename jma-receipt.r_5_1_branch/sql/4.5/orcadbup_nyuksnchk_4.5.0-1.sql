\set ON_ERROR_STOP
--
-- �������û������å�
-- �������ϰ�ܹԼ»ܲû������å�
-- Create Date : 2009/06/12

delete from tbl_nyuksnchk where ksnkbn = '423' ;

COPY tbl_nyuksnchk (nyuinkbn, ksnkbn, yukostymd, yukoedymd, chkkbn, termid, opid, creymd, upymd, uphms) FROM stdin;
031	423	20080401	99999999	1			        	        	      
033	423	20080401	99999999	1			        	        	      
034	423	20080401	99999999	1			        	        	      
035	423	20080401	99999999	1			        	        	      
038	423	20080401	99999999	1			        	        	      
050	423	20080401	99999999	1			        	        	      
140	423	20080401	99999999	1			        	        	      
141	423	20080401	99999999	1			        	        	      
217	423	20080401	99999999	1			        	        	      
\.
