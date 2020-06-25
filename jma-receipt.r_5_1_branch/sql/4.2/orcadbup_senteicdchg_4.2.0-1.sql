--                                              --
-- 平成２０年度改定選定入院データ追加           --
--                                              --
-- （選）一般病棟準７対１入院基本料             --
-- （選）一般病棟準７対１入院基本料（離島等）   --
--                                              --
-- Create Date : 2008/03/07                     --
--                                              --
\set ON_ERROR_STOP

delete from tbl_senteicdchg where ipnsrycd = '190125410';
delete from tbl_senteicdchg where ipnsrycd = '190125510';

COPY "tbl_senteicdchg" FROM stdin;
190125410	190125610			        	        	      
190125510	190125710			        	        	      
\.
