--                                              --
-- ʿ������ǯ�ٲ������������ǡ����ɲ�           --
--                                              --
-- �����˰�������ࣷ�У�����������             --
-- �����˰�������ࣷ�У�������������Υ������   --
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
