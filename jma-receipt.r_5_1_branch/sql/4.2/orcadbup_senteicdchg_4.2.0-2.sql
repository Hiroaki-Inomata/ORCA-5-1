--                                    --
-- ���������ִ��ơ��֥륳�����ɲ�       --
-- 190130810 ����������У������������ʣȣ���ǯ��������������У���
-- 190131110 �����±������У������������ʣȣ���ǯ��������������У���
--                                    --
-- Create Date : 2008/06/25           --
--                                    --
\set ON_ERROR_STOP

delete from tbl_senteicdchg where ipnsrycd = '190130810' ;
delete from tbl_senteicdchg where ipnsrycd = '190131110' ;


COPY tbl_senteicdchg (ipnsrycd, senteisrycd, termid, opid, creymd, upymd, uphms) FROM stdin;
190130810	190130910			        	        	      
190131110	190131210			        	        	      
\.
