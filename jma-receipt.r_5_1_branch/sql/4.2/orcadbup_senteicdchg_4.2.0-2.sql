--                                    --
-- 選定療養置換テーブルコード追加       --
-- 190130810 一般病棟１０対１入院基本料（Ｈ２０年３月３１日時点７対１）
-- 190131110 専門病院１０対１入院基本料（Ｈ２０年３月３１日時点７対１）
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
