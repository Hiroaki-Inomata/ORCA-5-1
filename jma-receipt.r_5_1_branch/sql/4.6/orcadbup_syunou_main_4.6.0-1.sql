--                                      
-- ��Ǽ�ơ��֥�                        
--                                    
-- ʬ�ڴ����ե饰�ɲ�
--                                   
--                                  
-- Create Date : 2010/08/27        
--                                
\set ON_ERROR_STOP

alter table tbl_syunou_main add COLUMN  bunbeninfflg char(1);
update tbl_syunou_main set bunbeninfflg = '';
