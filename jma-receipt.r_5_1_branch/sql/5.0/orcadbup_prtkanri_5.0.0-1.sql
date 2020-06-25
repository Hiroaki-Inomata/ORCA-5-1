--                                      
-- °õºþ´ÉÍý
--                                    
-- add api_id 
--                                   
-- Create Date : 2017/01/19

\set ON_ERROR_STOP

alter table tbl_prtkanri  add column api_id character varying(20);
update tbl_prtkanri set api_id = ''     where shellid <> 'API';
update tbl_prtkanri set api_id = prtnm  where shellid =  'API';
