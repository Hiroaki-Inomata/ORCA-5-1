--                                      
-- ÅÀ¿ô¥Æ¡¼¥Ö¥ë
--                                    
-- ÉûÉ¡¹Ğ¼ê½ÑÍÑÆâ»ë¶À²Ã»»ÄÉ²Ã
-- ÉûÉ¡¹Ğ¼ê½ÑÍÑ¹üÆğÉôÁÈ¿¥ÀÚ½üµ¡´ï²Ã»»ÄÉ²Ã
-- Ä¹»ş´ÖËã¿ì´ÉÍı²Ã»»ÄÉ²Ã
-- £Ä£Ğ£Ã¶èÊ¬ÄÉ²Ã
--                                   
-- Create Date : 2014/03/06      

\set ON_ERROR_STOP

alter table tbl_tensu add column fukubikunaikbn smallint default 0;
alter table tbl_tensu add column fukubikukotukbn smallint default 0;
alter table tbl_tensu add column timemasuikbn smallint default 0;
alter table tbl_tensu add column dpckbn smallint default 0;
-- update tbl_tensu set fukubikunaikbn=0,fukubikukotukbn=0,timemasuikbn=0,dpckbn=0;
