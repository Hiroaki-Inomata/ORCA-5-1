--                                    --
-- ÅÀ¿ô¥Æ¡¼¥Ö¥ë¤ÎÊÑ¹¹       --
--                                    --
-- ¹àÌÜ¤ÎÄÉ²Ã                         --
--   ²èÁü¼ê½Ñ»Ù±ç²Ã»»                 --
--   °åÎÅ´Ñ»¡Ë¡ÂĞ¾İ¶èÊ¬               --
--   Ëã¿ì¼±ÊÌ¶èÊ¬                     --
--                                    --
-- Create Date : 2008/03/07           --
--                                    --
\set ON_ERROR_STOP


--    ¹àÌÜÄÉ²Ã                        --
alter table tbl_tensu
   add column gazoopesup numeric(01);
alter table tbl_tensu
   add column iryokansatukbn numeric(01);
alter table tbl_tensu
   add column masuiskbkbn numeric(01);

--    ¥¼¥íÀßÄê                       --
alter table tbl_tensu
   alter gazoopesup set default 0;
alter table tbl_tensu
   alter iryokansatukbn set default 0;
alter table tbl_tensu
   alter masuiskbkbn set default 0;

update tbl_tensu set gazoopesup=0,iryokansatukbn=0,masuiskbkbn=0;
