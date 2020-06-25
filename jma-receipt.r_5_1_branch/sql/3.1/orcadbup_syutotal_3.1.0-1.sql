--                                    --
-- º˝«ºπÁ∑◊•∆°º•÷•Î§Œ —ππ                 --
--                                    --
-- π‡Ã‹§Œƒ…≤√                         --
--   À„øÏ   MSI                       --
--   ¿∫ø¿≤ ¿ÏÃÁŒ≈À°À„øÏ   SSN         --
--    ¸ºÕ¿˛         HOU               --
--   Œ≈Õ‹√¥≈ˆºÍ≈ˆ   RYO               --
--                                    --
--   ΩË ˝§ª§ÛŒ¡∫∆∑« SHOHOU_SAI        --
--                                    --
-- Create Date : 2006/07/06           --
--                                    --
\set ON_ERROR_STOP

--    π‡Ã‹ƒ…≤√                        --
alter table TBL_SYUTOTAL
   add column MSI_HKNTEN    numeric(07);
alter table TBL_SYUTOTAL
   add column MSI_MONEY     numeric(07);
alter table TBL_SYUTOTAL
   add column MSI_TGMONEY   numeric(07);
alter table TBL_SYUTOTAL
   add column MSI_TGMONEY_TAX numeric(07);
alter table TBL_SYUTOTAL
   add column SSN_HKNTEN    numeric(07);
alter table TBL_SYUTOTAL
   add column SSN_MONEY     numeric(07);
alter table TBL_SYUTOTAL
   add column SSN_TGMONEY   numeric(07);
alter table TBL_SYUTOTAL
   add column SSN_TGMONEY_TAX numeric(07);
alter table TBL_SYUTOTAL
   add column HOU_HKNTEN    numeric(07);
alter table TBL_SYUTOTAL
   add column HOU_MONEY     numeric(07);
alter table TBL_SYUTOTAL
   add column HOU_TGMONEY   numeric(07);
alter table TBL_SYUTOTAL
   add column HOU_TGMONEY_TAX numeric(07);
alter table TBL_SYUTOTAL
   add column RYO_HKNTEN    numeric(07);
alter table TBL_SYUTOTAL
   add column RYO_MONEY     numeric(07);
alter table TBL_SYUTOTAL
   add column RYO_TGMONEY   numeric(07);
alter table TBL_SYUTOTAL
   add column RYO_TGMONEY_TAX numeric(07);
alter table TBL_SYUTOTAL
   add column SHOHOU_SAI      numeric(07);

--    •º•Ì¿ﬂƒÍ                       --
alter table TBL_SYUTOTAL
   alter MSI_HKNTEN set default 0;
alter table TBL_SYUTOTAL
   alter MSI_MONEY set default 0;
alter table TBL_SYUTOTAL
   alter MSI_TGMONEY set default 0;
alter table TBL_SYUTOTAL
   alter MSI_TGMONEY_TAX set default 0;
alter table TBL_SYUTOTAL
   alter SSN_HKNTEN set default 0;
alter table TBL_SYUTOTAL
   alter SSN_MONEY set default 0;
alter table TBL_SYUTOTAL
   alter SSN_TGMONEY set default 0;
alter table TBL_SYUTOTAL
   alter SSN_TGMONEY_TAX set default 0;
alter table TBL_SYUTOTAL
   alter HOU_HKNTEN set default 0;
alter table TBL_SYUTOTAL
   alter HOU_MONEY set default 0;
alter table TBL_SYUTOTAL
   alter HOU_TGMONEY set default 0;
alter table TBL_SYUTOTAL
   alter HOU_TGMONEY_TAX set default 0;
alter table TBL_SYUTOTAL
   alter RYO_HKNTEN set default 0;
alter table TBL_SYUTOTAL
   alter RYO_MONEY set default 0;
alter table TBL_SYUTOTAL
   alter RYO_TGMONEY set default 0;
alter table TBL_SYUTOTAL
   alter RYO_TGMONEY_TAX set default 0;
alter table TBL_SYUTOTAL
   alter SHOHOU_SAI set default 0;
