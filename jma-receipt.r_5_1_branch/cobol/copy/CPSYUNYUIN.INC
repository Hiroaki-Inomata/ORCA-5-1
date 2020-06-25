      *******************************************************************
      * Project code name "ORCA"
      * ∆¸∞Â…∏Ω‡•Ï•ª•◊•»•Ω•’•»° JMA standard receipt software°À
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    ∆˛±°º˝«º•ﬁ•π•ø     COPY (CPSYUNYUIN.INC)              *
      ************************************************************
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      ************************************************************
      *01  SYUNYUIN-REC.
           03  SYUNYUIN-KEY.
      *∞ÂŒ≈µ°¥ÿ£…£ƒ
               05  SYUNYUIN-HOSPNUM          PIC 9(2).
      *∆˛≥∞∂Ë ¨
               05  SYUNYUIN-NYUGAIKBN       PIC  X(01).
      *¥µº‘£…£ƒ
               05  SYUNYUIN-PTID            PIC  9(10).
      *≈¡…º»÷πÊ
               05  SYUNYUIN-DENPNUM         PIC  9(07).
      *…Ù≤∞£Œ£œ
           03  SYUNYUIN-ROOMNUM             PIC  X(08).
      *…¬≈Ô•≥°º•…
           03  SYUNYUIN-BYOTONUM            PIC  X(02).
      *ººŒ¡∫π≥€
           03  SYUNYUIN-RMSAGAKU            PIC S9(07).
      *ººŒ¡∫π≥€°›æ√»Ò¿«° ∫∆∑«°À
           03  SYUNYUIN-RMSAGAKU-TAX-SAI    PIC S9(07).
      *ø©ªˆ£±§´§Èø©ªˆ£±£¥
           03  SYUNYUIN-SHOKUJI-NAIYOU.
               05  SYUNYUIN-SHOKUJI1-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI1        PIC S9(07).
               05  SYUNYUIN-SHOKUJI2-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI2        PIC S9(07).
               05  SYUNYUIN-SHOKUJI3-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI3        PIC S9(07).
               05  SYUNYUIN-SHOKUJI4-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI4        PIC S9(07).
               05  SYUNYUIN-SHOKUJI5-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI5        PIC S9(07).
               05  SYUNYUIN-SHOKUJI6-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI6        PIC S9(07).
               05  SYUNYUIN-SHOKUJI7-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI7        PIC S9(07).
               05  SYUNYUIN-SHOKUJI8-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI8        PIC S9(07).
               05  SYUNYUIN-SHOKUJI9-NISSU  PIC S9(02).
               05  SYUNYUIN-SHOKUJI9        PIC S9(07).
               05  SYUNYUIN-SHOKUJI10-NISSU PIC S9(02).
               05  SYUNYUIN-SHOKUJI10       PIC S9(07).
               05  SYUNYUIN-SHOKUJI11-NISSU PIC S9(02).
               05  SYUNYUIN-SHOKUJI11       PIC S9(07).
               05  SYUNYUIN-SHOKUJI12-NISSU PIC S9(02).
               05  SYUNYUIN-SHOKUJI12       PIC S9(07).
               05  SYUNYUIN-SHOKUJI13-NISSU PIC S9(02).
               05  SYUNYUIN-SHOKUJI13       PIC S9(07).
               05  SYUNYUIN-SHOKUJI14-NISSU PIC S9(02).
               05  SYUNYUIN-SHOKUJI14       PIC S9(07).
           03  SYUNYUIN-SYOKUJI-NAIYOUR
                       REDEFINES   SYUNYUIN-SHOKUJI-NAIYOU.
               05  SYUNYUIN-SYOKUJI-TBL     OCCURS   14.
                 07  SYUNYUIN-SHOKUJI-NISSU   PIC S9(02).
                 07  SYUNYUIN-SHOKUJI         PIC S9(07).
      *ø©ªˆŒ≈Õ‹»Òæ Û° ø©ªˆ∆¸øÙ°¶ø©ªˆŒ≈Õ‹»Ò°¶…∏Ω‡…È√¥≥€°À
           03  SYUNYUIN-SKJ-AREA.
               05  SYUNYUIN-SYUSKJ-NISSU      PIC S9(02).
               05  SYUNYUIN-SYUSKJ-RYOYOHI    PIC S9(07).
               05  SYUNYUIN-SYUSKJ-FTN        PIC S9(07).
               05  SYUNYUIN-SYUSKJ-FTN-RECE   PIC S9(07).
               05  SYUNYUIN-SYUSKJ-FTNKBN     PIC  X(01).
           03  SYUNYUIN-SKJ-AREA2.
               05  SYUNYUIN-KOH1SKJ-NISSU     PIC S9(02).
               05  SYUNYUIN-KOH1SKJ-RYOYOHI   PIC S9(07).
               05  SYUNYUIN-KOH1SKJ-FTN       PIC S9(07).
               05  SYUNYUIN-KOH1SKJ-FTN-RECE  PIC S9(07).
               05  SYUNYUIN-KOH1SKJ-FTNKBN    PIC  X(01).
               05  SYUNYUIN-KOH2SKJ-NISSU     PIC S9(02).
               05  SYUNYUIN-KOH2SKJ-RYOYOHI   PIC S9(07).
               05  SYUNYUIN-KOH2SKJ-FTN       PIC S9(07).
               05  SYUNYUIN-KOH2SKJ-FTN-RECE  PIC S9(07).
               05  SYUNYUIN-KOH2SKJ-FTNKBN    PIC  X(01).
               05  SYUNYUIN-KOH3SKJ-NISSU     PIC S9(02).
               05  SYUNYUIN-KOH3SKJ-RYOYOHI   PIC S9(07).
               05  SYUNYUIN-KOH3SKJ-FTN       PIC S9(07).
               05  SYUNYUIN-KOH3SKJ-FTN-RECE  PIC S9(07).
               05  SYUNYUIN-KOH3SKJ-FTNKBN    PIC  X(01).
               05  SYUNYUIN-KOH4SKJ-NISSU     PIC S9(02).
               05  SYUNYUIN-KOH4SKJ-RYOYOHI   PIC S9(07).
               05  SYUNYUIN-KOH4SKJ-FTN       PIC S9(07).
               05  SYUNYUIN-KOH4SKJ-FTN-RECE  PIC S9(07).
               05  SYUNYUIN-KOH4SKJ-FTNKBN    PIC  X(01).
           03  SYUNYUIN-SKJ-AREA2R
                       REDEFINES   SYUNYUIN-SKJ-AREA2.
               05  SYUNYUIN-SKJ-TBL         OCCURS   4.
                 07  SYUNYUIN-SKJ-NISSU       PIC S9(02).
                 07  SYUNYUIN-SKJ-RYOYOHI     PIC S9(07).
                 07  SYUNYUIN-SKJ-FTN         PIC S9(07).
                 07  SYUNYUIN-SKJ-FTN-RECE    PIC S9(07).
                 07  SYUNYUIN-SKJ-FTNKBN      PIC  X(01).
      *ø©ªˆŒ≈Õ‹»Ò°  ›∏±°À
           03  SYUNYUIN-RYOYOHI-SKJ         PIC S9(07).
      *ø©ªˆ…È√¥≥€°  ›∏±°ßº´∏ …È√¥°À
           03  SYUNYUIN-SKYMONEY-SKJ        PIC S9(07).
      *ø©ªˆ…È√¥≥€°  ›∏±°ßº´∏ …È√¥æ√»Ò¿«°À
           03  SYUNYUIN-SKYMONEY-SKJ-TAX    PIC S9(07).
      *ø©ªˆ…È√¥≥€°  ›∏±°ßº´∏ …È√¥πÁ∑◊°À
           03  SYUNYUIN-SKYMONEY-SKJ-KEI    PIC S9(07).
      *ø©ªˆŒ≈Õ‹»Ò° º´»Ò°À
           03  SYUNYUIN-RYOYOHI-SKJ-JIHI    PIC S9(07).
      *ø©ªˆ…È√¥≥€° º´»Ò°ßº´∏ …È√¥°À
           03  SYUNYUIN-SKYMONEY-SKJ-JIHI   PIC S9(07).
      *ø©ªˆ…È√¥≥€° º´»Ò°ßº´∏ …È√¥æ√»Ò¿«°À
           03  SYUNYUIN-SKYMONEY-SKJ-JIHI-TAX
                                       PIC S9(07).
      *ø©ªˆ…È√¥≥€° º´»Ò°ßº´∏ …È√¥πÁ∑◊°À
           03  SYUNYUIN-SKYMONEY-SKJ-JIHI-KEI
                                       PIC S9(07).
      *∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò°  ›∏±°À
           03  SYUNYUIN-RYOYOHI-LIFE        PIC S9(07).
      *∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€°  ›∏±°ßº´∏ …È√¥°À
           03  SYUNYUIN-SKYMONEY-LIFE       PIC S9(07).
      *∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€°  ›∏±°ßº´∏ …È√¥æ√»Ò¿«°À
           03  SYUNYUIN-SKYMONEY-LIFE-TAX   PIC S9(07).
      *∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€°  ›∏±°ßº´∏ …È√¥πÁ∑◊°À
           03  SYUNYUIN-SKYMONEY-LIFE-KEI   PIC S9(07).
      *∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò»Ò° º´»Ò°À
           03  SYUNYUIN-RYOYOHI-LIFE-JIHI   PIC S9(07).
      *∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€° º´»Ò°ßº´∏ …È√¥°À
           03  SYUNYUIN-SKYMONEY-LIFE-JIHI  PIC S9(07).
      *∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€° º´»Ò°ßº´∏ …È√¥æ√»Ò¿«°À
           03  SYUNYUIN-SKYMONEY-LIFE-JIHI-TAX
                                       PIC S9(07).
      *∆˛±°ª˛¿∏≥ËŒ≈Õ‹»Ò…È√¥≥€° º´»Ò°ßº´∏ …È√¥πÁ∑◊°À
           03  SYUNYUIN-SKYMONEY-LIFE-JIHI-KEI
                                       PIC S9(07).
      *ø«Œ≈•’•È•∞•∆°º•÷•Î
           03  SYUNYUIN-DAY-AREA.
      *        ø«Œ≈•’•È•∞•∆°º•÷•Î
             04  SYUNYUIN-DAY-TBL.
      *        ø«Œ≈•’•È•∞° £±∆¸§´§È£≥£±∆¸°À
               05  SYUNYUIN-DAY-1           PIC  X(01).
               05  SYUNYUIN-DAY-2           PIC  X(01).
               05  SYUNYUIN-DAY-3           PIC  X(01).
               05  SYUNYUIN-DAY-4           PIC  X(01).
               05  SYUNYUIN-DAY-5           PIC  X(01).
               05  SYUNYUIN-DAY-6           PIC  X(01).
               05  SYUNYUIN-DAY-7           PIC  X(01).
               05  SYUNYUIN-DAY-8           PIC  X(01).
               05  SYUNYUIN-DAY-9           PIC  X(01).
               05  SYUNYUIN-DAY-10          PIC  X(01).
               05  SYUNYUIN-DAY-11          PIC  X(01).
               05  SYUNYUIN-DAY-12          PIC  X(01).
               05  SYUNYUIN-DAY-13          PIC  X(01).
               05  SYUNYUIN-DAY-14          PIC  X(01).
               05  SYUNYUIN-DAY-15          PIC  X(01).
               05  SYUNYUIN-DAY-16          PIC  X(01).
               05  SYUNYUIN-DAY-17          PIC  X(01).
               05  SYUNYUIN-DAY-18          PIC  X(01).
               05  SYUNYUIN-DAY-19          PIC  X(01).
               05  SYUNYUIN-DAY-20          PIC  X(01).
               05  SYUNYUIN-DAY-21          PIC  X(01).
               05  SYUNYUIN-DAY-22          PIC  X(01).
               05  SYUNYUIN-DAY-23          PIC  X(01).
               05  SYUNYUIN-DAY-24          PIC  X(01).
               05  SYUNYUIN-DAY-25          PIC  X(01).
               05  SYUNYUIN-DAY-26          PIC  X(01).
               05  SYUNYUIN-DAY-27          PIC  X(01).
               05  SYUNYUIN-DAY-28          PIC  X(01).
               05  SYUNYUIN-DAY-29          PIC  X(01).
               05  SYUNYUIN-DAY-30          PIC  X(01).
               05  SYUNYUIN-DAY-31          PIC  X(01).
           03  SYUNYUIN-DAY-AREA-R REDEFINES   SYUNYUIN-DAY-AREA.
      *        ø«Œ≈≤ÛøÙ•∆°º•÷•Î
               05  SYUNYUIN-DAY-TBL2.
                   07  SYUNYUIN-DAY         PIC  X(01)
                                         OCCURS   31.
      *∆˛±°Õ˙ŒÚ»÷πÊ
           03  SYUNYUIN-NYUIN-RRKNUM        PIC  9(03).
      *∆˛±°§Ω§Œ¬æ£±§´§È£µ
           03  SYUNYUIN-NYUINETC-INF.
               05  SYUNYUIN-NYUINETC1       PIC S9(07).
               05  SYUNYUIN-NYUINETC2       PIC S9(07).
               05  SYUNYUIN-NYUINETC3       PIC S9(07).
               05  SYUNYUIN-NYUINETC4       PIC S9(07).
               05  SYUNYUIN-NYUINETC5       PIC S9(07).
      *
      *≈¡…º∫Ó¿Æ∂Ë ¨° ∏°∫˜Õ—°À° ∂ı«Ú°ß≥∞ÕË°°£±°ß¬‡±°ª˛∫Ó¿Æ°°£≤°ßƒÍ¥¸¿¡µ·
      *              £≥°ß∆˛±°•Ï•ª•◊•»Õ—•¿•ﬂ°º°À
           03  SYUNYUIN-SELCREATEKBN        PIC  X(01).
      *ø«Œ≈∆¸° ∏°∫˜Õ—°À
           03  SYUNYUIN-SELSRYYMD           PIC  X(08).
      *∆˛≥∞∂Ë ¨° ∏°∫˜Õ—°À
           03  SYUNYUIN-SELNYUGAIKBN        PIC  X(01).
