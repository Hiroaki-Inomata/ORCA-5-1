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
      *    º˝«ºπÁ∑◊•ﬁ•π•ø         COPY (CPSYUTOTAL.INC)       *
      ************************************************************
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      *  01.00.01    NACL-æÆ∆¶¬Ù   05/07/13  º´»Òπ‡Ã‹§Œƒ…≤√
      *                                      ∏∫Ã»∂‚≥€§Œƒ…≤√
      *                                       —¬ßΩı¿Æπ‡Ã‹§Œƒ…≤√
      *  01.00.02    NACL-æÆ∆¶¬Ù   06/07/06  …Ù Ã§Œπ‡Ã‹ƒ…≤√
      ************************************************************
      *01  SYUTTL-REC.
           03  SYUTTL-KEY.
      *∞ÂŒ≈µ°¥ÿ£…£ƒ
               05  SYUTTL-HOSPNUM          PIC 9(2).
      *∆˛≥∞∂Ë ¨
               05  SYUTTL-NYUGAIKBN       PIC  X(01).
      *¥µº‘£…£ƒ
               05  SYUTTL-PTID            PIC  9(10).
      *≈¡…º»÷πÊ
               05  SYUTTL-DENPNUM         PIC  9(07).
      *ø«Œ≈≤ ° πÁ∑◊ ¨§Œ∞Ÿ£∞£∞§«∏«ƒÍ°À
           03  SYUTTL-SRYKA               PIC  X(02).
      *∆±ª˛∆˛Œœø«Œ≈≤ 
           03  SYUTTL-DOUJI-SRYKA         PIC  X(02)
                                          OCCURS  10.
      *ø«Œ≈∆¸
           03  SYUTTL-SRYYMD              PIC  X(08).
      *≈¡…ºæı¬÷∂Ë ¨
           03  SYUTTL-DENPJTIKBN          PIC  X(01).
      *≈¡…º∫Ó¿Æ∂Ë ¨° ∂ı«Ú°ß≥∞ÕË°°£±°ß¬‡±°ª˛∫Ó¿Æ°°£≤°ßƒÍ¥¸¿¡µ·
      *              £≥°ß∆˛±°•Ï•ª•◊•»Õ—•¿•ﬂ°º°À
           03  SYUTTL-CREATEKBN           PIC  X(01).
      *≈¡…º»Øπ‘∆¸
           03  SYUTTL-DENPPRTYMD          PIC  X(08).
      *¿¡µ·¥¸¥÷
           03  SYUTTL-SKY-KIKAN.
      *¿¡µ·≥´ªœ∆¸
               05  SYUTTL-SKYSTYMD        PIC  X(08).
      *¿¡µ·Ω™Œª∆¸
               05  SYUTTL-SKYEDYMD        PIC  X(08).
      * ›∏±¡»πÁ§ª»÷πÊ
           03  SYUTTL-HKNCOMBINUM         PIC  X(04).
      *ºÁ ›∏±°› ›∏±»÷πÊ
           03  SYUTTL-SYUHKNNUM           PIC  X(03).
      *ºÁ ›∏±°›¥µº‘…È√¥∂‚≥€
           03  SYUTTL-SYUHKNFTNMONEY      PIC S9(07).
      *ºÁ ›∏±°›∑◊ªª…È√¥∂‚
           03  SYUTTL-SYUCOMPFTN          PIC S9(07).
      *ºÁ ›∏±°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
           03  SYUTTL-SYUCOMPFTN-ENTANI   PIC S9(07)V9(01).
      *ºÁ ›∏±°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
           03  SYUTTL-SYUCOMPYKZFTN       PIC S9(07).
      *ºÁ ›∏±°›∑◊ªª…È√¥∂‚∑◊
           03  SYUTTL-SYUCOMPTOTAL        PIC S9(07).
      *ºÁ ›∏±°›ÃÙ∫ﬁ…È√¥∂Ë ¨
           03  SYUTTL-SYUYKZFTNKBN        PIC  X(01).
      *∏¯»Ò°›…È√¥æ Û
           03  SYUTTL-KOH-AREA.
      *∏¯»Ò£±°› ›∏±»÷πÊ
               05  SYUTTL-KOH1HKNNUM          PIC  X(03).
      *∏¯»Ò£±°›¥µº‘…È√¥∂‚≥€
               05  SYUTTL-KOH1HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò£±°›∑◊ªª…È√¥∂‚
               05  SYUTTL-KOH1COMPFTN         PIC S9(07).
      *∏¯»Ò£±°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
               05  SYUTTL-KOH1COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò£±°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SYUTTL-KOH1COMPYKZFTN      PIC S9(07).
      *∏¯»Ò£±°›∑◊ªª…È√¥∂‚∑◊
               05  SYUTTL-KOH1COMPTOTAL       PIC S9(07).
      *∏¯»Ò£±°›ÃÙ∫ﬁ…È√¥∂Ë ¨
               05  SYUTTL-KOH1YKZFTNKBN       PIC  X(01).
      *∏¯»Ò£≤°› ›∏±»÷πÊ
               05  SYUTTL-KOH2HKNNUM          PIC  X(03).
      *∏¯»Ò£≤°›¥µº‘…È√¥∂‚≥€
               05  SYUTTL-KOH2HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò£≤°›∑◊ªª…È√¥∂‚
               05  SYUTTL-KOH2COMPFTN         PIC S9(07).
      *∏¯»Ò£≤°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
               05  SYUTTL-KOH2COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò£≤°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SYUTTL-KOH2COMPYKZFTN      PIC S9(07).
      *∏¯»Ò£≤°›∑◊ªª…È√¥∂‚∑◊
               05  SYUTTL-KOH2COMPTOTAL       PIC S9(07).
      *∏¯»Ò£≤°›ÃÙ∫ﬁ…È√¥∂Ë ¨
               05  SYUTTL-KOH2YKZFTNKBN       PIC  X(01).
      *∏¯»Ò£≥°› ›∏±»÷πÊ
               05  SYUTTL-KOH3HKNNUM          PIC  X(03).
      *∏¯»Ò£≥°›¥µº‘…È√¥∂‚≥€
               05  SYUTTL-KOH3HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò£≥°›∑◊ªª…È√¥∂‚
               05  SYUTTL-KOH3COMPFTN         PIC S9(07).
      *∏¯»Ò£≥°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
               05  SYUTTL-KOH3COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò£≥°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SYUTTL-KOH3COMPYKZFTN      PIC S9(07).
      *∏¯»Ò£≥°›∑◊ªª…È√¥∂‚∑◊
               05  SYUTTL-KOH3COMPTOTAL       PIC S9(07).
      *∏¯»Ò£≥°›ÃÙ∫ﬁ…È√¥∂Ë ¨
               05  SYUTTL-KOH3YKZFTNKBN       PIC  X(01).
      *∏¯»Ò£¥°› ›∏±»÷πÊ
               05  SYUTTL-KOH4HKNNUM          PIC  X(03).
      *∏¯»Ò£¥°›¥µº‘…È√¥∂‚≥€
               05  SYUTTL-KOH4HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò£¥°›∑◊ªª…È√¥∂‚
               05  SYUTTL-KOH4COMPFTN         PIC S9(07).
      *∏¯»Ò£¥°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
               05  SYUTTL-KOH4COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò£¥°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SYUTTL-KOH4COMPYKZFTN      PIC S9(07).
      *∏¯»Ò£¥°›∑◊ªª…È√¥∂‚∑◊
               05  SYUTTL-KOH4COMPTOTAL       PIC S9(07).
      *∏¯»Ò£¥°›ÃÙ∫ﬁ…È√¥∂Ë ¨
               05  SYUTTL-KOH4YKZFTNKBN       PIC  X(01).
      *∏¯»Ò…È√¥æ Û∫∆ƒÍµ¡
           03  SYUTTL-KOHFTN-TABLE  REDEFINES   SYUTTL-KOH-AREA.
               05  SYUTTL-KOHFTN-TBL          OCCURS  4.
      *∏¯»Ò°› ›∏±»÷πÊ
                 07  SYUTTL-KOH-HKNNUM          PIC  X(03).
      *∏¯»Ò°›¥µº‘…È√¥∂‚≥€
                 07  SYUTTL-KOH-HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò°›∑◊ªª…È√¥∂‚
                 07  SYUTTL-KOH-COMPFTN         PIC S9(07).
      *∏¯»Ò°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
                 07  SYUTTL-KOH-COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
                 07  SYUTTL-KOH-COMPYKZFTN      PIC S9(07).
      *∏¯»Ò°›∑◊ªª…È√¥∂‚∑◊
                 07  SYUTTL-KOH-COMPTOTAL       PIC S9(07).
      *∏¯»Ò°›ÃÙ∫ﬁ…È√¥∂Ë ¨
                 07  SYUTTL-KOH-YKZFTNKBN       PIC  X(01).
      *¥µº‘…È√¥Œ®
           03  SYUTTL-PTFTNRATE           PIC  9(03).
      *¿¡µ·ΩÒ∫Ó¿Æ∂Ë ¨
           03  SYUTTL-SKYKUKBN            PIC  X(01).
      *¿¡µ·∆‚Õ∆
           03  SYUTTL-SKY-NAIYOU.
               05  SYUTTL-SKY-TBL             OCCURS   17.
      *     ›∏±≈¿øÙ
                   09  SYUTTL-HKNTEN          PIC S9(07).
      *    ∂‚≥€
                   09  SYUTTL-MONEY           PIC S9(07).
      *    ≈¨Õ—≥∞∂‚≥€
                   09  SYUTTL-TGMONEY         PIC S9(07).
      *    ≈¨Õ—≥∞∂‚≥€° æ√»Ò¿«§¢§Í°À
                   09  SYUTTL-TGMONEY-TAX     PIC S9(07).
      *¿¡µ·∆‚Õ∆
           03  SYUTTL-SKY-NAIYOU-R        REDEFINES   SYUTTL-SKY-NAIYOU.
      *ø«Œ≈Œ¡(01)
              05  SYUTTL-SSU.
                  09  SYUTTL-SSU-HKNTEN          PIC S9(07).
                  09  SYUTTL-SSU-MONEY           PIC S9(07).
                  09  SYUTTL-SSU-TGMONEY         PIC S9(07).
                  09  SYUTTL-SSU-TGMONEY-TAX     PIC S9(07).
      *ªÿ∆≥Œ¡(02)
              05  SYUTTL-SDO.
                  09  SYUTTL-SDO-HKNTEN          PIC S9(07).
                  09  SYUTTL-SDO-MONEY           PIC S9(07).
                  09  SYUTTL-SDO-TGMONEY         PIC S9(07).
                  09  SYUTTL-SDO-TGMONEY-TAX     PIC S9(07).
      *∫ﬂ¬Œ¡(03)
              05  SYUTTL-ZTK.
                  09  SYUTTL-ZTK-HKNTEN          PIC S9(07).
                  09  SYUTTL-ZTK-MONEY           PIC S9(07).
                  09  SYUTTL-ZTK-TGMONEY         PIC S9(07).
                  09  SYUTTL-ZTK-TGMONEY-TAX     PIC S9(07).
      *≈ÍÃÙŒ¡(04)
              05  SYUTTL-TYK.
                  09  SYUTTL-TYK-HKNTEN          PIC S9(07).
                  09  SYUTTL-TYK-MONEY           PIC S9(07).
                  09  SYUTTL-TYK-TGMONEY         PIC S9(07).
                  09  SYUTTL-TYK-TGMONEY-TAX     PIC S9(07).
      *√ÌºÕŒ¡(05)
              05  SYUTTL-CSY.
                  09  SYUTTL-CSY-HKNTEN          PIC S9(07).
                  09  SYUTTL-CSY-MONEY           PIC S9(07).
                  09  SYUTTL-CSY-TGMONEY         PIC S9(07).
                  09  SYUTTL-CSY-TGMONEY-TAX     PIC S9(07).
      *ΩË√÷Œ¡(06)
              05  SYUTTL-SYC.
                  09  SYUTTL-SYC-HKNTEN          PIC S9(07).
                  09  SYUTTL-SYC-MONEY           PIC S9(07).
                  09  SYUTTL-SYC-TGMONEY         PIC S9(07).
                  09  SYUTTL-SYC-TGMONEY-TAX     PIC S9(07).
      *ºÍΩ—Œ¡(07)
              05  SYUTTL-SJT.
                  09  SYUTTL-SJT-HKNTEN          PIC S9(07).
                  09  SYUTTL-SJT-MONEY           PIC S9(07).
                  09  SYUTTL-SJT-TGMONEY         PIC S9(07).
                  09  SYUTTL-SJT-TGMONEY-TAX     PIC S9(07).
      *À„øÏ(08)
              05  SYUTTL-MSI.
                  09  SYUTTL-MSI-HKNTEN          PIC S9(07).
                  09  SYUTTL-MSI-MONEY           PIC S9(07).
                  09  SYUTTL-MSI-TGMONEY         PIC S9(07).
                  09  SYUTTL-MSI-TGMONEY-TAX     PIC S9(07).
      *∏°∫∫Œ¡(09)
              05  SYUTTL-KNS.
                  09  SYUTTL-KNS-HKNTEN          PIC S9(07).
                  09  SYUTTL-KNS-MONEY           PIC S9(07).
                  09  SYUTTL-KNS-TGMONEY         PIC S9(07).
                  09  SYUTTL-KNS-TGMONEY-TAX     PIC S9(07).
      *£ÿ¿˛Œ¡(10)
              05  SYUTTL-GZU.
                  09  SYUTTL-GZU-HKNTEN          PIC S9(07).
                  09  SYUTTL-GZU-MONEY           PIC S9(07).
                  09  SYUTTL-GZU-TGMONEY         PIC S9(07).
                  09  SYUTTL-GZU-TGMONEY-TAX     PIC S9(07).
      *•Í•œ(11)
              05  SYUTTL-ETC.
                  09  SYUTTL-ETC-HKNTEN          PIC S9(07).
                  09  SYUTTL-ETC-MONEY           PIC S9(07).
                  09  SYUTTL-ETC-TGMONEY         PIC S9(07).
                  09  SYUTTL-ETC-TGMONEY-TAX     PIC S9(07).
      *¿∫ø¿≤ ¿ÏÃÁŒ≈À°(12)
              05  SYUTTL-SSN.
                  09  SYUTTL-SSN-HKNTEN          PIC S9(07).
                  09  SYUTTL-SSN-MONEY           PIC S9(07).
                  09  SYUTTL-SSN-TGMONEY         PIC S9(07).
                  09  SYUTTL-SSN-TGMONEY-TAX     PIC S9(07).
      * ¸ºÕ¿˛(13)
              05  SYUTTL-HOU.
                  09  SYUTTL-HOU-HKNTEN          PIC S9(07).
                  09  SYUTTL-HOU-MONEY           PIC S9(07).
                  09  SYUTTL-HOU-TGMONEY         PIC S9(07).
                  09  SYUTTL-HOU-TGMONEY-TAX     PIC S9(07).
      *…¬Õ˝ø«√«(14)
              05  SYUTTL-BYR.
                  09  SYUTTL-BYR-HKNTEN          PIC S9(07).
                  09  SYUTTL-BYR-MONEY           PIC S9(07).
                  09  SYUTTL-BYR-TGMONEY         PIC S9(07).
                  09  SYUTTL-BYR-TGMONEY-TAX     PIC S9(07).
      *∆˛±°Œ¡(15)
              05  SYUTTL-NYN.
                  09  SYUTTL-NYN-HKNTEN          PIC S9(07).
                  09  SYUTTL-NYN-MONEY           PIC S9(07).
                  09  SYUTTL-NYN-TGMONEY         PIC S9(07).
                  09  SYUTTL-NYN-TGMONEY-TAX     PIC S9(07).
      *Œ≈Õ‹√¥≈ˆºÍ≈ˆ(16)
              05  SYUTTL-RYO.
                  09  SYUTTL-RYO-HKNTEN          PIC S9(07).
                  09  SYUTTL-RYO-MONEY           PIC S9(07).
                  09  SYUTTL-RYO-TGMONEY         PIC S9(07).
                  09  SYUTTL-RYO-TGMONEY-TAX     PIC S9(07).
      *πÁ∑◊  (17)
              05  SYUTTL-TOTAL.
                  09  SYUTTL-TOTAL-HKNTEN        PIC S9(07).
                  09  SYUTTL-TOTAL-MONEY         PIC S9(07).
                  09  SYUTTL-TOTAL-TGMONEY       PIC S9(07).
                  09  SYUTTL-TOTAL-TGMONEY-TAX   PIC S9(07).
      *ΩË ˝§ª§ÛŒ¡∫∆∑«
           03  SYUTTL-SHOHOU-SAI                 PIC S9(07).
      * ›∏±≈¨Õ—≥∞∂‚≥€°›æ√»Ò¿«° ∫∆∑«°À
           03  SYUTTL-TGMONEY-TAX-SAI         PIC S9(07).
      *º´»Ò£±§´§È£±£∞
           03  SYUTTL-JIHI-NAIYOU.
               05  SYUTTL-JIHI-1              PIC S9(07). 
               05  SYUTTL-JIHI-1-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-2              PIC S9(07). 
               05  SYUTTL-JIHI-2-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-3              PIC S9(07). 
               05  SYUTTL-JIHI-3-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-4              PIC S9(07). 
               05  SYUTTL-JIHI-4-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-5              PIC S9(07). 
               05  SYUTTL-JIHI-5-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-6              PIC S9(07). 
               05  SYUTTL-JIHI-6-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-7              PIC S9(07). 
               05  SYUTTL-JIHI-7-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-8              PIC S9(07). 
               05  SYUTTL-JIHI-8-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-9              PIC S9(07). 
               05  SYUTTL-JIHI-9-TAX          PIC S9(07). 
               05  SYUTTL-JIHI-10             PIC S9(07). 
               05  SYUTTL-JIHI-10-TAX         PIC S9(07). 
           03  SYUTTL-JIHI-NAIYOU-R           REDEFINES
                                              SYUTTL-JIHI-NAIYOU.
               05  SYUTTL-JIHI-TBL            OCCURS  10.
                 07  SYUTTL-JIHI-TAXNASI        PIC S9(07).
                 07  SYUTTL-JIHI-TAXARI         PIC S9(07).
      *º´»ÒæÆ∑◊° æ√»Ò¿«§ §∑°À
           03  SYUTTL-JIHI-TOTAL          PIC S9(07).
      *º´»ÒæÆ∑◊° æ√»Ò¿«§¢§Í°À
           03  SYUTTL-JIHI-TOTAL-TAX      PIC S9(07).
      *º´»Ò∂‚≥€æ√»Ò¿«
           03  SYUTTL-JIHI-TAX            PIC S9(07).
      *œ∑øÕ∞Ï…Ù…È√¥∂‚
           03  SYUTTL-RJN-FTN             PIC S9(07).
      *∏¯»Ò∞Ï…Ù…È√¥∂‚
           03  SYUTTL-KOH-FTN             PIC S9(07).
      *∏¯»Ò∞Ï…Ù…È√¥∂‚° ±ﬂ√±∞Ã°À
           03  SYUTTL-KOH-FTN-ENTANI      PIC S9(07)V9(01).
      *ƒ¥¿∞∂‚
           03  SYUTTL-CHOSEI              PIC S9(07).
           03  SYUTTL-CHOSEI-NAIYOU.
      *ƒ¥¿∞∂‚∆‚Ãı£±
               05  SYUTTL-CHOSEI1         PIC S9(07).
      *ƒ¥¿∞∂‚∆‚Ãı£≤
               05  SYUTTL-CHOSEI2         PIC S9(07).
           03  SYUTTL-CHOSEI-NAIYOU-R REDEFINES  SYUTTL-CHOSEI-NAIYOU.
               05  SYUTTL-RCHOSEI         PIC S9(07)
                                          OCCURS  2.
      *§ﬁ§»§·∆˛Œœ∫π≥€
           03  SYUTTL-GRP-SGKMONEY        PIC S9(07).
      * ›∏±≈¨Õ—∂‚≥€
           03  SYUTTL-HKNTEKMONEY         PIC S9(07).
      *…¬±°∏∫Ã»ªˆÕ≥∂Ë ¨
           03  SYUTTL-DISCOUNT-KBN        PIC  X(02).
      *…¬±°∏∫Ã»ªˆÕ≥
           03  SYUTTL-DISCOUNT-BODY       PIC  X(160).
      *≥‰∞˙Œ®∂Ë ¨
           03  SYUTTL-DISCOUNT-RATEKBN    PIC  X(02).
      *ƒÍŒ®ƒÍ≥€∂Ë ¨
           03  SYUTTL-DISCOUNT-TEIRITU    PIC  X(01).
      *≥‰∞˙Œ®
           03  SYUTTL-DISCOUNT-RATE       PIC S9(07).
      *∏∫Ã»∂‚≥€
           03  SYUTTL-DISCOUNT-MONEY      PIC S9(07).
      *œ´∫“ ›∏±°›ΩÈø«Œ¡
           03  SYUTTL-RSISHOSHIN-MONEY    PIC S9(07).
      *œ´∫“ ›∏±°›∫∆ø«Œ¡
           03  SYUTTL-RSISAISHIN-MONEY    PIC S9(07).
      *œ´∫“ ›∏±°›ªÿ∆≥Œ¡
           03  SYUTTL-RSISDO-MONEY        PIC S9(07).
      *œ´∫“ ›∏±°›§Ω§Œ¬æ
           03  SYUTTL-RSIETC-MONEY        PIC S9(07).
      *œ´∫“ ›∏±°›æ√»Ò¿«° ∫∆∑«°À
           03  SYUTTL-RSI-TAX-SAI         PIC S9(07).
      *œ´∫“ ›∏±°›¥µº‘¿¡µ· ¨±ﬂ∑˙§∆πÁ∑◊≥€
           03  SYUTTL-RSI-TOTAL           PIC S9(07).
      *œ´∫“º´«Â°›¿¡µ·∂‚≥€° ∆¸∑◊…Ωµ≠∫‹Õ—°À
           03  SYUTTL-RSIJIBAI-SKYMONEY   PIC S9(07).
      *¿¡µ·∂‚≥€°›æ√»Ò¿«° ∫∆∑«°À
           03  SYUTTL-SKYMONEY-TAX-SAI    PIC S9(07).
      *¿¡µ·∂‚≥€
           03  SYUTTL-SKYMONEY            PIC S9(07).
      *∆˛∂‚πÁ∑◊≥€
           03  SYUTTL-NYUKIN-TOTAL        PIC S9(07).
      *∆˛∂‚≤ÛøÙ
           03  SYUTTL-NYUKIN-KAISU        PIC S9(07).
      *Ã§º˝Õ˝Õ≥
           03  SYUTTL-MISYURIYU           PIC  X(02).
      *∫ﬂ¡Ìø«∂Ë ¨
           03  SYUTTL-ZAITAKU             PIC  X(01).
      *ø«Œ≈∂Ë ¨ ÃµÎ…’≥∞≈¿øÙ
           03  SYUTTL-KYUFUGAI.
               05  SYUTTL-KYUFUGAI-TBL          OCCURS   6.
      *    µÎ…’¬–æ›≥∞≈¿øÙ
                   09  SYUTTL-KYUFUGAI-TEN      PIC S9(07).
      *ø«Œ≈∂Ë ¨ ÃµÎ…’≥∞≈¿øÙ∫∆ƒÍµ¡
           03  SYUTTL-KYUFUGAI-R      REDEFINES   SYUTTL-KYUFUGAI.
      *µÎ…’¬–æ›≥∞≈¿øÙ° ΩÈø«°À
              05  SYUTTL-KYUFUGAI-SHOSHIN-TEN    PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° ªÿ∆≥°À
              05  SYUTTL-KYUFUGAI-SIDOU-TEN      PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° ±˝ø«°À
              05  SYUTTL-KYUFUGAI-OSIN1-TEN      PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° ±˝ø«°øÃÎ¥÷°À
              05  SYUTTL-KYUFUGAI-OSIN2-TEN      PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° ±˝ø«°øøºÃÎ°ø∂€µﬁ°À
              05  SYUTTL-KYUFUGAI-OSIN3-TEN      PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° πÁ∑◊°À
              05  SYUTTL-KYUFUGAI-GOKEI-TEN      PIC S9(07).
      * ›∏±≈¨Õ—∂Ë ¨
           03  SYUTTL-HKNTEKKBN           PIC  X(01).
      *√ºÀˆ£…£ƒ
           03  SYUTTL-TERMID              PIC  X(16).
      *•™•⁄°º•Ï°º•ø£…£ƒ
           03  SYUTTL-OPID                PIC  X(16).
      *∫Ó¿Æ«Ø∑Ó∆¸
           03  SYUTTL-CREYMD              PIC  X(08).
      *ππø∑«Ø∑Ó∆¸
           03  SYUTTL-UPYMD               PIC  X(08).
      *ππø∑ª˛¥÷
           03  SYUTTL-UPHMS               PIC  X(06).
      *
