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
      *    ø∑º˝«º•ﬁ•π•ø         COPY (CPSYUMAIN.INC)             *
      ************************************************************
      ************************************************************
      *  •≥•‘°º∂ÁΩ§¿µÕ˙ŒÚ
      * Maj/Min/Rev  Ω§¿µº‘       ∆¸…’      ∆‚Õ∆
      ************************************************************
      *01  SYUMAIN-REC.
           03  SYUMAIN-KEY.
      *∞ÂŒ≈µ°¥ÿ£…£ƒ
               05  SYUMAIN-HOSPNUM          PIC 9(2).
      *∆˛≥∞∂Ë ¨
               05  SYUMAIN-NYUGAIKBN       PIC  X(01).
      *¥µº‘£…£ƒ
               05  SYUMAIN-PTID            PIC  9(10).
      *≈¡…º»÷πÊ
               05  SYUMAIN-DENPNUM         PIC  9(07).
      *≈¡…ºªﬁ»÷∫«Ω™»÷πÊ
           03  SYUMAIN-DENPLASTNUM         PIC  9(02).
      *ø«Œ≈≤ 
           03  SYUMAIN-SRYKA               PIC  X(02).
      *ø«Œ≈∆¸
           03  SYUMAIN-SRYYMD              PIC  X(08).
      *≈¡…ºæı¬÷∂Ë ¨
           03  SYUMAIN-DENPJTIKBN          PIC  X(01).
      *≈¡…º∫Ó¿Æ∂Ë ¨° ∂ı«Ú°ß≥∞ÕË°°£±°ß¬‡±°ª˛∫Ó¿Æ°°£≤°ßƒÍ¥¸¿¡µ·
      *              £≥°ß∆˛±°•Ï•ª•◊•»Õ—•¿•ﬂ°º°À
           03  SYUMAIN-CREATEKBN           PIC  X(01).
      *≈¡…º»Øπ‘∆¸
           03  SYUMAIN-DENPPRTYMD          PIC  X(08).
      *¿¡µ·¥¸¥÷
           03  SYUMAIN-SKY-KIKAN.
      *¿¡µ·≥´ªœ∆¸
               05  SYUMAIN-SKYSTYMD        PIC  X(08).
      *¿¡µ·Ω™Œª∆¸
               05  SYUMAIN-SKYEDYMD        PIC  X(08).
      * ›∏±¡»πÁ§ª»÷πÊ
           03  SYUMAIN-HKNCOMBINUM         PIC  X(04).
      *ºÁ ›∏±°› ›∏±»÷πÊ
           03  SYUMAIN-SYUHKNNUM           PIC  X(03).
      *ºÁ ›∏±°›¥µº‘…È√¥∂‚≥€
           03  SYUMAIN-SYUHKNFTNMONEY      PIC S9(07).
      *ºÁ ›∏±°›∑◊ªª…È√¥∂‚
           03  SYUMAIN-SYUCOMPFTN          PIC S9(07).
      *ºÁ ›∏±°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
           03  SYUMAIN-SYUCOMPFTN-ENTANI   PIC S9(07)V9(01).
      *ºÁ ›∏±°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
           03  SYUMAIN-SYUCOMPYKZFTN       PIC S9(07).
      *ºÁ ›∏±°›∑◊ªª…È√¥∂‚∑◊
           03  SYUMAIN-SYUCOMPTOTAL        PIC S9(07).
      *ºÁ ›∏±°›ÃÙ∫ﬁ…È√¥∂Ë ¨
           03  SYUMAIN-SYUYKZFTNKBN        PIC  X(01).
      *∏¯»Ò°›…È√¥æ Û
           03  SYUMAIN-KOH-AREA.
      *∏¯»Ò£±°› ›∏±»÷πÊ
               05  SYUMAIN-KOH1HKNNUM          PIC  X(03).
      *∏¯»Ò£±°›¥µº‘…È√¥∂‚≥€
               05  SYUMAIN-KOH1HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò£±°›∑◊ªª…È√¥∂‚
               05  SYUMAIN-KOH1COMPFTN         PIC S9(07).
      *∏¯»Ò£±°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
               05  SYUMAIN-KOH1COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò£±°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SYUMAIN-KOH1COMPYKZFTN      PIC S9(07).
      *∏¯»Ò£±°›∑◊ªª…È√¥∂‚∑◊
               05  SYUMAIN-KOH1COMPTOTAL       PIC S9(07).
      *∏¯»Ò£±°›ÃÙ∫ﬁ…È√¥∂Ë ¨
               05  SYUMAIN-KOH1YKZFTNKBN       PIC  X(01).
      *∏¯»Ò£≤°› ›∏±»÷πÊ
               05  SYUMAIN-KOH2HKNNUM          PIC  X(03).
      *∏¯»Ò£≤°›¥µº‘…È√¥∂‚≥€
               05  SYUMAIN-KOH2HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò£≤°›∑◊ªª…È√¥∂‚
               05  SYUMAIN-KOH2COMPFTN         PIC S9(07).
      *∏¯»Ò£≤°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
               05  SYUMAIN-KOH2COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò£≤°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SYUMAIN-KOH2COMPYKZFTN      PIC S9(07).
      *∏¯»Ò£≤°›∑◊ªª…È√¥∂‚∑◊
               05  SYUMAIN-KOH2COMPTOTAL       PIC S9(07).
      *∏¯»Ò£≤°›ÃÙ∫ﬁ…È√¥∂Ë ¨
               05  SYUMAIN-KOH2YKZFTNKBN       PIC  X(01).
      *∏¯»Ò£≥°› ›∏±»÷πÊ
               05  SYUMAIN-KOH3HKNNUM          PIC  X(03).
      *∏¯»Ò£≥°›¥µº‘…È√¥∂‚≥€
               05  SYUMAIN-KOH3HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò£≥°›∑◊ªª…È√¥∂‚
               05  SYUMAIN-KOH3COMPFTN         PIC S9(07).
      *∏¯»Ò£≥°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
               05  SYUMAIN-KOH3COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò£≥°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SYUMAIN-KOH3COMPYKZFTN      PIC S9(07).
      *∏¯»Ò£≥°›∑◊ªª…È√¥∂‚∑◊
               05  SYUMAIN-KOH3COMPTOTAL       PIC S9(07).
      *∏¯»Ò£≥°›ÃÙ∫ﬁ…È√¥∂Ë ¨
               05  SYUMAIN-KOH3YKZFTNKBN       PIC  X(01).
      *∏¯»Ò£¥°› ›∏±»÷πÊ
               05  SYUMAIN-KOH4HKNNUM          PIC  X(03).
      *∏¯»Ò£¥°›¥µº‘…È√¥∂‚≥€
               05  SYUMAIN-KOH4HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò£¥°›∑◊ªª…È√¥∂‚
               05  SYUMAIN-KOH4COMPFTN         PIC S9(07).
      *∏¯»Ò£¥°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
               05  SYUMAIN-KOH4COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò£¥°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
               05  SYUMAIN-KOH4COMPYKZFTN      PIC S9(07).
      *∏¯»Ò£¥°›∑◊ªª…È√¥∂‚∑◊
               05  SYUMAIN-KOH4COMPTOTAL       PIC S9(07).
      *∏¯»Ò£¥°›ÃÙ∫ﬁ…È√¥∂Ë ¨
               05  SYUMAIN-KOH4YKZFTNKBN       PIC  X(01).
      *∏¯»Ò…È√¥æ Û∫∆ƒÍµ¡
           03  SYUMAIN-KOHFTN-TABLE  REDEFINES   SYUMAIN-KOH-AREA.
               05  SYUMAIN-KOHFTN-TBL          OCCURS  4.
      *∏¯»Ò°› ›∏±»÷πÊ
                 07  SYUMAIN-KOH-HKNNUM          PIC  X(03).
      *∏¯»Ò°›¥µº‘…È√¥∂‚≥€
                 07  SYUMAIN-KOH-HKNFTNMONEY     PIC S9(07).
      *∏¯»Ò°›∑◊ªª…È√¥∂‚
                 07  SYUMAIN-KOH-COMPFTN         PIC S9(07).
      *∏¯»Ò°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
                 07  SYUMAIN-KOH-COMPFTN-ENTANI  PIC S9(07)V9(01).
      *∏¯»Ò°›∑◊ªªÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
                 07  SYUMAIN-KOH-COMPYKZFTN      PIC S9(07).
      *∏¯»Ò°›∑◊ªª…È√¥∂‚∑◊
                 07  SYUMAIN-KOH-COMPTOTAL       PIC S9(07).
      *∏¯»Ò°›ÃÙ∫ﬁ…È√¥∂Ë ¨
                 07  SYUMAIN-KOH-YKZFTNKBN       PIC  X(01).
      *¥µº‘…È√¥Œ®
           03  SYUMAIN-PTFTNRATE           PIC  9(03).
      *¿¡µ·ΩÒ∫Ó¿Æ∂Ë ¨
           03  SYUMAIN-SKYKUKBN            PIC  X(01).
      *æ√»Ò¿«¬–æ›∂‚≥€
           03  SYUMAIN-TAX-TAISHOU         PIC S9(07).
      *æ√»Ò¿«° ∆‚¿«°À
           03  SYUMAIN-TAX-MONEY           PIC S9(07).
      *æ√»Ò¿«° πÁ∑◊°À
           03  SYUMAIN-SKYGK               PIC S9(07).
      *¿¡µ·∆‚Õ∆
           03  SYUMAIN-SKY-NAIYOU.
               05  SYUMAIN-SKY-TBL             OCCURS   17.
      *     ›∏±≈¿øÙ
                   09  SYUMAIN-HKNTEN          PIC S9(07).
      *    ∂‚≥€
                   09  SYUMAIN-MONEY           PIC S9(07).
      *    ≈¨Õ—≥∞∂‚≥€
                   09  SYUMAIN-TGMONEY         PIC S9(07).
      *    ≈¨Õ—≥∞∂‚≥€° æ√»Ò¿«§¢§Í°À
                   09  SYUMAIN-TGMONEY-TAX     PIC S9(07).
      *¿¡µ·∆‚Õ∆
           03  SYUMAIN-SKY-NAIYOU-R    REDEFINES   SYUMAIN-SKY-NAIYOU.
      *ø«Œ≈Œ¡(01)
              05  SYUMAIN-SSU.
                  09  SYUMAIN-SSU-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SSU-MONEY           PIC S9(07).
                  09  SYUMAIN-SSU-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SSU-TGMONEY-TAX     PIC S9(07).
      *ªÿ∆≥Œ¡(02)
              05  SYUMAIN-SDO.
                  09  SYUMAIN-SDO-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SDO-MONEY           PIC S9(07).
                  09  SYUMAIN-SDO-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SDO-TGMONEY-TAX     PIC S9(07).
      *∫ﬂ¬Œ¡(03)
              05  SYUMAIN-ZTK.
                  09  SYUMAIN-ZTK-HKNTEN          PIC S9(07).
                  09  SYUMAIN-ZTK-MONEY           PIC S9(07).
                  09  SYUMAIN-ZTK-TGMONEY         PIC S9(07).
                  09  SYUMAIN-ZTK-TGMONEY-TAX     PIC S9(07).
      *≈ÍÃÙŒ¡(04)
              05  SYUMAIN-TYK.
                  09  SYUMAIN-TYK-HKNTEN          PIC S9(07).
                  09  SYUMAIN-TYK-MONEY           PIC S9(07).
                  09  SYUMAIN-TYK-TGMONEY         PIC S9(07).
                  09  SYUMAIN-TYK-TGMONEY-TAX     PIC S9(07).
      *√ÌºÕŒ¡(05)
              05  SYUMAIN-CSY.
                  09  SYUMAIN-CSY-HKNTEN          PIC S9(07).
                  09  SYUMAIN-CSY-MONEY           PIC S9(07).
                  09  SYUMAIN-CSY-TGMONEY         PIC S9(07).
                  09  SYUMAIN-CSY-TGMONEY-TAX     PIC S9(07).
      *ΩË√÷Œ¡(06)
              05  SYUMAIN-SYC.
                  09  SYUMAIN-SYC-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SYC-MONEY           PIC S9(07).
                  09  SYUMAIN-SYC-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SYC-TGMONEY-TAX     PIC S9(07).
      *ºÍΩ—Œ¡(07)
              05  SYUMAIN-SJT.
                  09  SYUMAIN-SJT-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SJT-MONEY           PIC S9(07).
                  09  SYUMAIN-SJT-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SJT-TGMONEY-TAX     PIC S9(07).
      *À„øÏ(08)
              05  SYUMAIN-MSI.
                  09  SYUMAIN-MSI-HKNTEN          PIC S9(07).
                  09  SYUMAIN-MSI-MONEY           PIC S9(07).
                  09  SYUMAIN-MSI-TGMONEY         PIC S9(07).
                  09  SYUMAIN-MSI-TGMONEY-TAX     PIC S9(07).
      *∏°∫∫Œ¡(09)
              05  SYUMAIN-KNS.
                  09  SYUMAIN-KNS-HKNTEN          PIC S9(07).
                  09  SYUMAIN-KNS-MONEY           PIC S9(07).
                  09  SYUMAIN-KNS-TGMONEY         PIC S9(07).
                  09  SYUMAIN-KNS-TGMONEY-TAX     PIC S9(07).
      *£ÿ¿˛Œ¡(10)
              05  SYUMAIN-GZU.
                  09  SYUMAIN-GZU-HKNTEN          PIC S9(07).
                  09  SYUMAIN-GZU-MONEY           PIC S9(07).
                  09  SYUMAIN-GZU-TGMONEY         PIC S9(07).
                  09  SYUMAIN-GZU-TGMONEY-TAX     PIC S9(07).
      *•Í•œ(11)
              05  SYUMAIN-ETC.
                  09  SYUMAIN-ETC-HKNTEN          PIC S9(07).
                  09  SYUMAIN-ETC-MONEY           PIC S9(07).
                  09  SYUMAIN-ETC-TGMONEY         PIC S9(07).
                  09  SYUMAIN-ETC-TGMONEY-TAX     PIC S9(07).
      *¿∫ø¿≤ ¿ÏÃÁŒ≈À°(12)
              05  SYUMAIN-SSN.
                  09  SYUMAIN-SSN-HKNTEN          PIC S9(07).
                  09  SYUMAIN-SSN-MONEY           PIC S9(07).
                  09  SYUMAIN-SSN-TGMONEY         PIC S9(07).
                  09  SYUMAIN-SSN-TGMONEY-TAX     PIC S9(07).
      * ¸ºÕ¿˛(13)
              05  SYUMAIN-HOU.
                  09  SYUMAIN-HOU-HKNTEN          PIC S9(07).
                  09  SYUMAIN-HOU-MONEY           PIC S9(07).
                  09  SYUMAIN-HOU-TGMONEY         PIC S9(07).
                  09  SYUMAIN-HOU-TGMONEY-TAX     PIC S9(07).
      *…¬Õ˝ø«√«(14)
              05  SYUMAIN-BYR.
                  09  SYUMAIN-BYR-HKNTEN          PIC S9(07).
                  09  SYUMAIN-BYR-MONEY           PIC S9(07).
                  09  SYUMAIN-BYR-TGMONEY         PIC S9(07).
                  09  SYUMAIN-BYR-TGMONEY-TAX     PIC S9(07).
      *∆˛±°Œ¡(15)
              05  SYUMAIN-NYN.
                  09  SYUMAIN-NYN-HKNTEN          PIC S9(07).
                  09  SYUMAIN-NYN-MONEY           PIC S9(07).
                  09  SYUMAIN-NYN-TGMONEY         PIC S9(07).
                  09  SYUMAIN-NYN-TGMONEY-TAX     PIC S9(07).
      *Œ≈Õ‹√¥≈ˆºÍ≈ˆ(16)
              05  SYUMAIN-RYO.
                  09  SYUMAIN-RYO-HKNTEN          PIC S9(07).
                  09  SYUMAIN-RYO-MONEY           PIC S9(07).
                  09  SYUMAIN-RYO-TGMONEY         PIC S9(07).
                  09  SYUMAIN-RYO-TGMONEY-TAX     PIC S9(07).
      *πÁ∑◊  (17)
              05  SYUMAIN-TOTAL.
                  09  SYUMAIN-TOTAL-HKNTEN        PIC S9(07).
                  09  SYUMAIN-TOTAL-MONEY         PIC S9(07).
                  09  SYUMAIN-TOTAL-TGMONEY       PIC S9(07).
                  09  SYUMAIN-TOTAL-TGMONEY-TAX   PIC S9(07).
      *ΩË ˝§ª§ÛŒ¡∫∆∑«
           03  SYUMAIN-SHOHOU-SAI                 PIC S9(07).
      * ›∏±≈¨Õ—≥∞∂‚≥€°›æ√»Ò¿«° ∫∆∑«°À
           03  SYUMAIN-TGMONEY-TAX-SAI         PIC S9(07).
      *º´»Ò£±§´§È£±£∞
           03  SYUMAIN-JIHI-NAIYOU.
               05  SYUMAIN-JIHI-1              PIC S9(07). 
               05  SYUMAIN-JIHI-1-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-2              PIC S9(07). 
               05  SYUMAIN-JIHI-2-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-3              PIC S9(07). 
               05  SYUMAIN-JIHI-3-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-4              PIC S9(07). 
               05  SYUMAIN-JIHI-4-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-5              PIC S9(07). 
               05  SYUMAIN-JIHI-5-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-6              PIC S9(07). 
               05  SYUMAIN-JIHI-6-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-7              PIC S9(07). 
               05  SYUMAIN-JIHI-7-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-8              PIC S9(07). 
               05  SYUMAIN-JIHI-8-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-9              PIC S9(07). 
               05  SYUMAIN-JIHI-9-TAX          PIC S9(07). 
               05  SYUMAIN-JIHI-10             PIC S9(07). 
               05  SYUMAIN-JIHI-10-TAX         PIC S9(07). 
           03  SYUMAIN-JIHI-NAIYOU-R   REDEFINES  SYUMAIN-JIHI-NAIYOU.
               05  SYUMAIN-JIHI-TBL            OCCURS   10.
                 07  SYUMAIN-JIHI-TAXNASI        PIC S9(07).
                 07  SYUMAIN-JIHI-TAXARI         PIC S9(07).
      *º´»ÒæÆ∑◊° æ√»Ò¿«§ §∑°À
           03  SYUMAIN-JIHI-TOTAL          PIC S9(07).
      *º´»ÒæÆ∑◊° æ√»Ò¿«§¢§Í°À
           03  SYUMAIN-JIHI-TOTAL-TAX      PIC S9(07).
      *º´»Ò∂‚≥€æ√»Ò¿«
           03  SYUMAIN-JIHI-TAX            PIC S9(07).
      *œ∑øÕ∞Ï…Ù…È√¥∂‚
           03  SYUMAIN-RJN-FTN             PIC S9(07).
      *∏¯»Ò∞Ï…Ù…È√¥∂‚
           03  SYUMAIN-KOH-FTN             PIC S9(07).
      *∏¯»Ò∞Ï…Ù…È√¥∂‚° ±ﬂ√±∞Ã°À
           03  SYUMAIN-KOH-FTN-ENTANI      PIC S9(07)V9(01).
      *ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
           03  SYUMAIN-YKZ-FTN             PIC S9(07).
      *•Ï•ª•◊•»Ω–ŒœÕ—ÃÙ∫ﬁ∞Ï…Ù…È√¥∂‚
           03  SYUMAIN-RESE-YKZ-FTN        PIC S9(07).
      *∏¯»Ò¬–æ›ÃÙ∫ﬁ∂Ë ¨
           03  SYUMAIN-KOHTAIYKZKBN        PIC  X(01).
      *ƒ¥¿∞∂‚
           03  SYUMAIN-CHOSEI              PIC S9(07).
           03  SYUMAIN-CHOSEI-NAIYOU.
      *ƒ¥¿∞∂‚∆‚Ãı£±
               05  SYUMAIN-CHOSEI1         PIC S9(07).
      *ƒ¥¿∞∂‚∆‚Ãı£≤
               05  SYUMAIN-CHOSEI2         PIC S9(07).
           03  SYUMAIN-CHOSEI-NAIYOU-R REDEFINES  SYUMAIN-CHOSEI-NAIYOU.
               05  SYUMAIN-RCHOSEI         PIC S9(07)
                                           OCCURS  2.
      *§ﬁ§»§·∆˛Œœ≈¡…º»÷πÊ
           03  SYUMAIN-GRP-DENPNUM         PIC 9(07).
      *§ﬁ§»§·∆˛Œœœ¢»÷
           03  SYUMAIN-GRP-RENNUM          PIC 9(02).
      *§ﬁ§»§·∆˛Œœ∫π≥€
           03  SYUMAIN-GRP-SGKMONEY        PIC S9(07).
      *§ﬁ§»§·»Øπ‘ ˝À°•’•È•∞
           03  SYUMAIN-GRP-HAKHOUFLG       PIC  9(01).
      * ›æ⁄∂‚≥€
           03  SYUMAIN-SECMONEY            PIC S9(07).
      * ›∏±≈¨Õ—∂‚≥€
           03  SYUMAIN-HKNTEKMONEY         PIC S9(07).
      *…¬±°∏∫Ã»ªˆÕ≥∂Ë ¨
           03  SYUMAIN-DISCOUNT-KBN        PIC  X(02).
      *…¬±°∏∫Ã»ªˆÕ≥
           03  SYUMAIN-DISCOUNT-BODY       PIC  X(160).
      *≥‰∞˙Œ®∂Ë ¨
           03  SYUMAIN-DISCOUNT-RATEKBN    PIC  X(02).
      *ƒÍŒ®ƒÍ≥€∂Ë ¨
           03  SYUMAIN-DISCOUNT-TEIRITU    PIC  X(01).
      *≥‰∞˙Œ®
           03  SYUMAIN-DISCOUNT-RATE       PIC S9(07).
      *∏∫Ã»∂‚≥€
           03  SYUMAIN-DISCOUNT-MONEY      PIC S9(07).
      *œ´∫“ ›∏±°›ΩÈø«Œ¡
           03  SYUMAIN-RSISHOSHIN-MONEY    PIC S9(07).
      *œ´∫“ ›∏±°›∫∆ø«Œ¡
           03  SYUMAIN-RSISAISHIN-MONEY    PIC S9(07).
      *œ´∫“ ›∏±°›ªÿ∆≥Œ¡
           03  SYUMAIN-RSISDO-MONEY        PIC S9(07).
      *œ´∫“ ›∏±°›§Ω§Œ¬æ
           03  SYUMAIN-RSIETC-MONEY        PIC S9(07).
      *œ´∫“ ›∏±°›æ√»Ò¿«° ∫∆∑«°À
           03  SYUMAIN-RSI-TAX-SAI         PIC S9(07).
      *œ´∫“ ›∏±°›¥µº‘¿¡µ· ¨±ﬂ∑˙§∆πÁ∑◊≥€
           03  SYUMAIN-RSI-TOTAL           PIC S9(07).
      *œ´∫“º´«Â°›¿¡µ·∂‚≥€° ∆¸∑◊…Ωµ≠∫‹Õ—°À
           03  SYUMAIN-RSIJIBAI-SKYMONEY   PIC S9(07).
      *¿¡µ·∂‚≥€°›æ√»Ò¿«° ∫∆∑«°À
           03  SYUMAIN-SKYMONEY-TAX-SAI    PIC S9(07).
      *¿¡µ·∂‚≥€
           03  SYUMAIN-SKYMONEY            PIC S9(07).
      *∆˛∂‚πÁ∑◊≥€
           03  SYUMAIN-NYUKIN-TOTAL        PIC S9(07).
      *∆˛∂‚≤ÛøÙ
           03  SYUMAIN-NYUKIN-KAISU        PIC S9(07).
      *Ã§º˝Õ˝Õ≥
           03  SYUMAIN-MISYURIYU           PIC  X(02).
      *•…•Ø•ø°º
           03  SYUMAIN-DRCD-G.
               05  SYUMAIN-DRCD-SIK            PIC  X(01).
               05  SYUMAIN-DRCD                PIC  X(04).
      *∆˛±°ΩÈø«≤ÛøÙ
           03  SYUMAIN-NYNSHOKAISU         PIC S9(02).
      *ÃÙ∫ﬁ»Ø∑Ù»÷πÊ
           03  SYUMAIN-YKZKENNUM           PIC  9(05).
      *ø©ªˆ…È√¥° …È√¥≥€ Ã¥…Õ˝Õ—°À
           03  SYUMAIN-SKJ-PTFTN           OCCURS 3.
               05  SYUMAIN-SKJ-FTNGAKU     PIC S9(05).
               05  SYUMAIN-SKJ-FTNDAY      PIC S9(02).
      *∫∆∑◊ªª∂Ë ¨
           03  SYUMAIN-SAIKEISANKBN        PIC  X(01).
      *±°≥∞ΩË ˝∂Ë ¨
           03  SYUMAIN-INGAISHOHOKBN       PIC  X(01).
      *∆±ª˛ø«Œ≈≈¡…º»÷πÊ
           03  SYUMAIN-DOUJI-DENPNUM       PIC  9(07).
      *∑—¬≥∂Ë ¨
           03  SYUMAIN-CONTKBN             PIC  X(01).
      * £øÙ≤ §ﬁ§»§·º± Ã≈¡…º»÷πÊ
           03  SYUMAIN-FUKU-DENPNUM        PIC  9(07).
      * £øÙ≤ §ﬁ§»§·∂Ë ¨° £±°ß∑—¬≥°°£≤°ß≥ŒƒÍ°À
           03  SYUMAIN-FUKU-KBN            PIC  X(01).
      *∫ﬂ¡Ìø«∂Ë ¨
           03  SYUMAIN-ZAITAKU             PIC  X(01).
      *ø«Œ≈∂Ë ¨ ÃµÎ…’≥∞≈¿øÙ
           03  SYUMAIN-KYUFUGAI.
               05  SYUMAIN-KYUFUGAI-TBL          OCCURS   6.
      *    µÎ…’¬–æ›≥∞≈¿øÙ
                   09  SYUMAIN-KYUFUGAI-TEN      PIC S9(07).
      *ø«Œ≈∂Ë ¨ ÃµÎ…’≥∞≈¿øÙ∫∆ƒÍµ¡
           03  SYUMAIN-KYUFUGAI-R      REDEFINES   SYUMAIN-KYUFUGAI.
      *µÎ…’¬–æ›≥∞≈¿øÙ° ΩÈø«°À
              05  SYUMAIN-KYUFUGAI-SHOSHIN-TEN    PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° ªÿ∆≥°À
              05  SYUMAIN-KYUFUGAI-SIDOU-TEN      PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° ±˝ø«°À
              05  SYUMAIN-KYUFUGAI-OSIN1-TEN      PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° ±˝ø«°øÃÎ¥÷°À
              05  SYUMAIN-KYUFUGAI-OSIN2-TEN      PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° ±˝ø«°øøºÃÎ°ø∂€µﬁ°À
              05  SYUMAIN-KYUFUGAI-OSIN3-TEN      PIC S9(07).
      *µÎ…’¬–æ›≥∞≈¿øÙ° πÁ∑◊°À
              05  SYUMAIN-KYUFUGAI-GOKEI-TEN      PIC S9(07).
      *∆¸ Ãº˝«º•’•È•∞
           03  SYUMAIN-DAYINFFLG           PIC  X(01).
      * ¨ ⁄¥…Õ˝•’•È•∞
           03  SYUMAIN-BUNBENINFFLG        PIC  X(01).
      *≤Ò∑◊ —ππ∂Ë ¨
           03  SYUMAIN-ACCT-UPDKBN         PIC  X(01).
      * —¬ßΩı¿Æ°›¥µº‘…È√¥∂‚≥€
           03  SYUMAIN-JYO-HKNFTNMONEY     PIC S9(07).
      * —¬ßΩı¿Æ°›∑◊ªª…È√¥∂‚
           03  SYUMAIN-JYO-COMPFTN         PIC S9(07).
      * —¬ßΩı¿Æ°›∑◊ªª…È√¥∂‚° ±ﬂ√±∞Ãµ≠œøÕ—°À
           03  SYUMAIN-JYO-COMPFTN-ENTANI  PIC S9(07)V9(01).
      *¿¡µ·∆˛∂‚∂Ë ¨° £∞°ß¿¡µ·∂‚≥€°·∆˛∂‚πÁ∑◊≥€°ø£±°ß¿¡µ·∂‚≥€°‚∆˛∂‚πÁ∑◊≥€°À
           03  SYUMAIN-SKYNYUKINKBN        PIC  X(01).
      * ›∏±≈¨Õ—∂Ë ¨
           03  SYUMAIN-HKNTEKKBN           PIC  X(01).
      *√ºÀˆ£…£ƒ
           03  SYUMAIN-TERMID              PIC  X(16).
      *•™•⁄°º•Ï°º•ø£…£ƒ
           03  SYUMAIN-OPID                PIC  X(16).
      *∫Ó¿Æ«Ø∑Ó∆¸
           03  SYUMAIN-CREYMD              PIC  X(08).
      *ππø∑«Ø∑Ó∆¸
           03  SYUMAIN-UPYMD               PIC  X(08).
      *ππø∑ª˛¥÷
           03  SYUMAIN-UPHMS               PIC  X(06).
      *
      *ø«Œ≈«Ø∑Ó∆¸° ∏°∫˜Õ—°À
           03  SYUMAIN-UPSRYYMD            PIC  X(08).
      *
      *∆˛±°Õ˙ŒÚ»÷πÊ
           03  SYUMAIN-SELNYUIN-RRKNUM     PIC  9(03).
      *ø«Œ≈«Ø∑Ó∆¸° ∏°∫˜Õ—°À
           03  SYUMAIN-SELKOHID            PIC  9(10).
      *∆˛≥∞∂Ë ¨° ∏°∫˜Õ—°À
           03  SYUMAIN-SELNYUGAIKBN        PIC  X(01).
