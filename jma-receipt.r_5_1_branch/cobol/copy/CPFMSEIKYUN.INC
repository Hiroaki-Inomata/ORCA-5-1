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
        01  FMSEIKYUN-REC.
            03  FMSEIKYUN-CONTEXT              PIC S9(9)   BINARY.
            03  FMSEIKYUN-OBJECT               PIC X(36).
            03  FMSEIKYUN-MODE                 PIC S9(9)   BINARY.
            03  FMSEIKYUN-RECORDNAME           PIC X(128).
            03  FMSEIKYUN-DATA.
              05  FMSEIKYUN-FORM-ID            PIC X(30).
              05  FMSEIKYUN-CUSTOM-ID          PIC X(30).
              05  FMSEIKYUN-PRINTER            PIC X(20).
      *
              05  FMSEIKYUN-MOTOID             PIC X(02).
              05  FMSEIKYUN-KARIKEISANKBN      PIC X(01).
      *
      *       ¥µº‘»÷πÊ
              05  FMSEIKYUN-PTNUM              PIC X(20).
      *       ¥µº‘ª·Ãæ
              05  FMSEIKYUN-NAME               PIC X(50).
      *       ¥µº‘•´• ª·Ãæ
              05  FMSEIKYUN-KANANAME           PIC X(50).
      *
      *       ∞ÂŒ≈µ°¥ÿæ Û
              05  FMSEIKYUN-HOSP-AREA.
      *         ∞ÂŒ≈µ°¥ÿÃææŒ
                07  FMSEIKYUN-HOSP-NAME-G.
                  09  FMSEIKYUN-HOSP-NAME      PIC X(120)
                                               OCCURS   3.
      *         ∞ÂŒ≈µ°¥ÿÕπ ÿ»÷πÊ
                07  FMSEIKYUN-HOSP-POST        PIC X(7).
      *         ∞ÂŒ≈µ°¥ÿΩªΩÍ
                07  FMSEIKYUN-HOSP-ADRS-G.
                  09  FMSEIKYUN-HOSP-ADRS      PIC X(200)
                                               OCCURS   3.
      *         ∞ÂŒ≈µ°¥ÿ≈≈œ√»÷πÊ
                07  FMSEIKYUN-HOSP-TEL         PIC X(15).
      *       ≥´ªœø«Œ≈∆¸
              05  FMSEIKYUN-STSRYYMD           PIC X(10).
      *       Ω™Œªø«Œ≈∆¸
              05  FMSEIKYUN-EDSRYYMD           PIC X(10).
      *       ≈¡…º»Øπ‘∆¸
              05  FMSEIKYUN-DENPPRTYMD         PIC X(10).
      *       …¬≈Ô»÷πÊ
              05  FMSEIKYUN-BTUNUM             PIC X(2).
      *       …¬≈ÔÃææŒ
              05  FMSEIKYUN-BTUNAME            PIC X(20).
      *       …¬ºº»÷πÊ
              05  FMSEIKYUN-BRMNUM             PIC X(6).
      *
      *       ø«Œ≈≤ 
              05  FMSEIKYUN-SRYKANAME          PIC X(30).
      *        ›∏±æ Û
              05  FMSEIKYUN-HOKENNAME          PIC X(40).
              05  FMSEIKYUN-FUTAN-G.
      *         …È√¥≥‰πÁ
                07  FMSEIKYUN-FUTAN            PIC X(04).
      *         …È√¥≥‰πÁ
                07  FMSEIKYUN-FUTANMEI         PIC X(02).
      *       ≈¡…º»÷πÊ° ∞Ï≥Á∞ ≥∞°À
              05  FMSEIKYUN-DENPNUM            PIC X(07).
      *       ƒ˚¿µ…Ωº®∂Ë ¨
              05  FMSEIKYUN-TEISEI-KBN         PIC X(01).
      *       ¿¡µ·π‡Ã‹
              05  FMSEIKYUN-MEISAI-OCC         OCCURS 16.
      *           ¿¡µ·π‡Ã‹∂Ë ¨
                  07  FMSEIKYUN-HKNTEN-KBN     PIC X(03).
      *           ¿¡µ·π‡Ã‹Ãæ
                  07  FMSEIKYUN-HKNTEN-NAME    PIC X(20).
      *           ¿¡µ·≈¿øÙ
                  07  FMSEIKYUN-HKNTEN         PIC X(10).
      *           ¿¡µ·∂‚≥€°  ›∏±≈¨Õ—≥∞°À
                  07  FMSEIKYUN-TGMONEY        PIC X(10).
      *       ≈¿øÙπÁ∑◊
              05  FMSEIKYUN-TOTAL-TEN          PIC X(10).
      *       ¥µº‘∞Ï…Ù…È√¥∂‚
              05  FMSEIKYUN-HKNTEKMONEY                 PIC X(10).
      *       ¥µº‘∞Ï…Ù…È√¥∂‚•È•Ÿ•Î° œ´∫“ø«Œ≈Œ¡≈˘∫∆∑«°À
              05  FMSEIKYUN-LBL-RSI-TOTAL               PIC X(20).
      *       ¥µº‘∞Ï…Ù…È√¥∂‚° œ´∫“ø«Œ≈Œ¡≈˘∫∆∑«°À
              05  FMSEIKYUN-RSI-TOTAL                   PIC X(10).
      *       ∏¯»Ò∞Ï…Ù…È√¥∂‚
              05  FMSEIKYUN-KOH-FTN                     PIC X(10).
      *       ø©ªˆŒ≈Õ‹»Ò
              05  FMSEIKYUN-SKJRYOYO                    PIC X(10).
      *       ø©ªˆ…È√¥∂‚
              05  FMSEIKYUN-SKJ-FTN                     PIC X(10).
      *       ¿∏≥ËŒ≈Õ‹»Ò
              05  FMSEIKYUN-LIFERYOYO                    PIC X(10).
      *       ¿∏≥Ë…È√¥∂‚
              05  FMSEIKYUN-LIFE-FTN                     PIC X(10).
      *        ›∏±≈¨Õ—≥∞πÁ∑◊
              05  FMSEIKYUN-TOTAL-TGMONEY               PIC X(10).
      *        ›∏±≈¨Õ—≥∞πÁ∑◊æ√»Ò¿«∫∆∑«
              05  FMSEIKYUN-TOTAL-TGMONEY-TAX           PIC X(10).
      *       §Ω§Œ¬æº´»Ò
              05  FMSEIKYUN-JIHI-OCC                    OCCURS  10.
      *             §Ω§Œ¬æº´»ÒÃææŒ
                07  FMSEIKYUN-JIHI-NAME                 PIC X(20).
      *             §Ω§Œ¬æº´»Ò∂‚≥€
                07  FMSEIKYUN-JIHI                      PIC X(10).
      *       §Ω§Œ¬æº´»ÒπÁ∑◊∂‚≥€
              05  FMSEIKYUN-JIHI-TOTAL                  PIC X(10).
      *       §Ω§Œ¬æº´»ÒπÁ∑◊∂‚≥€æ√»Ò¿«∫∆∑«
              05  FMSEIKYUN-JIHI-TOTAL-TAX              PIC X(10).
      *       ººŒ¡∫π≥€
              05  FMSEIKYUN-RMSAGAKU                    PIC X(10).
      *       ººŒ¡∫π≥€æ√»Ò¿«∫∆∑«
              05  FMSEIKYUN-RMSAGAKU-TAX                PIC X(10).
      *       ƒ¥¿∞∂‚£±
              05  FMSEIKYUN-CHOSEI1                     PIC X(10).
      *       ƒ¥¿∞∂‚£≤
              05  FMSEIKYUN-CHOSEI2                     PIC X(10).
      *       ∫£≤Û¿¡µ·≥€
              05  FMSEIKYUN-SKYMONEY                    PIC X(10).
      *       ∫£≤Û¿¡µ·≥€æ√»Ò¿«∫∆∑«
              05  FMSEIKYUN-SKYMONEY-TAX                PIC X(10).
      *       ∫£≤Û∆˛∂‚≥€
              05  FMSEIKYUN-NYUKIN                      PIC X(10).
      *       ∆˛±°Ã§º˝≥€
              05  FMSEIKYUN-NYU-MISYU                   PIC X(10).
      *       ≥∞ÕËÃ§º˝≥€
              05  FMSEIKYUN-GAI-MISYU                   PIC X(10).
      *
      *       √Ì∞’ΩÒ§≠° ¿¡µ·ΩÒ…Ù ¨°À
              05  FMSEIKYUN-SKY-CAUTION                 PIC X(100).
      *
      *       •·•‚
              05  FMSEIKYUN-MEMO                        PIC X(80)
                                                        OCCURS 10.
