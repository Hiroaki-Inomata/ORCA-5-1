      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
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
      *****************************************************************
      *
      *    病名登録 画面ＳＰＡ 
      *            画面　Ｃ０２で使用 
      *          
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/06/06  保険病名フラグの追加
      *  01.02.01    MCC-藤原     01/08/27  画面修正による項目の修正            
      *  01.03.01    MCC-藤原     01/12/06  自院病名マスタの追加     
      *
      *  01.01.01    NACL-藤原    03/08/28  登録数の制限をなくす
      *  01.01.01    NACL-藤原    04/02/24  入外区分の追加
      *
      *  02.06.01    NACL-藤原    05/08/26  保険組合せの追加
      *****************************************************************
      *
      *01  SPA-SCR-REC.
        02  SPA-SCR-C02-REC.
           03  SPA-GMN-C02-TBL.
               05  SPA-GMN-C02-TBLO            OCCURS   200.
                   07  SPA-GMN-C02-TBANGO      PIC 9(05).
                   07  SPA-GMN-C02-TSRYKA      PIC X(04).
                   07  SPA-GMN-C02-TSRYYMD     PIC X(09).
                   07  SPA-GMN-C02-TBYOMEI     PIC X(80).
                   07  SPA-GMN-C02-TUTAGAIFLG  PIC X(14).
                   07  SPA-GMN-C02-TSYUBYOFLG  PIC X(12).
                   07  SPA-GMN-C02-TMANSEIKBN  PIC X(42).
                   07  SPA-GMN-C02-TNYUGAIKBN  PIC X(06).
                   07  SPA-GMN-C02-THKNCOMBI   PIC X(40).
                   07  SPA-GMN-C02-TREZEFLG    PIC X(10).
                   07  SPA-GMN-C02-TREZEMM     PIC 9(02).
                   07  SPA-GMN-C02-TTENKIKBN   PIC X(04).
                   07  SPA-GMN-C02-TTENKIYMD   PIC X(09).
                   07  SPA-GMN-C02-THKNBYOMEIFLG
                                               PIC X(08).
      * 
           03  SPA-NAI-C02-TBL.
               05  SPA-NAI-C02-TBLO            OCCURS   200.
                   07  SPA-NAI-C02-TSRYYMD     PIC X(08).
                   07  SPA-NAI-C02-TRENNUM     PIC 9(02).
                   07  SPA-NAI-C02-TSRYKA      PIC X(02).
                   07  SPA-NAI-C02-TNYUGAIKBN  PIC X(01).
                   07  SPA-NAI-C02-TTENKIYMD   PIC X(08).
                   07  SPA-NAI-C02-THKNCOMBI   PIC X(04).
