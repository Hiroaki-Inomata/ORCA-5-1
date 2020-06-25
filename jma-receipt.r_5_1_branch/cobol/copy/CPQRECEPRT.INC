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
      ************************************************************
      *    レセプト明細書     COPY (CPQRECEPRT.INC)              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    09/06/18  社保レセプト遍綴順対応
      *                                     審査支払機関、在総診フラグの追加
      *
      *  04.07.01    NACL-藤原    12/02/13  健保組合医への直接請求と審査
      *                                     支払機関請求対応
      *************************************************************
      *01  QUERY-RECE08-REC.
      *医療機関番号
           03  QUERY-RECE08-HOSPNUM        PIC 9(2).
      *帳票ＩＤ
           03  QUERY-RECE08-PRTID          PIC X(04).
      *診療年月
           03  QUERY-RECE08-RECEYM         PIC X(06).
      *作成日
           03  QUERY-RECE08-CREYMD         PIC X(08).
      *作成時間
           03  QUERY-RECE08-CREHMS         PIC X(06).
      *入外区分
           03  QUERY-RECE08-NYUGAIKBN      PIC X(01).
      *審査支払機関
           03  QUERY-RECE08-TEISYUTUSAKI   PIC X(01).
      *在総診・在医総
           03  QUERY-RECE08-ZAITAKU        PIC X(01).
      *医師コード
           03  QUERY-RECE08-DRCD           PIC X(05).
      *医師別診療科
           03  QUERY-RECE08-DRCD-SRYKA     PIC X(02).
      *病棟
           03  QUERY-RECE08-SORT-1         PIC X(10).
      *診療科
           03  QUERY-RECE08-SRYKA          PIC X(02).
      *検索用保険者番号        
           03  QUERY-RECE08-HKNJANUM-S     PIC X(08).
      *件数
           03  QUERY-RECE08-KENSU          PIC 9(06).
      *ページ
           03  QUERY-RECE08-PAGE           PIC 9(06).  
