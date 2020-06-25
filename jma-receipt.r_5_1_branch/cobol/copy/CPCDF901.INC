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
      *    収集データ（病院情報）         COPY (CPCDF901.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  COLDAT91-REC.
           03  COLDAT91-KEY.
      *        レコード区分
               05  COLDAT91-RECKBN         PIC  X(01).
               05  COLDAT91-RECSEQ         PIC  9(03).
           03  COLDAT91-BODY-1.
      *        医療機関ＩＤ
               05  COLDAT91-HOSPID         PIC  X(24).
      *        都道府県
               05  COLDAT91-PREFNUM        PIC  9(02).
      *        医療機関種別
               05  COLDAT91-HOSPSBT        PIC  9(01).
      *        旧総合病院
               05  COLDAT91-HOSPSBT1       PIC  9(01).
      *        病床数（許可）
               05  COLDAT91-BEDSU          PIC  9(04).
      *        病床数（一般）
               05  COLDAT91-BEDSUIPN       PIC  9(04).
      *        入外区分
               05  COLDAT91-NYUGAIKBN      PIC  X(01).
      *        データ年月
               05  COLDAT91-DATAYM         PIC  X(06).
      *        プログラム識別番号
               05  COLDAT91-PRGSKBNO       PIC  X(08).
      *
               05  COLDAT91-YOBI1          PIC  X(68).
           03  COLDAT91-BODY-2             REDEFINES
                                           COLDAT91-BODY-1.
      *        診療科コード
               05  COLDAT91-SRYKA          PIC  X(02).
      *        レセ電診療科コード
               05  COLDAT91-RECEKA         PIC  X(02).
      *        診療科名
               05  COLDAT91-SRYKANAME      PIC  X(40).
      *
               05  COLDAT91-YOBI2          PIC  X(75).
           03  COLDAT91-BODY-3             REDEFINES
                                           COLDAT91-BODY-1.
      *        施設基準コード
               05  COLDAT91-SSTKIJUNCD     PIC  9(03).
      *
               05  COLDAT91-YOBI3          PIC  X(116).
           03  COLDAT91-BODY-4             REDEFINES
                                           COLDAT91-BODY-1.
      *        保険番号
               05 COLDAT91-HKNNUM          PIC  X(03).
      *        支払区分
               05 COLDAT91-PAYKBN          PIC  X(02).
      *        法別番号
               05 COLDAT91-HBTNUM          PIC  X(02).
      *        制度名
               05 COLDAT91-SEIDONAME       PIC  X(100).
      *        保険公費種別区分
               05 COLDAT91-HKNKOHSBTKBN    PIC  X(01).
      *        公費主保区分
               05 COLDAT91-HKNKOHKBN       PIC  X(01).
      *        条件−年齢開始
               05 COLDAT91-JOKEN-STYEAR    PIC  9(03).
      *        条件−年齢終了
               05 COLDAT91-JOKEN-EDYEAR    PIC  9(03).
      *        点数単価
               05 COLDAT91-TENTNK          PIC  9(03).
      *        レセプト請求区分
               05 COLDAT91-RECESKYKBN      PIC  X(01).
