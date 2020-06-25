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
      *    診療報酬請求書     COPY (CPSKT3400TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         11/04/19  東北地方太平洋沖地震対応
      *  01.00.02    萩野         19/01/11  修正履歴コメント削除
      ************************************************************
       01  SKT3500TMP-REC.
           03      SKT3500TMP-KEY.
             05    SKT3500TMP-KBN                   PIC  9(01).
             05    SKT3500TMP-SRYKA                 PIC  X(02).
             05    SKT3500TMP-GENMEN                PIC  X(01).
           03      SKT3500TMP-DATA-TBL OCCURS 5.
             05    SKT3500TMP-KENSU                 PIC  9(05).
             05    SKT3500TMP-TENSU                 PIC  9(07).
