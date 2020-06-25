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
      *----------------------------------------------------------*
      *    お薬手帳出力パラメタ          COPY (CPORCSCH62.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSCH62AREA.
      *
      *    1:伝票番号（院内）、2:伝票番号（院外）
      *    3:中途終了時（院外）、4:中途終了時（院外）
      *    A:処方内容変更、B:前回処方（院外）
      *    C:前回処方（院内）、D:前回処方（選択）
           03  ORCSCH62-KBN             PIC X(01).
      *    1:ＣＳＶ出力のみ
           03  ORCSCH62-KBN2            PIC X(01).
      *    発行画面区分（1:請求確認、2:中途終了、3：前回処方
           03  ORCSCH62-PGKBN           PIC X(01).
      *
      *    伝票番号
           03  ORCSCH62-DENPNUM         PIC 9(07).
      *
      *    処方内容（A:処方内容変更、D:前回処方（選択）で使用）
           03  ORCSCH62-DATA-REC.
               05  ORCSCH62-ZAI-MAX         PIC 9(03).
               05  ORCSCH62-REC-TBL         OCCURS   100.
                   07  ORCSCH62-ZAIKAIKEI       PIC 9(03).
                   07  ORCSCH62-SRYKBN          PIC X(02).
                   07  ORCSCH62-SRYSYUKBN       PIC X(03).
                   07  ORCSCH62-NAI-TBL         OCCURS   50.
                       09  ORCSCH62-SRYCD           PIC  X(09).
                       09  ORCSCH62-SRYSURYO        PIC  9(05)V9(05).
                       09  ORCSCH62-SRYKAISU        PIC  9(03).
                       09  ORCSCH62-INPUTCOMENT     PIC  X(140).
                       09  ORCSCH62-KANSURYO        PIC  9(05)V9(05).
      *
