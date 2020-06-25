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
      *    薬剤情報パラメタ作成パラ        COPY (CPORCSCH30.INC)
      *                                          (ORCSCH30 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/09  処理区分 5 内容変更追加
      *  04.05.00    NACL-多々納  09/07/23  コメント追加
      *  04.05.00    NACL-多々納  09/10/XX  換算値数量追加
      *  04.08.00    NACL-多々納  16/10/XX  用法コード数量追加
      ************************************************************
      *
       01  ORCSCH30AREA.
      *
      *    処理区分(１：院内、２：院外、3:中途終了院内、4:中途終了院外）
      *              5:内容変更
           03  ORCSCH30-KBN            PIC X(01).
      *
      *    伝票番号
           03  ORCSCH30-DENPNUM        PIC 9(07).
      *
      *    処方内容（C:処方内容変更のみ使用）
           03  ORCSCH30-DATA-REC.
               05  ORCSCH30-ZAI-MAX         PIC 9(03).
               05  ORCSCH30-REC-TBL     OCCURS   100.
                   07  ORCSCH30-ZAIKAIKEI       PIC 9(03).
                   07  ORCSCH30-SRYKBN          PIC X(02).
                   07  ORCSCH30-SRYSYUKBN       PIC X(03).
                   07  ORCSCH30-NAI-TBL         OCCURS   50.
                       09  ORCSCH30-SRYCD           PIC  X(09).
                       09  ORCSCH30-SRYSURYO        PIC  9(05)V9(05).
                       09  ORCSCH30-KANSURYO        PIC  9(05)V9(05).
                       09  ORCSCH30-SRYKAISU        PIC  9(03).
                       09  ORCSCH30-ZAIKAISU        PIC  9(03).
                       09  ORCSCH30-INPUTCOMENT     PIC  X(140).
                       09  ORCSCH30-INPUTCHI-AREA.
                           11  ORCSCH30-INPUTCHI    PIC  X(10)
                                                    OCCURS  5.
      *
