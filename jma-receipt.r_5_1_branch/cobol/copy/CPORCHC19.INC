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
      *    処方箋出力パラメタ（Ｂ５）COPY (CPORCHC19.INC)
      *                                          (ORCHC19 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  04/07/20  伝票番号追加
      *  01.00.02    NACL-多々納  05/11/10  処理区分 A〜D 、処方内容追加
      *  05.00.00    NACL-多々納  20/02/XX  コメント、入力値桁数変更
      ************************************************************
      *
       01  ORCHC19AREA.
      *
      *    1:前回処方、空白：今回処方、2:頭書のみ、3:院内分
      *    4:中途終了時（院外）、5:中途終了時（院内）
      *    A:処方内容変更、B:前回処方（院外）
      *    C:前回処方（院内）、D:前回処方（選択）
           03  ORCHC19-KBN             PIC X(01).
      *
      *    受診歴の連番（確定しない時、ゼロ）
           03  ORCHC19-RENNUM          PIC 9(01).
      *
      *    伝票番号
           03  ORCHC19-DENPNUM         PIC 9(07).
      *
      *    処方内容（A:処方内容変更、D:前回処方（選択）で使用）
           03  ORCHC19-DATA-REC.
               05  ORCHC19-ZAI-MAX         PIC 9(03).
               05  ORCHC19-REC-TBL         OCCURS   100.
                   07  ORCHC19-ZAIKAIKEI       PIC 9(03).
                   07  ORCHC19-ZAITENKEI       PIC 9(08).
                   07  ORCHC19-SRYKBN          PIC X(02).
                   07  ORCHC19-SRYSYUKBN       PIC X(03).
                   07  ORCHC19-NAI-TBL         OCCURS   50.
                       09  ORCHC19-SRYCD           PIC  X(09).
                       09  ORCHC19-SRYSURYO        PIC  9(05)V9(05).
                       09  ORCHC19-SRYKAISU        PIC  9(03).
      *R02.4           09  ORCHC19-INPUTCOMENT     PIC  X(80).
                       09  ORCHC19-INPUTCOMENT     PIC  X(140).
      *H30.4           入力値
                       09  ORCHC19-INPUTCHI-G.
      *R02.4               11  ORCHC19-INPUTCHI    PIC  X(08)
                           11  ORCHC19-INPUTCHI    PIC  X(10)
                                                        OCCURS  5.
      *
