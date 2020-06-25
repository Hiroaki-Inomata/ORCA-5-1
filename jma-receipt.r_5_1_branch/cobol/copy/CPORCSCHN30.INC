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
      *    入院薬剤情報パラメタ作成パラ      COPY (CPORCSCHN30.INC)
      *                                          (ORCSCHN30 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/07/23  コメント追加
      *  05.00.00    NACL-多々納  17/05/16  病棟番号追加
      ************************************************************
      *
       01  ORCSCHN30AREA.
      *
      *    処理区分(N:入院印刷画面）
           03  ORCSHCN30-KBN             PIC X(01).
      *    交付日
           03  ORCSHCN30-SRYYMD          PIC X(08).
      *    ドクター
           03  ORCSHCN30-DRCD            PIC X(05).
      *    病棟
           03  ORCSHCN30-BTUNAME         PIC X(20).
      *    病室
           03  ORCSHCN30-ROOMNO          PIC X(06).
      *H29.5
      *    病棟番号
           03  ORCSHCN30-BTUNUM          PIC X(02).
      *
      *    処方内容
           03  ORCSHCN30-DATA-REC.
               05  ORCSHCN30-ZAI-MAX         PIC 9(03).
               05  ORCSHCN30-REC-TBL     OCCURS   100.
                   07  ORCSHCN30-ZAIKAIKEI       PIC 9(03).
                   07  ORCSHCN30-SRYKBN          PIC X(02).
                   07  ORCSHCN30-SRYSYUKBN       PIC X(03).
                   07  ORCSHCN30-NAI-TBL         OCCURS   50.
                       09  ORCSHCN30-SRYCD           PIC  X(09).
                       09  ORCSHCN30-SRYSURYO        PIC  9(05)V9(05).
                       09  ORCSHCN30-KANSURYO        PIC  9(05)V9(05).
                       09  ORCSHCN30-SRYKAISU        PIC  9(03).
                       09  ORCSHCN30-ZAIKAISU        PIC  9(03).
      *R02.4           09  ORCSHCN30-INPUTCOMENT     PIC  X(80).
                       09  ORCSHCN30-INPUTCOMENT     PIC  X(140).
      *H28.11          用法コード数量
                       09  ORCSHCN30-INPUTCHI-G.
      *R02.4               11  ORCSHCN30-INPUTCHI    PIC  X(08)
                           11  ORCSHCN30-INPUTCHI    PIC  X(10)
                                                     OCCURS   5.
      *
