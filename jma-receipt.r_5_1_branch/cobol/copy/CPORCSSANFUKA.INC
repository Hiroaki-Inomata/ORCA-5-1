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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *---------------------------------------------------*
      *    内分泌負荷試験更新パラメタ                          *
      *                         COPY  (CPORCSSANFUKA.INC) *
      *
      *---------------------------------------------------*
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  ORCSSANFUKAAREA.
      *ＩＮ項目
      *処理区分（１：当月分集計、２：点数計算、３：更新）
           03  LNK-SANFUKA-SYORI             PIC  9(01).
      *処理ＰＧ
           03  LNK-SANFUKA-PG                PIC  X(03).
      *診療年月日
           03  LNK-SANFUKA-SRYYMD            PIC  X(08).
      *    訂正時の診療コード
           03  LNK-SANFUKA-TEISEI-AREA.
               05  LNK-SANFUKA-TEISEI-OCC      OCCURS   400.
                   07  LNK-SANFUKA-TEI-SRYCD   PIC X(09).
                   07  LNK-SANFUKA-TEI-KAISU   PIC 9(03).
           03  LNK-SANFUKA-I-O.
      *内分泌負荷試験上限点数
               05  LNK-SANFUKA-TEN-MAX         PIC 9(08).
      *当月分合計
               05  LNK-SANFUKA-MSANTESU        PIC 9(08).
      *当月最終算定日
               05  LNK-SANFUKA-MSANTEID        PIC 9(02).
      *訂正分合計
               05  LNK-SANFUKA-MSANTESU-DEL    PIC 9(08).
      *
           03  LNK-SANFUKA-OUT.
      *累積点数エラー
               05  LNK-SANFUKA-ERRMSG       PIC X(80).
      *
