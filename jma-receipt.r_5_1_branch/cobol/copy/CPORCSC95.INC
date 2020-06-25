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
      *    初期ＳＰＡ編集用パラメタ　   COPY (CPORCSC95.INC)
      *
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSC95AREA.
      *ＩＮ
      *    処理区分（１：ＳＰＡの初期編集のみ
      *              ２：ＳＰＡの初期編集、ワーク診療行為の作成
      *              ３：ワーク診療行為の作成）
      *              ４：訂正時のワーク診療行為の作成）
      *              Ａ：ワーク診療行為内容の追加
           03  LNK-SC95-KBN            PIC X(01).
      *    処理ＰＧ名
           03  LNK-SC95-PG             PIC X(03).
      *    診療日
           03  LNK-SC95-SRYYMD         PIC X(08).
           03  LNK-SC95-SRYYMDG        PIC X(14).
      *    対象履歴日付
           03  LNK-SC95-RRKYMD         PIC X(08).
           03  LNK-SC95-RENNUM         PIC 9(01).
           03  LNK-SC95I-DOUJI-RENNUM  PIC 9(01).
      *
           03  LNK-SC95-GRP-DENPNUM   PIC 9(07).
      *    診療科・保険組合せ（ワーク診療行為追加時）
           03  LNK-SC95-SRYKA          PIC X(02).
           03  LNK-SC95-HKNCOMBI       PIC X(04).
      *
      *ＯＵＴ
      *    継続中（１：Ｋ０２で継続中の時、Ｋ０１へ）
           03  LNK-SC95-DOUJI.
               05  LNK-SC95-DOUJI-RENNUM    PIC 9(01).
               05  LNK-SC95-DOUJI-DENPNUM   PIC 9(07).
               05  LNK-SC95-DOUJI-HKNNUM    PIC X(04)
                                                   OCCURS   3.
           03   LNK-SC95-LAST-RENNUM   PIC 9(01).
      *    該当分の合計点数（訂正時使用）
           03   LNK-SC95-GOK-TENSU     PIC 9(11).
      *    点滴の有無（訂正時使用）
           03   LNK-SC95-TENTEKI       PIC 9(01).
           03   LNK-SC95-CHUSYA        PIC 9(08).
      *    ワーク診療行為作成有無
           03   LNK-SC95-WKSRYACT      PIC 9(01).
      *
           03  LNK-SC95-ERRCD          PIC X(04).
      *
