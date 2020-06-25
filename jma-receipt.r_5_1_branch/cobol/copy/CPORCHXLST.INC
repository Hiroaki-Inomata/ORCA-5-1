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
      *    チェックマスタ一覧印刷パラメタ　   COPY (CPORCHXLST.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHXLSTAREA.
      *
      *    チェック区分
           03  HXLST-CHKKBN             PIC  X(01).
      *    対象コード（開始）
           03  HXLST-SRYCD              PIC X(09).
      *    対象コード（終了）
           03  HXLST-SRYCD-END          PIC X(09).
      *
      *    対象ＩＤ
           03  HXLST-ID                 PIC 9(09).
      *    処理区分
           03  HXLST-PROCKBN            PIC X(01).
      *    ソート区分
           03  HXLST-SRTKBN             PIC X(01).
      *    基準日
           03  HXLST-KJNYMD             PIC X(08).
      *    適応病名マスタ分判別キー
           03  HXLST-TEMP-NAME          PIC X(10).
           03  HXLST-TEMP-KEY           PIC X(16).
      *    適応病名マスタ対象区分（マスタ）
      *    0:薬剤及び診療行為
      *    1:薬剤
      *    2:診療行為
           03  HXLST-TEKBYOMST          PIC X(01).
      *    適応病名マスタ対象区分（範囲）
      *    1:チェックマスタ登録分のみ印刷する
      *    2:自院コードと過去６ヶ月の診療内容より印刷する
      *    3:１または２に該当するものを印刷する
      *    4:全て印刷する
           03  HXLST-TEKBYOPRT          PIC X(01).
      *    一括パラメタ
           03  HXLST-PRTAREA            PIC X(25000).
      *    出力件数
           03  HXLST-OUT-CNT            PIC 9(8).
      *    復帰コード
           03  HXLST-RCD                PIC S9(9)   BINARY.
      *
