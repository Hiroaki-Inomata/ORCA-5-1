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
      *    入院診療費明細書出力パラメタ      COPY (CPORCHCN04.INC)
      *                                          (ORCHCN04 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHCN04AREA.
      *
      *    患者ＩＤ
           03  ORCHCN04-PTID                   PIC 9(10).
      *    患者番号
           03  ORCHCN04-PTNUM                  PIC X(20).
      *    患者氏名
           03  ORCHCN04-NAME                   PIC X(50).
      *    発行区分 (1:再発行 3:収納請求確認画面)
           03  ORCHCN04-HAKKOFLG               PIC 9(01).
      *    請求期間
           03  ORCHCN04-SKYSTYMD               PIC X(08).
           03  ORCHCN04-SKYEDYMD               PIC X(08).
      *
           03  ORCHCN04-INPUT-OCC        OCCURS   10.
      *    伝票番号（必須）
               05  ORCHCN04-DENPNUM        PIC 9(07).
      *    伝票番号枝番（収納からの再発行）
               05  ORCHCN04-DENPEDANUM     PIC  9(02).
      *
      *    伝票発行日(定期請求印刷指示画面から）
           03  ORCHCN04-DENPPRTYMD         PIC  X(08).
      *    発行ページ数
           03  ORCHCN04-PAGE               PIC  9(05).
      *    総ページ数（一時ファイル名編集用）
           03  ORCHCN04-PAGE-CNT           PIC  9(06).
      *
      *    印刷ＤＢ出力区分(1:印刷ＤＢに出力）
           03  ORCHCN04-PRTKBN             PIC X(01).
      *    一括パラメタ
           03  ORCHCN04-PRTAREA            PIC X(25000).
