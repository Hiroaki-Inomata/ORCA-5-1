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
      *    レセプト作成管理マスタ更新用パラメタ
      *                                    COPY (CPORCSRECEUPD.INC)
      *                                   使用プログラム (ORCSRECEUPD)
      *    設定項目
      *      処理区分  １：患者ＩＤのみ  ２：
      *      患者ＩＤ
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSRECEUPDAREA.
      *    処理区分    １：患者ＩＤのみで処理
      *                ２：
           03  LNK-UPD-SYORI           PIC X(01).
      *    復帰コード  １：処理区分エラー
           03  LNK-UPD-RC              PIC X(01).
      *    医療機関ＩＤ
           03  LNK-UPD-HOSPNUM         PIC 9(2).
      *    患者ＩＤ
           03  LNK-UPD-PTID            PIC 9(10).
      *    入外区分
           03  LNK-UPD-NYUGAIKBN       PIC X(01).
      *    診療科
           03  LNK-UPD-SRYKA           PIC X(02).
      *    診療年月
           03  LNK-UPD-SRYYM           PIC X(06).
      *    保険区分
           03  LNK-UPD-HKNKBN          PIC X(01).
      *    保険区分２
           03  LNK-UPD-HKNKBN2         PIC X(01).
      *    端末ＩＤ
           03  LNK-UPD-TERMID          PIC  X(16).
      *    オペーレータＩＤ
           03  LNK-UPD-OPID            PIC  X(16).
