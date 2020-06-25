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
      *******************************************************************
      ************************************************************
      *    労災レセプト回数取得用パラメタ　 COPY (CPORCSROUKAISU.INC)
      *                          使用プログラム  (ORCSROUKAISU)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  LNK-ORCSROUKAISU-REC.
      *
      *医療機関ＩＤ
           03  LNK-ORCSROUKAISU-HOSPNUM         PIC 9(2).
      *入外区分
           03  LNK-ORCSROUKAISU-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
           03  LNK-ORCSROUKAISU-PTID            PIC  9(10).
      *保険組合せ
           03  LNK-ORCSROUKAISU-HKNCOMBI        PIC  9(04).
      *診療月
           03  LNK-ORCSROUKAISU-SRYYM           PIC  9(06).
      *療養開始年月日
           03  LNK-ORCSROUKAISU-RYOSTYMD        PIC  X(08).
      *労災レセ回数記載：基準年月
           03  LNK-ORCSROUKAISU-KIJYUNYM-KSI    PIC  X(06).
      *労災レセ回数記載：回数
           03  LNK-ORCSROUKAISU-KAISU-KSI       PIC  9(03).
      *該当月療養開始日
           03  LNK-ORCSROUKAISU-KAISHIBI        PIC  9(02).
      *診療科（外来時のみセット）
           03  LNK-ORCSROUKAISU-SRYKA           PIC  X(02).
           03  LNK-ORCSROUKAISU-RECESRYKA       PIC  X(02).
      *
      *返却値
      *
           03  LNK-ORCSROUKAISU-OUT-AREA.
      *回数
             05  LNK-ORCSROUKAISU-RECEKAISU     PIC  9(03).
      *
      *復帰コード
      *
           03  LNK-ORCSROUKAISU-RC              PIC  9(02).
      *
