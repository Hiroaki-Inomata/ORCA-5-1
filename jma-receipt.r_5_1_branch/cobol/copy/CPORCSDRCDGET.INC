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
      *    医師コード取得用パラメタ      COPY (CPORCSDRCDGET.INC)
      *                                  使用プログラム (ORCSDRCDGET)
      *    設定項目
      *      診療年月
      *      入外区分
      *      患者ＩＤ
      *      保険組合せ
      *     （入院のみ）
      *      対象年月日情報
      *      入院年月日
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
           03  LNK-DRCDGET-IN.
      *        診療年月
               05  LNK-DRCDGET-I-SRYYM     PIC 9(06).
      *        入外区分
               05  LNK-DRCDGET-I-NYUGAIKBN PIC X(01).
      *        患者ＩＤ
               05  LNK-DRCDGET-I-PTID      PIC 9(10).
      *        保険組合せ情報
               05  LNK-DRCDGET-I-HKNCOMBI-OCC.
      *        保険組合せ
                   07  LNK-DRCDGET-I-HKNCOMBI
                                           PIC 9(04)   OCCURS  10.
      *        対象年月日情報
               05  LNK-DRCDGET-I-TEKYMD.
      *        適用開始年月日
                   07  LNK-DRCDGET-I-TEKSTYMD
                                           PIC X(08). 
      *        適用終了年月日
                   07  LNK-DRCDGET-I-TEKEDYMD
                                           PIC X(08). 
      *        入院年月日
               05  LNK-DRCDGET-I-NYUINYMD  PIC X(08). 
      *
           03  LNK-DRCDGET-OT.
      *        医師コード
               05  LNK-DRCDGET-O-DRCD      PIC X(05).
      *        診療科
               05  LNK-DRCDGET-O-DRCD-SRYKA
                                           PIC X(02).
      *    復帰コード
           03  LNK-DRCDGET-RC           PIC S9(9) BINARY.
