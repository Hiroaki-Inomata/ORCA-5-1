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
      ************************************************************
      *    ドクター編集パラメタ　　COPY (CPORCSDRSET.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.00    NACL-多々納  09/03/05  当日担当医対応なし追加
      ************************************************************
       01  ORCSDRSETAREA.
      *
      *入力
           03  ORCSDR-IN-AREA.
      *処理区分（１：編集、２：チェック）
               05  ORCSDR-KBN                  PIC  X(01).
      *診療年月日
               05  ORCSDR-SYSYMD               PIC  X(08).
      *診療科（科別のドクター編集の時）
               05  ORCSDR-SRYKA                PIC  X(02).
      *処理区分２（１：当日担当医なし）
               05  ORCSDR-KBN2                 PIC  X(01).
      *
      *ドクター（Ｉ−Ｏ）
           03  ORCSDR-IN-DRCD-G.
      *        ドクターコード（ＩＮ）
               05  ORCSDR-IN-DRCD              PIC  X(04).
               05  ORCSDR-IN-DRCD-H            PIC  X(01).
      *        ドクター名（ＯＵＴ）
               05  ORCSDR-OUT-DRCDMEI          PIC  X(24).
      *    テーブル位置
           03  ORCSDR-DRCD-IDX                 PIC  9(04).
      *出力
           03  ORCSDR-OUT-AREA.
      *        テーブル
               05  ORCSDR-DRCD-TBL-G.
                   07  ORCSDR-DRCD-TBL         OCCURS   99.
      *                ドクターコード
                       09  ORCSDR-DRCD             PIC  X(04).
      *
                       09  ORCSDR-DRCD-H           PIC  X(01).
      *                ドクター名
                       09  ORCSDR-DRCDMEI          PIC  X(24).
      *        テーブルＭＡＸ
               05  ORCSDR-DRCD-MAX             PIC  9(04).
      *エラーコード（１：ドクターコードなし）
           03  ORCSDR-ERRCD                    PIC 9(01).
      *
