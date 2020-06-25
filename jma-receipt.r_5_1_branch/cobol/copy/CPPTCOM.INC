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
      *    患者コメント       COPY (CPPTCOM.INC)
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.06.00     NACL-多々納  11/02/14  用法コメント対応
      * 05.00.00     NACL-多々納  20/02/XX  コメント、入力値桁数変更
      ************************************************************
      *01  PTCOM-REC.
      *キー
           03  PTCOM-KEY.
      *医療機関ＩＤ
               05  PTCOM-HOSPNUM         PIC 9(2).
      *患者ＩＤ
               05  PTCOM-PTID           PIC  9(10).
      *剤番号
               05  PTCOM-ZAINUM         PIC  9(08).
      *診療コード
               05  PTCOM-SRYCD          PIC  X(09).
      *コメント枝番番号
               05  PTCOM-RENNUM         PIC  9(03).
      *入力コメント
           03  PTCOM-INPUTCOMENT        PIC  X(140).
      *入力値領域
           03  PTCOM-INPUTCHI-AREA.
      *入力値１
               05  PTCOM-INPUTCHI1      PIC  X(10).
      *入力値２
               05  PTCOM-INPUTCHI2      PIC  X(10).
      *入力値３
               05  PTCOM-INPUTCHI3      PIC  X(10).
      *入力値４
               05  PTCOM-INPUTCHI4      PIC  X(10).
      *入力値５
               05  PTCOM-INPUTCHI5      PIC  X(10).
           03  PTCOM-INPUTCHI-AREA-R    REDEFINES   PTCOM-INPUTCHI-AREA
                                        OCCURS  5.
      *入力値
               05  PTCOM-INPUTCHI       PIC  X(10).
      *端末ＩＤ
           03  PTCOM-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  PTCOM-OPID               PIC  X(16).
      *作成年月日
           03  PTCOM-CREYMD             PIC  X(08).
      *更新年月日
           03  PTCOM-UPYMD              PIC  X(08).
      *更新時間
           03  PTCOM-UPHMS              PIC  X(06).
      *
