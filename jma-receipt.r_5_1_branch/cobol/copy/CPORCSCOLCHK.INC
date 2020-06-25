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
      *    定点調査研究事業処理チェックパラメタ                  *
      *                         COPY  (CPORCSCOLCHK.INC)         *
      *
      *     COLCHK-RC    : ZERO  (何もなし)
      *                    10    (システム管理未作成指示)
      *                    20    (収集データ作成処理中)
      *                    30    (収集データ存在)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCOLCHKAREA.
      *
      *復帰コード
           03  COLCHK-RC                 PIC S9(9)   BINARY.
      *
