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
      *         チェックマスタ４     (CPCK1200.INC)              *
      *         （１２００）                                     *
      *         前回エラー情報                                   *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CK4-1200-REC.
           03  CK4-1200-KEY.
               05  CK4-1200-HOSPNUM     PIC 9(2).
      *管理コード
               05  CK4-1200-KANRICD     PIC  X(04).
      *区分コード(1-6桁目：連番／7-8桁目：確認項目の番号）
               05  CK4-1200-KBNCD       PIC  X(08).
      *有効開始年月日
               05  CK4-1200-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  CK4-1200-EDYUKYMD    PIC  X(08).
      *患者ＩＤ
           03  CK4-1200-PTID            PIC  9(10).
      *管理テーブル
           03  CK4-1200-TBL.
               05  CK4-1200-HKNKBN      PIC  X(01).
               05  CK4-1200-ERRCD       PIC  X(06).
               05  CK4-1200-FIL1        PIC  X(193).
               05  CK4-1200-ERRMSG      PIC  X(220).
               05  CK4-1200-FIL2        PIC  X(80).
      *端末ＩＤ
           03  CK4-1200-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  CK4-1200-OPID            PIC  X(16).
      *作成年月日
           03  CK4-1200-CREYMD          PIC  X(08).
      *更新年月日
           03  CK4-1200-UPYMD           PIC  X(08).
      *更新時間
           03  CK4-1200-UPHMS           PIC  X(06).
