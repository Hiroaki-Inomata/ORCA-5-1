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
      *         チェックマスタ４     (CPCK2001.INC)              *
      *         （２００１）                                     *
      *         個別選択患者                                     *
      *         KBNCD : 1 画面指示                               *
      *         KBNCD : 2 個別指示画面選択済患者                 *
      *         KBNCD : 3 仮選択                                 *
      *         KBNCD : 4 前回エラー患者                         *
      *         KBNCD : A APIからの指示                          *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CK4-2001-REC.
           03  CK4-2001-KEY.
               05  CK4-2001-HOSPNUM     PIC 9(2).
      *管理コード
               05  CK4-2001-KANRICD     PIC  X(04).
      *区分コード
               05  CK4-2001-KBNCD       PIC  X(08).
      *有効開始年月日
               05  CK4-2001-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  CK4-2001-EDYUKYMD    PIC  X(08).
      *患者ＩＤ
           03  CK4-2001-PTID            PIC  9(10).
      *管理テーブル
           03  CK4-2001-TBL.
               05  CK4-2001-FILLER      PIC  X(500).
      *端末ＩＤ
           03  CK4-2001-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  CK4-2001-OPID            PIC  X(16).
      *作成年月日
           03  CK4-2001-CREYMD          PIC  X(08).
      *更新年月日
           03  CK4-2001-UPYMD           PIC  X(08).
      *更新時間
           03  CK4-2001-UPHMS           PIC  X(06).
