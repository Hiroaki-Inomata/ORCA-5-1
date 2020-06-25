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
      *         システム管理マスタ     (CPSK9100.INC)            *
      *         （９１００）                                     *
      *         ＩＣリーダライタ情報                             *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-9100-REC.
           03  SYS-9100-KEY.
               05  SYS-9100-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-9100-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-9100-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-9100-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-9100-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-9100-KANRITBL.
      *接続する・しない(0:しない/1:する)
               05  SYS-9100-CONNECT     PIC  X(1).
      *データ保持期間
               05  SYS-9100-KEEPINGTERM PIC  9(3).
      *予備
               05  SYS-9100-YOBI        PIC  X(496).
      *端末ＩＤ
           03  SYS-9100-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-9100-OPID        PIC  X(16).
      *作成年月日
           03  SYS-9100-CREYMD      PIC  X(08).
      *更新年月日
           03  SYS-9100-UPYMD       PIC  X(08).
      *更新時間
           03  SYS-9100-UPHMS       PIC  X(06).
