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
      *         チェックマスタ４     (CPCK1103.INC)              *
      *         （１１０２）                                     *
      *         データチェック機能情報３                         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CK4-1103-REC.
           03  CK4-1103-KEY.
               05  CK4-1103-HOSPNUM        PIC 9(2).
      *管理コード
               05  CK4-1103-KANRICD        PIC  X(04).
      *区分コード
               05  CK4-1103-KBNCD          PIC  X(08).
      *有効開始年月日
               05  CK4-1103-STYUKYMD       PIC  X(08).
      *有効終了年月日
               05  CK4-1103-EDYUKYMD       PIC  X(08).
      *患者ＩＤ
           03  CK4-1103-PTID               PIC  9(10).
      *管理テーブル
           03  CK4-1103-TBL.
      *
      *        エラー区分(空白or２：エラーとしない／１：エラーとする）
               05  CK4-1103-ERRKBN         PIC  X(01)
                                           OCCURS  500.
      *端末ＩＤ
           03  CK4-1103-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  CK4-1103-OPID               PIC  X(16).
      *作成年月日
           03  CK4-1103-CREYMD             PIC  X(08).
      *更新年月日
           03  CK4-1103-UPYMD              PIC  X(08).
      *更新時間
           03  CK4-1103-UPHMS              PIC  X(06).
