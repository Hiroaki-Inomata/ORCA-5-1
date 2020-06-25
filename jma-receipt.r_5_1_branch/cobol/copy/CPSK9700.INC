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
      *         システム管理マスタ     (CPSK9700.INC)   *
      *         （９７００）                            *
      *         ユーザプログラム起動                 *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-9700-REC.
           03  SYS-9700-KEY.
               05  SYS-9700-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-9700-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-9700-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-9700-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-9700-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-9700-TBL.
               05  SYS-9700-TBL-OCC     OCCURS   25.
      *指示画面ＩＤ
                   07  SYS-9700-GYOUMUID         PIC  X(10).
      *指示管理コード
                   07  SYS-9700-GYOUMU-KANRICD   PIC  X(04).
      *指示画面初期表示
                   07  SYS-9700-GYOUMU-FLG       PIC  X(01).
      *指示件数表示
                   07  SYS-9700-GYOUMU-CNT       PIC  9(02).
      *予備
                   07  SYS-9700-YOBI             PIC  X(03).
      *端末ＩＤ
           03  SYS-9700-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-9700-OPID            PIC  X(16).
      *作成年月日
           03  SYS-9700-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-9700-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-9700-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-9700-UPD-KEY.
               05  SYS-9700-UPD-HOSPNUM     PIC 9(2).
               05  SYS-9700-UPD-KANRICD     PIC X(04).
               05  SYS-9700-UPD-KBNCD       PIC X(08).
               05  SYS-9700-UPD-STYUKYMD    PIC X(08).
               05  SYS-9700-UPD-EDYUKYMD    PIC X(08).
