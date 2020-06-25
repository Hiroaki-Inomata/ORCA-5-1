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
      *         （９７０１）                            *
      *         ユーザプログラム起動                 *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-9701-REC.
           03  SYS-9701-KEY.
               05  SYS-9701-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-9701-KANRICD     PIC  X(04).
      *区分コード（9701:P02 , 9702:U02 , 9703:K03 )
               05  SYS-9701-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-9701-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-9701-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-9701-TBL.
      *処理・帳票名
               05  SYS-9701-SYORINM        PIC  X(30).
      *シェル名
               05  SYS-9701-SHELL          PIC  X(100).
      *プリンタ名
               05  SYS-9701-PRTNM          PIC  X(16).
      *実行可否
               05  SYS-9701-EXECUTE-SW     PIC  X(01).
      *実行形式
               05  SYS-9701-EXECUTE-YTPE   PIC  X(01).
      *予備
           03  SYS-9701-YOBI            PIC  X(352).
      *端末ＩＤ
           03  SYS-9701-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-9701-OPID            PIC  X(16).
      *作成年月日
           03  SYS-9701-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-9701-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-9701-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-9701-UPD-KEY.
               05  SYS-9701-UPD-HOSPNUM     PIC 9(2).
               05  SYS-9701-UPD-KANRICD     PIC X(04).
               05  SYS-9701-UPD-KBNCD       PIC X(08).
               05  SYS-9701-UPD-STYUKYMD    PIC X(08).
               05  SYS-9701-UPD-EDYUKYMD    PIC X(08).
