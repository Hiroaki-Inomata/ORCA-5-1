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
      *         システム管理マスタ     (CPSK9101.INC)            *
      *         （９１０１）                                     *
      *         医療情報連携基盤用情報                           *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-9101-REC.
           03  SYS-9101-KEY.
               05  SYS-9101-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-9101-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-9101-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-9101-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-9101-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-9101-KANRITBL.
      *連携基盤用データ作成
               05  SYS-9101-COOPBASE    PIC X(01).
      *連携基盤用データファイルデイレクトリ
               05  SYS-9101-FILE-DIR    PIC X(200).
      *傷病名レコード記録
               05  SYS-9101-SYREC       PIC X(01).
      *自費保険分作成
               05  SYS-9101-JIHI        PIC X(01).
      *患者番号記録
               05  SYS-9101-PTNUM       PIC X(01).
      *傷病名名称全記録
               05  SYS-9101-SYBYO       PIC X(01).
      *診療行為、医薬品レコードに名称記録
               05  SYS-9101-NMREC       PIC X(01).
      *連携基盤用当日受診分
               05  SYS-9101-COOPBASE2   PIC X(01).
      *予備
               05  SYS-9101-YOBI        PIC X(293).
      *         05  SYS-9101-YOBI        PIC X(294).
      *         05  SYS-9101-YOBI        PIC X(295).
      *         05  SYS-9101-YOBI        PIC X(296).
      *端末ＩＤ
           03  SYS-9101-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-9101-OPID        PIC  X(16).
      *作成年月日
           03  SYS-9101-CREYMD      PIC  X(08).
      *更新年月日
           03  SYS-9101-UPYMD       PIC  X(08).
      *更新時間
           03  SYS-9101-UPHMS       PIC  X(06).
