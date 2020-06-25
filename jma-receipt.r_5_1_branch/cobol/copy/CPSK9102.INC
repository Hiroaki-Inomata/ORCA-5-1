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
      *         システム管理マスタ     (CPSK9102.INC)            *
      *         （９１０２）                                     *
      *         ＥＦファイルデータ                               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    15/08/24  公費のみの診療（医療保険のな
      *                                     い診療）のＥＦ出力可否対応
      ************************************************************
       01  SYS-9102-REC.
           03  SYS-9102-KEY.
               05  SYS-9102-HOSPNUM    PIC 9(02).
      *管理コード
               05  SYS-9102-KANRICD    PIC X(04).
      *区分コード
               05  SYS-9102-KBNCD      PIC X(08).
      *有効開始年月日
               05  SYS-9102-STYUKYMD   PIC X(08).
      *有効終了年月日
               05  SYS-9102-EDYUKYMD   PIC X(08).
      *管理テーブル
           03  SYS-9102-KANRITBL.
      *ＥＦファイルデータ作成
               05  SYS-9102-EFOUT      PIC X(01).
      *ＥＦファイルデータファイルデイレクトリ
               05  SYS-9102-FILE-DIR   PIC X(200).
      *データ識別番号記録
               05  SYS-9102-NUMBER     PIC X(01).
      *退院時処方指示
               05  SYS-9102-SRYCD-TBL.
                   07  SYS-9102-SRYCD  PIC X(09)   OCCURS  2.
      *公費のみのレセプト出力可否
               05  SYS-9102-KOH-RECEIPT
                                       PIC X(01).
      *予備
               05  SYS-9102-YOBI       PIC X(279).
      *端末ＩＤ
           03  SYS-9102-TERMID         PIC X(16).
      *オペーレータＩＤ
           03  SYS-9102-OPID           PIC X(16).
      *作成年月日
           03  SYS-9102-CREYMD         PIC X(08).
      *更新年月日
           03  SYS-9102-UPYMD          PIC X(08).
      *更新時間
           03  SYS-9102-UPHMS          PIC X(06).
