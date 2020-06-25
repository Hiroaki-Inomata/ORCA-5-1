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
      *         システム管理マスタ     (CPSK1039.INC)   *
      *         （１０３８）                            *
      *         収納機能情報                            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1039-REC.
           03  SYS-1039-KEY.
               05  SYS-1039-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1039-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1039-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1039-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1039-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1039-TBL.
      *入金管理区分
               05  SYS-1039-NYKNKNRKBN  PIC  X(01).
      *請求書兼領収書発行区分（外来）
               05  SYS-1039-SKYPRTGKBN  PIC  X(01).
      *請求書兼領収書発行区分（入院）
               05  SYS-1039-SKYPRTNKBN  PIC  X(01).
      *統計時間
               05  SYS-1039-STIME.
                   07  SYS-1039-STIME-HH   PIC  X(02).
                   07  SYS-1039-STIME-MM   PIC  X(02).
      *仮締時間
               05  SYS-1039-KARITIME.
                   07  SYS-1039-KARITIME-OCC1          OCCURS  2.
                       09  SYS-1039-KARIDAY    PIC X   OCCURS  7.
                       09  SYS-1039-KARITIME-OCC2      OCCURS  2.
                           11  SYS-1039-KARISTTIME.
                               13  SYS-1039-KARIST-HH  PIC X(02).
                               13  SYS-1039-KARIST-MM  PIC X(02).
                           11  SYS-1039-KARIEDTIME.
                               13  SYS-1039-KARIED-HH  PIC X(02).
                               13  SYS-1039-KARIED-MM  PIC X(02).
      *診療費明細書発行区分（外来）
               05  SYS-1039-SRYPRTGKBN  PIC  X(01).
      *診療費明細書発行区分（入院）
               05  SYS-1039-SRYPRTNKBN  PIC  X(01).
      *一括入金画面請求書兼領収書発行区分（入院）
               05  SYS-1039-S06SKYPRTNKBN
                                        PIC  X(01).
      *一括入金画面請求書兼領収書発行区分（外来）
               05  SYS-1039-S06SKYPRTGKBN
                                        PIC  X(01).
      *一括入金画面外来分発行方法
               05  SYS-1039-S06SKYSUMKBN
                                        PIC  X(01).
      *請求一覧画面外来請求書兼領収書発行方法
               05  SYS-1039-S02SKYSAIKBN
                                        PIC  X(01).
      *請求一覧画面外来請求書兼領収書未収印字区分
               05  SYS-1039-S02SKYMISYUPRTKBN
                                        PIC  X(01).
      *請求確認画面外来請求書兼領収書未収印字区分
               05  SYS-1039-S03SKYMISYUPRTKBN
                                        PIC  X(01).
      *請求一覧画面月別請求書兼領収書未収印字区分
               05  SYS-1039-TUKISKYMISYUPRTKBN
                                        PIC  X(01).
      *支払証明書印刷画面支払証明書発行区分
               05  SYS-1039-S04SHIHARAIKBN      PIC X(01).
      *支払証明書印刷画面入外区分
               05  SYS-1039-S04NYUGAIKBN        PIC X(01).
      *支払証明書印刷画面期間指定
               05  SYS-1039-S04SRYKBN           PIC X(01).
      *支払証明書印刷画面期間区分
               05  SYS-1039-S04KIKANKBN         PIC X(01).
      *支払証明書印刷画面請求内容の印字
               05  SYS-1039-S04DSPKBN           PIC X(01).
      *支払証明書印刷画面期間終了後の支払い
               05  SYS-1039-S04SYORIKBN         PIC X(01).
      *支払証明書印刷画面期間開始前診療分の支払い
               05  SYS-1039-S04SUMKBN           PIC X(01).
      *
      *請求一覧画面並び順
               05  SYS-1039-S02SORTKBN          PIC X(01).
      *一括再計算順
               05  SYS-1039-IKTSORTKBN          PIC X(01).
      *一括再計算方法
               05  SYS-1039-IKTKEISANKBN        PIC X(01).
      *一括入金診療年月指定区分
               05  SYS-1039-IKTSRYYMKBN         PIC X(01).
      *月別請求書発行方法
               05  SYS-1039-TUKIGRPKBN          PIC X(01).
      *月別請求書集計区分
               05  SYS-1039-TUKISUMKBN          PIC X(01).
      *請求書未収金集計期間区分（入院）
               05  SYS-1039-MISYUKIKANKBN       PIC X(01).
      *再計算該当者チェック
               05  SYS-1039-SAIKEISANSKYCHK     PIC X(01).
      *支払証明書印刷画面自費内訳明細書
               05  SYS-1039-S04JIHIPRTKBN       PIC X(01).
      *請求一覧画面コラムリスト初期表示
               05  SYS-1039-S02MODE             PIC X(01).
      *請求一覧画面コラムリスト最大件数
               05  SYS-1039-S02LIMIT            PIC 9(03).
      *予備
               05  SYS-1039-YOBI                PIC  X(418).
      *端末ＩＤ
           03  SYS-1039-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1039-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1039-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1039-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1039-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1039-UPD-KEY.
               05  SYS-1039-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1039-UPD-KANRICD     PIC X(04).
               05  SYS-1039-UPD-KBNCD       PIC X(08).
               05  SYS-1039-UPD-STYUKYMD    PIC X(08).
               05  SYS-1039-UPD-EDYUKYMD    PIC X(08).
