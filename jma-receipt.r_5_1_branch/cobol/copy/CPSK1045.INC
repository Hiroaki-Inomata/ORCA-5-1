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
      *         システム管理マスタ     (CPSK1043.INC)   *
      *         （１０４５）                            *
      *         予約機能情報                            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  10/06/21  予約受付連動追加
      *  04.07.00    NACL-多々納  11/06/XX  受付保険組合せ区分追加
      ************************************************************
       01  SYS-1045-REC.
           03  SYS-1045-KEY.
               05  SYS-1045-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1045-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1045-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1045-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1045-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1045-TBL.
      *初期ドクター表示
               05  SYS-1045-INIT-DRCD   PIC  X(05).
      *予約一覧の初期表示
               05  SYS-1045-YYKINIT-FLG PIC  X(01).
      *予約枠最大件数
               05  SYS-1045-YYKMAXCNT   PIC  9(02).
      *予約票発行区分
               05  SYS-1045-YYKHYO-KBN  PIC  X(01).
      *予約患者番号連動区分
               05  SYS-1045-YYKPTNUM-KBN   PIC  X(01).
      *予約受付番号連動区分
               05  SYS-1045-YYKUKE-KBN  PIC  X(01).
      *受付保険組合せ区分
               05  SYS-1045-UKEHKNCHK-KBN    PIC  X(01).
      *予備
               05  SYS-1045-YOBI        PIC  X(488).
      *端末ＩＤ
           03  SYS-1045-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1045-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1045-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1045-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1045-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1045-UPD-KEY.
               05  SYS-1045-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1045-UPD-KANRICD     PIC X(04).
               05  SYS-1045-UPD-KBNCD       PIC X(08).
               05  SYS-1045-UPD-STYUKYMD    PIC X(08).
               05  SYS-1045-UPD-EDYUKYMD    PIC X(08).
