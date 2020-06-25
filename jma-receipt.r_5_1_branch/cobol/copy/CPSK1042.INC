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
      *         システム管理マスタ     (CPSK1042.INC)   *
      *         （１０４２）                            *
      *         病名機能情報                            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  ver.2.6.0
      *  00.00.01    NACL-藤原    05/04/20  移行病名警告メッセージ表示
      *                                     有無区分
      *                                     廃止年月日警告メッセージ表示
      *                                     有無区分
      *                                     診療科初期表示区分
      *                                     表示対象期間初期表示区分
      *                                     同一病名警告メッセージ表示
      *                                     有無区分
      *                                     の追加
      *
      *  04.06.01    NACL-藤原    10/06/16  疑いフラグ設定対応
      *                                     疑いフラグ設定区分の追加
      *
      *  04.08.01    NACL-藤原    16/01/20  転帰区分の死亡変更時の患者
      *                                     情報更新対応
      *                                     死亡区分更新確認メッセージ表示
      *                                     有無区分の追加
      ************************************************************
       01  SYS-1042-REC.
           03  SYS-1042-KEY.
               05  SYS-1042-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1042-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1042-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1042-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1042-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1042-TBL.
      *主病名確認メッセージ表示有無区分
               05  SYS-1042-MSG1-DSPKBN PIC  X(01).
      *移行病名警告メッセージ表示有無区分
               05  SYS-1042-MSG2-DSPKBN PIC  X(01).
      *廃止年月日警告メッセージ表示有無区分
               05  SYS-1042-MSG3-DSPKBN PIC  X(01).
      *同一病名警告メッセージ表示有無区分
               05  SYS-1042-MSG4-DSPKBN PIC  X(01).
      *診療科初期表示区分
               05  SYS-1042-SRYKA-DSPKBN
                                        PIC  X(01).
      *表示対象期間初期表示区分
               05  SYS-1042-SYOKBN-DSPKBN
                                        PIC  X(01).
      *疑いフラグ設定区分
               05  SYS-1042-UTAGAIFLG-KBN
                                        PIC  X(01).
      *予備
               05  SYS-1042-YOBI1
                                        PIC  X(09).
      *コメント症状詳記区分初期設定
               05  SYS-1042-SJKBN-SET
                                        PIC  X(02).
      *死亡区分更新確認メッセージ表示有無区分
               05  SYS-1042-MSG5-DSPKBN PIC  X(01).
      *予備
      ******** 05                       PIC  X(494).
               05  SYS-1042-YOBI        PIC  X(481).
      *端末ＩＤ
           03  SYS-1042-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1042-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1042-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1042-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1042-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1042-UPD-KEY.
               05  SYS-1042-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1042-UPD-KANRICD     PIC X(04).
               05  SYS-1042-UPD-KBNCD       PIC X(08).
               05  SYS-1042-UPD-STYUKYMD    PIC X(08).
               05  SYS-1042-UPD-EDYUKYMD    PIC X(08).
