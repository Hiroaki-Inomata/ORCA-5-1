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
      *    ＡＰＩ保険組合せ処理パラメタ　　COPY (CPORCSHKNGET.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  14/11/12  確認日付他追加
      * 04.08.00     NACL-多々納  17/01/XX  処理区分３、保険組合せ情報追加
      * 05.00.00     NACL-多々納  18/11/XX  診療日付追加
      * 05.00.00     NACL-多々納  19/06/XX  公費確認日付追加
      * 05.00.00     NACL-多々納  20/04/XX  被保険者証枝番追加
      ************************************************************
       01  ORCSAPIHKNAREA.
      *入力
      *処理区分（１：自動選択する）
           03  ORCSAPIHKN-KBN              PIC  X(01).
      *診療科（前回受診履歴より）
           03  ORCSAPIHKN-SRYKA            PIC  X(02).
      *処理区分２（１：患者登録時（すべて）、２：年月範囲、以外：当日）
           03  ORCSAPIHKN-KBN2             PIC  X(01).
      *H29.1
      *処理区分３（１：対象２００件まで）
           03  ORCSAPIHKN-KBN3             PIC  X(01).
      *H30.11
      *診療日付（未設定はシステム日付）
           03  ORCSAPIHKN-SRYYMD           PIC  X(08).
      *保険情報
           03  ORCSAPIHKN-IN-HKNAREA.
      *        保険種類
               05  ORCSAPIHKN-IN-HKNNUM            PIC X(03).
      *        保険者番号
               05  ORCSAPIHKN-IN-HKNJANUM          PIC X(08).
      *        保険種類名称
               05  ORCSAPIHKN-IN-HKNNUM-NAME       PIC X(10).
      *        記号
               05  ORCSAPIHKN-IN-MARK              PIC X(80).
      *        番号
               05  ORCSAPIHKN-IN-NUMBER            PIC X(80).
      *        有効期間
               05  ORCSAPIHKN-IN-TEKSTYMD          PIC X(08).
               05  ORCSAPIHKN-IN-TEKEDYMD          PIC X(08).
      *        公費情報
               05  ORCSAPIHKN-IN-INSURANCE-INFO
                                       OCCURS  4 TIMES.
                   07  ORCSAPIHKN-IN-KOHNUM            PIC X(04).
                   07  ORCSAPIHKN-IN-KOHNUM-NAME       PIC X(10).
                   07  ORCSAPIHKN-IN-FTNJANUM          PIC X(08).
                   07  ORCSAPIHKN-IN-JKYSNUM           PIC X(20).
      *    保険組合せ
           03  ORCSAPIHKN-IN-HKNCOMBI      PIC X(04).
      *
      *出力領域
           03  ORCSAPIHKN-OUT-AREA.
      *        対象保険組合せ
               05  ORCSAPIHKN-OT-HKNCOMBI      PIC X(04).
      *有効保険一覧
           03  ORCSAPIHKN-HKNCOMBI-AREA.
               05  ORCSAPIHKN-HKNCOMBI-TBL     OCCURS   200.
      *            保険組合せ
                   07  ORCSAPIHKN-THKNCOMBI        PIC  X(04).
      *H29.1
      *            保険組合せ負担割合（外来）
                   07  ORCSAPIHKN-COMB-RATEG       PIC  9(03).
      *            保険組合せ負担割合（入院）
                   07  ORCSAPIHKN-COMB-RATEN       PIC  9(03).
      *            保険組合せ有効期間
                   07  ORCSAPIHKN-COMB-TEKSTYMD    PIC  X(10).
                   07  ORCSAPIHKN-COMB-TEKEDYMD    PIC  X(10).
                   07  ORCSAPIHKN-COMBN-TEKSTYMD   PIC  X(08).
                   07  ORCSAPIHKN-COMBN-TEKEDYMD   PIC  X(08).
      *
      *            保険種別
                   07  ORCSAPIHKN-HKNNUM           PIC  X(03).
      *            保険種別名称
                   07  ORCSAPIHKN-HKNNUM-NAME      PIC  X(20).
      *            保険者番号
                   07  ORCSAPIHKN-HKNJANUM         PIC  X(08).
      *            記号
                   07  ORCSAPIHKN-KIGO             PIC  X(80).
      *            番号
                   07  ORCSAPIHKN-NUM              PIC  X(80).
      *R02.4       番号枝番
                   07  ORCSAPIHKN-EDABAN           PIC  X(02).
      *            継続区分
                   07  ORCSAPIHKN-CONTKBN          PIC  X(01).
      *            補助区分
                   07  ORCSAPIHKN-HOJOKBN          PIC  X(01).
      *            補助区分名称
                   07  ORCSAPIHKN-HOJOKBN-NAME     PIC  X(30).
      *            本人家族区分
                   07  ORCSAPIHKN-HONKZKKBN        PIC  X(01).
      *            被保険者名称
                   07  ORCSAPIHKN-HIHKNJANAME      PIC  X(100).
      *            適用期間（表示）
                   07  ORCSAPIHKN-TEKSTYMD         PIC  X(10).
                   07  ORCSAPIHKN-TEKEDYMD         PIC  X(10).
      *            適用期間
                   07  ORCSAPIHKN-NAI-TEKSTYMD     PIC  X(08).
                   07  ORCSAPIHKN-NAI-TEKEDYMD     PIC  X(08).
      *            資格取得日
                   07  ORCSAPIHKN-SKKGETYMD        PIC  X(10).
                   07  ORCSAPIHKN-NAI-SKKGETYMD    PIC  X(08).
      *            確認日付
                   07  ORCSAPIHKN-KAKUNINYMD       PIC  X(10).
                   07  ORCSAPIHKN-NAI-KAKUNINYMD   PIC  X(08).
      *            保険ＩＤ
                   07  ORCSAPIHKN-NAI-HKNID        PIC  9(10).
      *            労災区分
                   07  ORCSAPIHKN-RSI-HKNKBN       PIC  X(01).
      *            非表示区分
                   07  ORCSAPIHKN-HYOJIKBN         PIC  X(01).
      *            公費種類・公費ＩＤ
                   07  ORCSAPIHKN-KOH-G.
                       09  ORCSAPIHKN-KOH-OCC      OCCURS   4.
                           11  ORCSAPIHKN-KOHNUM           PIC  X(03).
                           11  ORCSAPIHKN-KOHNUM-NAME      PIC  X(20).
                           11  ORCSAPIHKN-FTNJANUM         PIC  X(08).
                           11  ORCSAPIHKN-JKYSNUM          PIC  X(20).
      *                    適用期間
                           11  ORCSAPIHKN-KOH-TEKSTYMD     PIC  X(10).
                           11  ORCSAPIHKN-KOH-TEKEDYMD     PIC  X(10).
      *R01.6
      *                    確認日付
                           11  ORCSAPIHKN-KOH-KAKUNINYMD   PIC  X(10).
      *                    入院−負担率
                           11  ORCSAPIHKN-RATE-ADMISSION   PIC X(05).
      *                    入院−固定額
                           11  ORCSAPIHKN-MONEY-ADMISSION  PIC X(06).
      *                    外来−負担率
                           11  ORCSAPIHKN-RATE-OUTPATIENT  PIC X(05).
      *                     外来−固定額
                           11  ORCSAPIHKN-MONEY-OUTPATIENT PIC X(06).
      *                    公費ＩＤ
                           11  ORCSAPIHKN-NAI-KOHID        PIC 9(10).
      *        対象保険組合せ件数
               05  ORCSAPIHKN-OT-MAX           PIC 9(04).
      *エラーコード
           03  ORCSAPIHKN-ERRCD                    PIC 9(01).
      *
      *
