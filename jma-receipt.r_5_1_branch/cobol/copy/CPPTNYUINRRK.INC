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
      *    患者入院履歴              COPY (CPPTNYUINRRK.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTNYUINRRK-REC.
      *キー
           03  PTNYUINRRK-KEY.
      *医療機関ＩＤ
               05  PTNYUINRRK-HOSPNUM              PIC 9(2).
      *患者ＩＤ
               05  PTNYUINRRK-PTID                PIC  9(10).
      *入院履歴番号（入院１回で＋１ずつ採番）
               05  PTNYUINRRK-RRKNUM              PIC  9(03).
      *入院履歴枝番号（転科・転室・転棟時に採番）
               05  PTNYUINRRK-RRKEDANUM           PIC  9(03).
      *病棟番号
           03  PTNYUINRRK-BTUNUM                  PIC  X(02).
      *病棟名称
           03  PTNYUINRRK-BTUNAME                 PIC  X(20).
      *病室番号
           03  PTNYUINRRK-BRMNUM                  PIC  X(08).
      *入院科
           03  PTNYUINRRK-NYUINKA                 PIC  X(02).
      *保険組合せ番号
           03  PTNYUINRRK-HKNCOMBINUM             PIC  X(04).
      *入院日
           03  PTNYUINRRK-NYUINYMD                PIC  X(08).
      *退院日
           03  PTNYUINRRK-TAIINYMD                PIC  X(08).
      *退院事由
           03  PTNYUINRRK-TAIINCD                 PIC  X(02).
      *特定入院料区分（１：医療機関／２：病棟／３：病室）
           03  PTNYUINRRK-TOKUTEI-KBN             PIC  X(01).
      *特定入院料
           03  PTNYUINRRK-TOKUTEI-NYUIN           PIC  X(02).
      *特定入院料算定要件区分（空白：該当する／１：該当しない）
           03  PTNYUINRRK-TOKU-SANTEIYOUKENKBN    PIC  X(01).
      *入院加算コード
           03  PTNYUINRRK-KSNSRYCD-G.
               05  PTNYUINRRK-KSNSRYCD-OCC        OCCURS   3.
                   07  PTNYUINRRK-KSNSRYCD        PIC  X(09).
      *室料差額
           03  PTNYUINRRK-RM-SAGAKU               PIC  X(02).
      *定期請求区分
           03  PTNYUINRRK-TEIKI-SEIKYUKBN         PIC  X(01).
      *検索表示区分
           03  PTNYUINRRK-KENSAKU-DISPKBN         PIC  X(01).
      *初回継続区分
           03  PTNYUINRRK-SHOKAIKBN               PIC  X(01).
      *初回番号
           03  PTNYUINRRK-SHONUM                  PIC  9(03).
      *担当医１（主治医）
           03  PTNYUINRRK-DRCD1                   PIC  X(05).
      *担当医２
           03  PTNYUINRRK-DRCD2                   PIC  X(05).
      *担当医３
           03  PTNYUINRRK-DRCD3                   PIC  X(05).
      *状態区分（１:入院/２:転科/３:転棟/
      *          ４:転室/５:他院入院／６:自院歴/７:入院料変更）
           03  PTNYUINRRK-JTIKBN                  PIC  X(01).
      *算定病棟
           03  PTNYUINRRK-SANTEI-BTUNUM           PIC  X(02).
      *算定病室
           03  PTNYUINRRK-SANTEI-BRMNUM           PIC  X(08).
      *転入日
           03  PTNYUINRRK-TENNYUYMD               PIC  X(08).
      *転出日
           03  PTNYUINRRK-TENSTUYMD               PIC  X(08).
      *通算対象日数（他院歴で考慮する日数）
           03  PTNYUINRRK-TAISYONISSU             PIC  9(05).
      *通算日数１（９０日越え算定用）
           03  PTNYUINRRK-TUSANNISSU1             PIC  9(05).
      *通算日数２（１８０日越え算定用）
           03  PTNYUINRRK-TUSANNISSU2             PIC  9(05).
      *特定入院−通算対象日数
           03  PTNYUINRRK-TOKU-TAISYONISSU        PIC  9(05).
      *自院歴−入院中区分
      *  自院歴で作成した歴と
      *  入院登録で作成した歴をレセプト上１つの入院期間としたい
      *  場合、自院歴のデータに値を設定する。
      *  （０：退院済／１：入院中）
           03  PTNYUINRRK-NYUINCHUKBN             PIC  X(01).
      *同日再入院区分（１：自院退院からの再入院／２：他院歴からの再入院）
           03  PTNYUINRRK-DOUJITSUKBN             PIC  X(01).
      *入院履歴最大枝番号
           03  PTNYUINRRK-MAXEDANUM               PIC  9(03).
      *入院食事区分（０：食なし／１：食あり／２：特食／３：流動食）
      *　　　　　　（Ａ：変更なし／Ｃ：個別指示／Ｂ：個別指示変更なし）
           03  PTNYUINRRK-SKJKBN                  PIC  X(01).
           03  PTNYUINRRK-SKJKBN-KOBETSU-G.
      *入院食事区分　朝（０：食なし／１：食あり／２：特食／３：流動食）
               05  PTNYUINRRK-SKJKBN-ASA              PIC  X(01).
      *入院食事区分　昼（０：食なし／１：食あり／２：特食／３：流動食）
               05  PTNYUINRRK-SKJKBN-HIRU             PIC  X(01).
      *入院食事区分　夕（０：食なし／１：食あり／２：特食／３：流動食）
               05  PTNYUINRRK-SKJKBN-YU               PIC  X(01).
           03  PTNYUINRRK-SKJKBN-KOBETSU-R
                           REDEFINES PTNYUINRRK-SKJKBN-KOBETSU-G.
               05  PTNYUINRRK-SKJKBN-KOBETSU      PIC  X(01)
                                                  OCCURS 3.
      *入院日食事開始時間区分
      *（０：食なし／１：朝から／２：昼から／３：夕から）
           03  PTNYUINRRK-TIMEKBN-IN              PIC  X(01).
      *選定入院区分（１：選定対象／２：選定対象外）
           03  PTNYUINRRK-SENTEIKBN               PIC  X(01).
      *他院関係区分（１：特別な関係にある医療機関）
           03  PTNYUINRRK-TAINRELKBN              PIC  X(01).
      *介護異動区分（１：介護施設からの急性増悪による異動）
           03  PTNYUINRRK-KAIGOKBN                PIC  X(01).
      *入院カレンダー設定区分（１：入院料を算定しない／２：入院料を算定する）
           03  PTNYUINRRK-NYUCALKBN               PIC  X(01).
      *端末ＩＤ
           03  PTNYUINRRK-TERMID                  PIC  X(16).
      *オペーレータＩＤ
           03  PTNYUINRRK-OPID                    PIC  X(16).
      *作成年月日
           03  PTNYUINRRK-CREYMD                  PIC  X(08).
      *更新年月日
           03  PTNYUINRRK-UPYMD                   PIC  X(08).
      *更新時間
           03  PTNYUINRRK-UPHMS                   PIC  X(06).
