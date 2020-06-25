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
      *         システム管理マスタ     (CPSK2010.INC)            *
      *         （２０１０）                                     *
      *         公費付加情報                                     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-2010-REC.
           03  SYS-2010-KEY.
               05  SYS-2010-HOSPNUM         PIC  9(02).
      *管理コード
               05  SYS-2010-KANRICD         PIC  X(04).
      *区分コード
               05  SYS-2010-KBNCD           PIC  X(08).
      *有効開始年月日
               05  SYS-2010-STYUKYMD        PIC  X(08).
      *有効終了年月日
               05  SYS-2010-EDYUKYMD        PIC  X(08).
      *
      *実日数省略記載
           03  SYS-2010-JNISSUKBN           PIC  X(01).
           03  SYS-2010-JNISSU-TAIKBN       PIC  X(01).
      *請求点省略記載
           03  SYS-2010-TOTALTENKBN         PIC  X(01).
           03  SYS-2010-TOTALTEN-TAIKBN     PIC  X(01).
      *一部負担金省略記載
           03  SYS-2010-FTNMONEYKBN-G       PIC  X(01).
           03  SYS-2010-FTNMONEY-TAIKBN-G   PIC  X(01).
           03  SYS-2010-FTNMONEYKBN-N       PIC  X(01).
           03  SYS-2010-FTNMONEY-TAIKBN-N   PIC  X(01).
      *一部負担金０円記載（記録）
           03  SYS-2010-FTNZEROKBN-G        PIC  X(01).
           03  SYS-2010-FTNZERO-TAIKBN-G    PIC  X(01).
           03  SYS-2010-FTNZEROKBN-N        PIC  X(01).
           03  SYS-2010-FTNZERO-TAIKBN-N    PIC  X(01).
      *入院食事療養省略記載
           03  SYS-2010-SKJKBN              PIC  X(01).
           03  SYS-2010-SKJ-TAIKBN          PIC  X(01).
      *ＯＣＲ実日数請求点省略記載
           03  SYS-2010-OCRKBN              PIC  X(01).
           03  SYS-2010-OCR-TAIKBN          PIC  X(01).
      *月途中受給者証変更時負担計算
           03  SYS-2010-JYUHENKBN           PIC  X(01).
           03  SYS-2010-JYUHEN-TAIKBN       PIC  X(01).
      *請求書公費再掲欄記載
           03  SYS-2010-SAIKEIKBN           PIC  X(01).
           03  SYS-2010-SAIKEI-TAIKBN       PIC  X(01).
      *全国公費併用分請求点集計
           03  SYS-2010-SYUKEIKBN           PIC  X(01).
           03  SYS-2010-SYUKEI-TAIKBN       PIC  X(01).
      *日・月上限額負担計算
           03  SYS-2010-JGNFTNKBN           PIC  X(01).
           03  SYS-2010-JGNFTN-TAIKBN       PIC  X(01).
      *
      *一部負担金金額集計
           03  SYS-2010-FTNKEIKBN-G         PIC  X(01).
           03  SYS-2010-FTNKEI-TAIKBN-G     PIC  X(01).
           03  SYS-2010-FTNKEIKBN-N         PIC  X(01).
           03  SYS-2010-FTNKEI-TAIKBN-N     PIC  X(01).
      *保険欄−一部負担金記載（高齢者）
           03  SYS-2010-HKNFTNKBN-K         PIC  X(01).
           03  SYS-2010-HKNFTN-TAIKBN-K     PIC  X(01).
      *保険欄−一部負担金記載（70歳未満）
           03  SYS-2010-HKNFTNKBN-Y         PIC  X(01).
           03  SYS-2010-HKNFTN-TAIKBN-Y     PIC  X(01).
      *設定と異なる記載設定
           03  SYS-2010-RECEKSIKBN-G        PIC  X(01).
           03  SYS-2010-RECEKSI-TAIKBN-G    PIC  X(01).
           03  SYS-2010-RECEKSIKBN-N        PIC  X(01).
           03  SYS-2010-RECEKSI-TAIKBN-N    PIC  X(01).
      *チェック対象上限額設定
           03  SYS-2010-CHKJGNKBN-G         PIC  X(01).
           03  SYS-2010-CHKJGN-TAIKBN-G     PIC  X(01).
           03  SYS-2010-CHKJGNKBN-N         PIC  X(01).
           03  SYS-2010-CHKJGN-TAIKBN-N     PIC  X(01).
      *公費請求テーブル作成
           03  SYS-2010-KOHSKYKBN-G         PIC  X(01).
           03  SYS-2010-KOHSKY-TAIKBN-G     PIC  X(01).
           03  SYS-2010-KOHSKYKBN-N         PIC  X(01).
           03  SYS-2010-KOHSKY-TAIKBN-N     PIC  X(01).
      *
      *患者登録−所得者情報−月上限額入力
           03  SYS-2010-PTJGNKBN            PIC  X(01).
           03  SYS-2010-PTJGN-TAIKBN        PIC  X(01).
      *設定額未満・以上で異なる負担金計算
           03  SYS-2010-IRYOKEIKBN-G        PIC  X(01).
           03  SYS-2010-IRYOKEI-TAIKBN-G    PIC  X(01).
           03  SYS-2010-IRYOKEIKBN-N        PIC  X(01).
           03  SYS-2010-IRYOKEI-TAIKBN-N    PIC  X(01).
           03  SYS-2010-SKJKEIKBN           PIC  X(01).
           03  SYS-2010-SKJKEI-TAIKBN       PIC  X(01).
      *本人タブ：食事療養・生活療養（食事）・生活療養（環境）
           03  SYS-2010-SKJRYOKBN-HON       PIC  X(01).
           03  SYS-2010-SKJRYO-TAIKBN-HON   PIC  X(01).
           03  SYS-2010-SEIRYOKBN-HON       PIC  X(01).
           03  SYS-2010-SEIRYO-TAIKBN-HON   PIC  X(01).
           03  SYS-2010-SEIKANKBN-HON       PIC  X(01).
           03  SYS-2010-SEIKAN-TAIKBN-HON   PIC  X(01).
      *低所得タブ：食事療養・生活療養（食事）・生活療養（環境）
           03  SYS-2010-SKJRYOKBN-TSY       PIC  X(01).
           03  SYS-2010-SKJRYO-TAIKBN-TSY   PIC  X(01).
           03  SYS-2010-SEIRYOKBN-TSY       PIC  X(01).
           03  SYS-2010-SEIRYO-TAIKBN-TSY   PIC  X(01).
           03  SYS-2010-SEIKANKBN-TSY       PIC  X(01).
           03  SYS-2010-SEIKAN-TAIKBN-TSY   PIC  X(01).
      *低年金タブ：食事療養・生活療養（食事）・生活療養（環境）
           03  SYS-2010-SKJRYOKBN-TNK       PIC  X(01).
           03  SYS-2010-SKJRYO-TAIKBN-TNK   PIC  X(01).
           03  SYS-2010-SEIRYOKBN-TNK       PIC  X(01).
           03  SYS-2010-SEIRYO-TAIKBN-TNK   PIC  X(01).
           03  SYS-2010-SEIKANKBN-TNK       PIC  X(01).
           03  SYS-2010-SEIKAN-TAIKBN-TNK   PIC  X(01).
      *
      *保険欄の括弧再掲記載（高齢者）
           03  SYS-2010-KAKKBN-K            PIC  X(01).
           03  SYS-2010-KAK-TAIKBN-K        PIC  X(01).
      *保険欄の括弧再掲（下）記載（高齢者）
      *　主＋全＋地（＋長）（分点ありの場合）
           03  SYS-2010-KAK21KBN-K          PIC  X(01).
           03  SYS-2010-KAK21-TAIKBN-K      PIC  X(01).
      *　主＋全＋地　　　　（分点なしの場合）
           03  SYS-2010-KAK22KBN-K          PIC  X(01).
           03  SYS-2010-KAK22-TAIKBN-K      PIC  X(01).
      *　主＋全＋地＋長　　（分点なしの場合）
           03  SYS-2010-KAK23KBN-K          PIC  X(01).
           03  SYS-2010-KAK23-TAIKBN-K      PIC  X(01).
      *保険欄の括弧再掲（下）金額計算（高齢者）
      *　主＋全＋地　　　　（分点ありの場合）
           03  SYS-2010-KAK31KBN-K          PIC  X(01).
           03  SYS-2010-KAK31-TAIKBN-K      PIC  X(01).
      *　主＋全＋地＋長　　（分点ありの場合）
           03  SYS-2010-KAK32KBN-K          PIC  X(01).
           03  SYS-2010-KAK32-TAIKBN-K      PIC  X(01).
      *保険欄の括弧再掲（下）金額加算（高齢者）
      *　主＋全＋地（＋長）（分点ありの場合）
           03  SYS-2010-KAK41KBN-K          PIC  X(01).
           03  SYS-2010-KAK41-TAIKBN-K      PIC  X(01).
      *　主＋全＋地　　　　（分点なしの場合）
           03  SYS-2010-KAK42KBN-K          PIC  X(01).
           03  SYS-2010-KAK42-TAIKBN-K      PIC  X(01).
      *　主＋全＋地＋長　　（分点なしの場合）
           03  SYS-2010-KAK43KBN-K          PIC  X(01).
           03  SYS-2010-KAK43-TAIKBN-K      PIC  X(01).
      *
      *保険欄の括弧再掲記載（７０歳未満）
           03  SYS-2010-KAKKBN-Y            PIC  X(01).
           03  SYS-2010-KAK-TAIKBN-Y        PIC  X(01).
      *保険欄の括弧再掲（下）記載（７０歳未満）
      *　主＋全＋地（＋長）（分点ありの場合）
           03  SYS-2010-KAK21KBN-Y          PIC  X(01).
           03  SYS-2010-KAK21-TAIKBN-Y      PIC  X(01).
      *　主＋全＋地　　　　（分点なしの場合）
           03  SYS-2010-KAK22KBN-Y          PIC  X(01).
           03  SYS-2010-KAK22-TAIKBN-Y      PIC  X(01).
      *　主＋全＋地＋長　　（分点なしの場合）
           03  SYS-2010-KAK23KBN-Y          PIC  X(01).
           03  SYS-2010-KAK23-TAIKBN-Y      PIC  X(01).
      *保険欄の括弧再掲（下）金額計算（７０歳未満）
      *　主＋全＋地　　　　（分点ありの場合）
           03  SYS-2010-KAK31KBN-Y          PIC  X(01).
           03  SYS-2010-KAK31-TAIKBN-Y      PIC  X(01).
      *　主＋全＋地＋長　　（分点ありの場合）
           03  SYS-2010-KAK32KBN-Y          PIC  X(01).
           03  SYS-2010-KAK32-TAIKBN-Y      PIC  X(01).
      *保険欄の括弧再掲（下）金額加算（７０歳未満）
      *　主＋全＋地（＋長）（分点ありの場合）
           03  SYS-2010-KAK41KBN-Y          PIC  X(01).
           03  SYS-2010-KAK41-TAIKBN-Y      PIC  X(01).
      *　主＋全＋地　　　　（分点なしの場合）
           03  SYS-2010-KAK42KBN-Y          PIC  X(01).
           03  SYS-2010-KAK42-TAIKBN-Y      PIC  X(01).
      *　主＋全＋地＋長　　（分点なしの場合）
           03  SYS-2010-KAK43KBN-Y          PIC  X(01).
           03  SYS-2010-KAK43-TAIKBN-Y      PIC  X(01).
      *
      *保険欄−一部負担金無記載
      *　（高齢者・外）
           03  SYS-2010-KSINGKBN-K-G        PIC  X(01).
           03  SYS-2010-KSING-TAIKBN-K-G    PIC  X(01).
      *　（高齢者・入）
           03  SYS-2010-KSINGKBN-K-N        PIC  X(01).
           03  SYS-2010-KSING-TAIKBN-K-N    PIC  X(01).
      *　（７０未・外）
           03  SYS-2010-KSINGKBN-Y-G        PIC  X(01).
           03  SYS-2010-KSING-TAIKBN-Y-G    PIC  X(01).
      *　（７０未・入）
           03  SYS-2010-KSINGKBN-Y-N        PIC  X(01).
           03  SYS-2010-KSING-TAIKBN-Y-N    PIC  X(01).
      *一部負担金１円→１０円単位記載
      *　（高齢者・外）
           03  SYS-2010-ENCHGKBN-K-G        PIC  X(01).
           03  SYS-2010-ENCHG-TAIKBN-K-G    PIC  X(01).
      *　（高齢者・入）
           03  SYS-2010-ENCHGKBN-K-N        PIC  X(01).
           03  SYS-2010-ENCHG-TAIKBN-K-N    PIC  X(01).
      *　（７０未・外）
           03  SYS-2010-ENCHGKBN-Y-G        PIC  X(01).
           03  SYS-2010-ENCHG-TAIKBN-Y-G    PIC  X(01).
      *　（７０未・入）
           03  SYS-2010-ENCHGKBN-Y-N        PIC  X(01).
           03  SYS-2010-ENCHG-TAIKBN-Y-N    PIC  X(01).
      *入院時食事療養記載（公費２欄置き換え）
           03  SYS-2010-KOH2SKJKBN          PIC  X(01).
           03  SYS-2010-KOH2SKJ-TAIKBN      PIC  X(01).
      *
      *負担金未発生分の回数カウント（外来）
           03  SYS-2010-KAICNTKBN-G         PIC  X(01).
           03  SYS-2010-KAICNT-TAIKBN-G     PIC  X(01).
      *現物給付対象者の特別計算（高齢者・外）
           03  SYS-2010-GNBKEIKBN-K-G       PIC  X(01).
           03  SYS-2010-GNBKEI-TAIKBN-K-G   PIC  X(01).
      *全国公費併用時の特別計算（外来）
           03  SYS-2010-ZENKEIKBN-G         PIC  X(01).
           03  SYS-2010-ZENKEI-TAIKBN-G     PIC  X(01).
      *診察料がない場合の負担金計算（外来）
           03  SYS-2010-SINKEIKBN-G         PIC  X(01).
           03  SYS-2010-SINKEI-TAIKBN-G     PIC  X(01).
      *初診時以外の負担金計算（外来）
           03  SYS-2010-SYOKEIKBN-G         PIC  X(01).
           03  SYS-2010-SYOKEI-TAIKBN-G     PIC  X(01).
      *初診時以外の負担金計算（入院）
           03  SYS-2010-SYOKEIKBN-N         PIC  X(01).
           03  SYS-2010-SYOKEI-TAIKBN-N     PIC  X(01).
      *１入院＠日限度とする特別計算（入院）
           03  SYS-2010-NYUKEIKBN-N         PIC  X(01).
           03  SYS-2010-NYUKEI-TAIKBN-N     PIC  X(01).
      *日上限額を固定額とする特別計算（入院）
           03  SYS-2010-DAYKEIKBN-N         PIC  X(01).
           03  SYS-2010-DAYKEI-TAIKBN-N     PIC  X(01).
      *限度額認定証提示が無い場合の特別計算
           03  SYS-2010-NINNASIKBN          PIC  X(01).
           03  SYS-2010-NINNASI-TAIKBN      PIC  X(01).
      *限度額認定証（上位）の場合の特別計算
           03  SYS-2010-NINJYOKBN           PIC  X(01).
           03  SYS-2010-NINJYO-TAIKBN       PIC  X(01).
      *長期併用時の特別計算（外来）
           03  SYS-2010-CYOKEIKBN-G         PIC  X(01).
           03  SYS-2010-CYOKEI-TAIKBN-G     PIC  X(01).
      *７０歳未満現物給付高額療養費合算計算
           03  SYS-2010-KOGAKKEIKBN         PIC  X(01).
           03  SYS-2010-KOGAKKEI-TAIKBN     PIC  X(01).
      *
      *設定額を助成する負担金計算
           03  SYS-2010-KINJYOKBN-G         PIC  X(01).
           03  SYS-2010-KINJYO-TAIKBN-G     PIC  X(01).
           03  SYS-2010-KINJYOKBN-N         PIC  X(01).
           03  SYS-2010-KINJYO-TAIKBN-N     PIC  X(01).
      *月−院内−上限額参照条件
           03  SYS-2010-INNAIKBN            PIC  X(01).
           03  SYS-2010-INNAI-TAIKBN        PIC  X(01).
      *
      *短期滞在手術等基本料入院時の日別の負担金計算
           03  SYS-2010-TANKIKBN            PIC  X(01).
           03  SYS-2010-TANKI-TAIKBN        PIC  X(01).
      *
      *負担金未発生日の回数カウント（入院）
           03  SYS-2010-KAICNTKBN-N         PIC  X(01).
           03  SYS-2010-KAICNT-TAIKBN-N     PIC  X(01).
      *長期併用時の特別計算（入院）
           03  SYS-2010-CYOKEIKBN-N         PIC  X(01).
           03  SYS-2010-CYOKEI-TAIKBN-N     PIC  X(01).
      *全国公費併用時の日の負担金計算（入院）
           03  SYS-2010-ZENDAYKBN-N         PIC  X(01).
           03  SYS-2010-ZENDAY-TAIKBN-N     PIC  X(01).
      *
      *自立支援医療の特別計算
           03  SYS-2010-JIRITUKBN           PIC  X(01).
           03  SYS-2010-JIRITU-TAIKBN       PIC  X(01).
      *
      *全国公費併用時の特別計算２（外来）
           03  SYS-2010-ZENKEI2KBN-G        PIC  X(01).
           03  SYS-2010-ZENKEI2-TAIKBN-G    PIC  X(01).
      *
      *全国公費併用時の日の負担金計算２（入院）
           03  SYS-2010-ZENDAY2KBN-N        PIC  X(01).
           03  SYS-2010-ZENDAY2-TAIKBN-N    PIC  X(01).
      *
      *初診時の負担金計算（外来）
           03  SYS-2010-SYOKEI2KBN-G        PIC  X(01).
           03  SYS-2010-SYOKEI2-TAIKBN-G    PIC  X(01).
      *初診時の負担金計算（入院）
           03  SYS-2010-SYOKEI2KBN-N        PIC  X(01).
           03  SYS-2010-SYOKEI2-TAIKBN-N    PIC  X(01).
      *
      *限度額認定証提示が無い場合の特別計算（高齢者）
           03  SYS-2010-NINNASIKBN-K        PIC  X(01).
           03  SYS-2010-NINNASI-TAIKBN-K    PIC  X(01).
      *
      *一部負担金記載１円未満端数処理（外来）
           03  SYS-2010-FTNHASUKBN-G        PIC  X(01).
           03  SYS-2010-FTNHASU-TAIKBN-G    PIC  X(01).
      *一部負担金記載１円未満端数処理（入院）
           03  SYS-2010-FTNHASUKBN-N        PIC  X(01).
           03  SYS-2010-FTNHASU-TAIKBN-N    PIC  X(01).
      *
      *マル長負担金計算処理区分
           03  SYS-2010-CYOKI-FTNKBN        PIC  X(01).
           03  SYS-2010-CYOKI-FTNKBN-TAIKBN PIC  X(01).
      *マル長レセプト記載処理区分
           03  SYS-2010-CYOKI-KSIKBN        PIC  X(01).
           03  SYS-2010-CYOKI-KSIKBN-TAIKBN PIC  X(01).
      *
      *予備
           03  SYS-2010-YOBI                PIC  X(318).
      *端末ＩＤ
           03  SYS-2010-TERMID              PIC  X(16).
      *オペレータＩＤ
           03  SYS-2010-OPID                PIC  X(16).
      *作成年月日
           03  SYS-2010-CREYMD              PIC  X(08).
      *更新年月日
           03  SYS-2010-UPYMD               PIC  X(08).
      *更新時間
           03  SYS-2010-UPHMS               PIC  X(06).
      *主キー更新用項目
           03  SYS-2010-UPD-KEY.
               05  SYS-2010-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2010-UPD-KANRICD     PIC X(04).
               05  SYS-2010-UPD-KBNCD       PIC X(08).
               05  SYS-2010-UPD-STYUKYMD    PIC X(08).
               05  SYS-2010-UPD-EDYUKYMD    PIC X(08).
