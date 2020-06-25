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
      *         システム管理マスタ     (CPSK4001.INC)            *
      *         （４００１）                                     *
      *         労災医療機関情報                                 *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02.09.17  加算率追加
      *  01.00.02    NACL-藤原    05.01.25  固定帯加算等取扱追加
      *  02.06.01    NACL-藤原    05/06/22  自賠責加算率（３桁）の追加
      *  02.06.02    NACL-門脇    05/12/05  労災レセ回数記載の追加
      ************************************************************
       01  SYS-4001-REC.
           03  SYS-4001-KEY.
               05  SYS-4001-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-4001-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-4001-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-4001-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-4001-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-4001-TBL.
      *労災指定医療機関区分
               05  SYS-4001-RSIIRYOKBN  PIC  X(01).
      *指定医療機関コード
               05  SYS-4001-RSIIRYOCDN  PIC  X(80).
      *点数単価区分
               05  SYS-4001-TENKBN      PIC  X(01).
      *自賠責加算率
               05  SYS-4001-KSNRATE     PIC  9(02).
      *自賠責文書料消費税
               05  SYS-4001-TAXKBN      PIC  X(01).
      *自賠責食事療養費
               05  SYS-4001-SKJKBN      PIC  X(01).
      *自賠責特定器材等消費税
               05  SYS-4001-TAXKBN-KZI  PIC  X(01).
      *自賠責固定帯加算等取扱
               05  SYS-4001-KOTEIKBN    PIC  X(01).
      *労災／自賠請求書記載区分
               05  SYS-4001-RSIPRTKBN   PIC  X(01).
      *自賠責加算率（３桁）
               05  SYS-4001-KSNRATE1    PIC  9(03).
      *労災レセ回数記載
               05  SYS-4001-ROUKAISU    PIC  X(01).
      *自賠責区分
               05  SYS-4001-JIBAIKBN    PIC  X(01).
      *自賠責（健保準拠）  技術点単価    ＜外来＞
               05  SYS-4001-SYUTENTANKA-G.
                   07   SYS-4001-SYUTENTANKA-X PIC  X(02).
                   07   SYS-4001-SYUTENTANKA   REDEFINES
                        SYS-4001-SYUTENTANKA-X PIC  9(02).
      *自賠責（健保準拠）  薬剤等点単価  ＜外来＞
               05  SYS-4001-YKZTENTANKA-G.
                   07   SYS-4001-YKZTENTANKA-X PIC  X(02).
                   07   SYS-4001-YKZTENTANKA   REDEFINES
                        SYS-4001-YKZTENTANKA-X PIC  9(02).
      *公務災害区分
               05  SYS-4001-KOMUKBN     PIC  X(01).
      *公務災害単価
               05  SYS-4001-KOMUTENTANKA-G.
                   07   SYS-4001-KOMUTENTANKA-X PIC  X(02).
                   07   SYS-4001-KOMUTENTANKA   REDEFINES
                        SYS-4001-KOMUTENTANKA-X PIC  9(02).
      *自賠責請求区分
               05  SYS-4001-JIBAI-SEIKYUKBN   PIC  X(01).
      *公務災害請求区分
               05  SYS-4001-KOMU-SEIKYUKBN   PIC  X(01).
      *休業証明記載区分
               05  SYS-4001-KYUGYO-PRTKBN   PIC  X(01).
      *公務災害労災準拠点数単価区分
               05  SYS-4001-KOMURSITANKAKBN PIC  X(01).
      *公務災害労災準拠点数単価
               05  SYS-4001-KOMURSITANKA-G.
                   07  SYS-4001-KOMURSITANKA    PIC  99V99.
      *自賠責（健保準拠）  技術点単価    ＜入院＞
               05  SYS-4001-SYUTENTANKA2-G.
                   07   SYS-4001-SYUTENTANKA2-X PIC  X(02).
                   07   SYS-4001-SYUTENTANKA2   REDEFINES
                        SYS-4001-SYUTENTANKA2-X PIC  9(02).
      *自賠責（健保準拠）  薬剤等点単価  ＜入院＞
               05  SYS-4001-YKZTENTANKA2-G.
                   07   SYS-4001-YKZTENTANKA2-X PIC  X(02).
                   07   SYS-4001-YKZTENTANKA2   REDEFINES
                        SYS-4001-YKZTENTANKA2-X PIC  9(02).
      *第三者行為現物給付区分初期設定
               05  SYS-4001-DAISAN-GENBTKBN     PIC  X(01).
      *予備
           03  SYS-4001-YOBI            PIC  X(386).
      *端末ＩＤ
           03  SYS-4001-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-4001-OPID            PIC  X(16).
      *作成年月日
           03  SYS-4001-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-4001-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-4001-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-4001-UPD-KEY.
               05  SYS-4001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-4001-UPD-KANRICD     PIC X(04).
               05  SYS-4001-UPD-KBNCD       PIC X(08).
               05  SYS-4001-UPD-STYUKYMD    PIC X(08).
               05  SYS-4001-UPD-EDYUKYMD    PIC X(08).
