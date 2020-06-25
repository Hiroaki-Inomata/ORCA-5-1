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
      *    請求管理           COPY (CPRCF010.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/08/08  総括用項目の追加
      *                                     更新用項目の追加 
      *  01.00.02    MCC-藤原     02/03/15  検索用項目の追加
      *
      *  01.01.01    NACL-藤原    02/09/17  県内県外区分・県番号・
      *                                     年齢・給付割合の追加
      *  01.01.02    NACL-藤原    02/10/07  在総診区分の追加
      *  01.01.03    NACL-藤原    03/03/06  補助区分の追加
      *                                     点数のタイプ変更
      *                                     レセプト記載外公費情報追加
      *  01.01.04    NACL-藤原    03/04/11  診療年月検索用項目の追加
      *  01.01.05    NACL-藤原    03/05/19  対象保険者番号の追加
      *  01.01.06    NACL-門脇    05/03/11  負担金額情報に項目追加
      *                                     一部負担金・食事療養費標準
      *                                     負担額区分追加
      *                                     記載外公費−負担金額に項目
      *                                     追加
      *
      *  03.04.01    NACL-藤原    07/02/27  保険ＩＤ、老人保険ＩＤ、
      *                                     公費ＩＤの追加
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/07/18  特記事項、
      *                                     請求区分（レセ電対象）を追加
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *                                     審査支払機関検索用項目の追加
      *
      *  04.02.01    NACL-藤原    08/03/12  フラグ、データエリア追加
      *
      *  04.04.01    NACL-藤原    08/12/17  返戻指示フラグ追加
      *
      *  04.05.01    NACL-藤原    09/11/13  特記事項照会対応
      *                                     特記事項略称追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *                                     調査用にcrehms,updhms 追加
      *  04.06.02    NACL-藤原    10/11/30  統計帳票用の項目追加
      *                                     請求額情報（医療費、食事）
      *                                     保険組合せ情報
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      *
      *  04.07.01    NACL-藤原    11/09/02  労災請求書作成対応
      *  04.07.02    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *                                     直接請求フラグの追加
      *  04.07.03    NACL-藤原    13/09/20  第三者行為対応
      *
      *  04.08.01    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *                                     保険区分（自賠責用）の追加
      ************************************************************
       01  RECE10-REC.
      *ＫＥＹ
           03  RECE10-KEY.
      *医療機関ＩＤ
               05  RECE10-HOSPNUM          PIC 9(2).
      *診療年月
               05  RECE10-SRYYM            PIC  9(06).
      *入外区分
               05  RECE10-NYUGAIKBN        PIC  X(01).
      *患者ＩＤ
               05  RECE10-PTID             PIC  9(10).
      *レセプト科（空白の時ゼロをセット）
               05  RECE10-RECEKA           PIC  X(02).
      *審査支払機関
               05  RECE10-TEISYUTUSAKI     PIC  9(01).
      *レセプト種別
               05  RECE10-RECESYUBETU      PIC  X(04).
      *保険者番号
               05  RECE10-HKNJANUM         PIC  X(08).
      *補助区分（船員保険のときのみ）
               05  RECE10-HOJOKBN-KEY      PIC  X(01).
      *公費ＩＤ（医保：公費単独のみ、労災、自賠責：保険ＩＤ）
               05  RECE10-KOHID-KEY        PIC  9(10).
      *適用開始年月日
               05  RECE10-TEKSTYMD         PIC  X(08).
      *分割区分
               05  RECE10-RECEIPT-KBN      PIC  X(10).
      *患者番号
           03  RECE10-PTNUM                PIC  X(20).
      *県内・県外区分（１：県内　２：県外　）
           03  RECE10-PREFKBN              PIC  X(01).
      *県番号
           03  RECE10-PREFNUM              PIC  X(02).
      *年齢
           03  RECE10-AGE                  PIC  9(03).
      *給付割合
           03  RECE10-KYURATE              PIC  9(03).
      *在総診区分
           03  RECE10-ZAITAKU              PIC  X(01).
      *請求年月
           03  RECE10-SKYYM                PIC  X(06).
      *返戻年月
           03  RECE10-HENREIYM             PIC  X(06).
      *請求区分
           03  RECE10-SKYKBN               PIC  X(01).
      *請求区分（レセ電対象）
           03  RECE10-SKYKBN-RECEDEN       PIC  X(01).
      *訂正区分
           03  RECE10-TEISEIKBN            PIC  X(01).
      *主保険−保険番号
           03  RECE10-HKNNUM               PIC  X(03).
      *主保険−保険ＩＤ
           03  RECE10-HKNID                PIC  9(10).
      *主保険−本人家族区分
           03  RECE10-HONKZKKBN            PIC  X(01).
      *主保険−補助区分
           03  RECE10-HOJOKBN              PIC  X(01).
      *主保険−継続区分
           03  RECE10-CONTKBN              PIC  X(01).
      *主保険−記号
           03  RECE10-KIGO                 PIC  X(80).
      *主保険−番号
           03  RECE10-NUM                  PIC  X(80).
      *老人保険−保険番号
           03  RECE10-RJNHKNNUM            PIC  X(03).
      *老人保険−保険ＩＤ
           03  RECE10-RJNHKNID             PIC  9(10).
      *公費情報
           03  RECE10-KOHINF               OCCURS  4.
      *公費−保険番号
               05  RECE10-KOHNUM           PIC  X(03).
      *公費情報
           03  RECE10-KOHIDINF             OCCURS  4.
      *公費−公費ＩＤ
               05  RECE10-KOHID            PIC  9(10).
      *実日数情報
           03  RECE10-JNISSUINF            OCCURS  5.
      *実日数
               05  RECE10-JNISSU           PIC  9(02).
      *診療区分別点数情報
           03  RECE10-SRYKBNINF           OCCURS  45.
      *基本点数
               05  RECE10-KHNTEN          PIC  9(08).
      *回数
               05  RECE10-KAISU           PIC  9(05).
      *明細点数情報
               05  RECE10-TENSUINF        OCCURS  5.
      *明細点数
                   07  RECE10-TENSU       PIC  S9(08).
      *合計点数情報
           03  RECE10-TOTALTENINF         OCCURS  5.
      *合計点数
               05  RECE10-TOTALTEN        PIC S9(08).
      *負担金額情報
           03  RECE10-FTNMONEYINF         OCCURS  5.
      *負担金額
               05  RECE10-FTNMONEY        PIC S9(09).
      *負担金額（一部負担額）
               05  RECE10-FTNMONEY-FTN    PIC S9(09).
      *負担金額（控除額）
               05  RECE10-FTNMONEY-KOJ    PIC S9(09).
      *
      *食事療養費情報
           03  RECE10-SHOKUJIINF          OCCURS  5.
      *食事日数
               05  RECE10-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  RECE10-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  RECE10-SHOKUJIFTN      PIC  9(09).
      *
      *一部負担金・食事療養費標準負担額区分
           03  RECE10-SHOKUJIFTNKBN       PIC  X(01).
      *
      *薬剤一部負担金情報
           03  RECE10-YKZFTNINF           OCCURS  5.
      *薬剤一部負担金
               05  RECE10-YKZFTN          PIC  9(05).
      *レセプト記載外公費情報
           03  RECE10-ETC-KOH-G.
      *記載外公費−保険番号
               05  RECE10-ETC-KOHINF.
                   07  RECE10-ETC-KOHNUM   PIC  X(03)   OCCURS  4.
      *記載外公費−実日数
               05  RECE10-ETC-JNISSUINF.
                   07  RECE10-ETC-JNISSU   PIC  9(02)   OCCURS  4.
      *記載外公費−合計点数
               05  RECE10-ETC-TOTALTENINF.
                   07  RECE10-ETC-TOTALTEN PIC S9(08)   OCCURS  4.
      *記載外公費−負担金額
               05  RECE10-ETC-FTNMONEYINF               OCCURS  4.
                   07  RECE10-ETC-FTNMONEY     PIC S9(09).
                   07  RECE10-ETC-FTNMONEY-FTN PIC S9(09).
                   07  RECE10-ETC-FTNMONEY-KOJ PIC S9(09).
      *記載外公費−薬剤一部負担金情報
               05  RECE10-ETC-YKZFTNINF.
                   07  RECE10-ETC-YKZFTN   PIC  9(05)   OCCURS  4.
      *記載外公費−食事療養費情報
               05  RECE10-ETC-SHOKUJIINF       OCCURS  4.
      *食事日数
                   07  RECE10-ETC-SHOKUJINISSU PIC  9(02).
      *食事療養費
                   07  RECE10-ETC-SHOKUJIRYOYOHI
                                               PIC  9(09).
      *標準負担額
                   07  RECE10-ETC-SHOKUJIFTN   PIC  9(09).
      *
      *請求額情報（医療費）
           03  RECE10-SKYIRYO-INF.
      *実日数
               05  RECE10-SKYIRYO-NISSU    PIC 9(02).
      *請求点
               05  RECE10-SKYIRYO-TOTALTEN PIC S9(08).
      *患者負担
               05  RECE10-SKYIRYO-ICHIBUFTN
                                           PIC S9(09).
      *保険者負担
               05  RECE10-SKYIRYO-HOKENJA  PIC S9(09).
      *公費負担
               05  RECE10-SKYIRYO-KOHFTN   PIC S9(09).
      *
      *請求額情報（食事）
           03  RECE10-SKYSKJ-INF.
      *回数
               05  RECE10-SKYSKJ-KAISU     PIC 9(02).
      *請求円
               05  RECE10-SKYSKJ-RYOYOHI   PIC 9(09).
      *患者負担
               05  RECE10-SKYSKJ-ICHIBUFTN PIC 9(09).
      *保険者負担
               05  RECE10-SKYSKJ-HOKENJA   PIC 9(09).
      *公費負担
               05  RECE10-SKYSKJ-KOHFTN    PIC 9(09).
      *保険組合せ情報
           03  RECE10-HKNCOMBI-INF.
               05  RECE10-HKNCOMBI         PIC 9(04)   OCCURS 10.
      *特記事項
           03  RECE10-TOKKIINF.
               05  RECE10-TOKKI            PIC X(02)   OCCURS  5.
      *特記事項略称
           03  RECE10-TOKKINMINF.
               05  RECE10-TOKKINM          PIC X(12)   OCCURS  5.
      *労災情報
           03  RECE10-RSIINF.
      *保険区分（１：短期、２：傷病、３：アフターケア、４：公務災害
      *          ５：自賠責（労災準拠）、６：自賠責（健保準拠））
               05  RECE10-HKNKBN           PIC X(01).
      *様式区分（４：新様式、５：従来様式、６：第三者行為）
               05  RECE10-RECEKBN          PIC X(01).
      *労災初回請求分フラグ（１：初回請求分）
               05  RECE10-SYOKAIFLG        PIC X(01).
      *労働基準監督署コード
               05  RECE10-SYOCD            PIC X(05).
      *患者交付番号
               05  RECE10-KOFUNUM          PIC X(14).
      *災害区分（１：業務、２：通勤）
               05  RECE10-SAIGAIKBN        PIC X(01).
      *労災コメント
               05  RECE10-COMMENT          PIC X(200).
      *フラグ
           03  RECE10-FLGINF.
               05  RECE10-FLG              PIC 9(01)   OCCURS  5.
      *データエリア
           03  RECE10-RESERVE-AREA         PIC X(50).
      *返戻指示フラグ
           03  RECE10-HENREI-SIJI-FLG      PIC 9(01).
      *直接請求フラグ
           03  RECE10-DIRECT-FLG           PIC X(01).
      *保険区分（５：自賠責（労災準拠）、６：自賠責（健保準拠））
           03  RECE10-HKNKBN-RSI           PIC X(01).
      *(総括表用)
      *主保険−保険番号
           03  RECE10-HKNNUM-S             PIC X(03).
      *保険者番号 
           03  RECE10-HKNJANUM-S           PIC X(08).
      *登録年月日
           03  RECE10-CREYMD               PIC X(08).
      *登録時間
           03  RECE10-CREHMS               PIC X(06).
      *更新年月日
           03  RECE10-UPDYMD               PIC X(08).
      *更新時間
           03  RECE10-UPDHMS               PIC X(06).
      *
      *主キー更新用項目
           03  RECE10-UPHKNJANUM           PIC X(10).
      *保険者番号検索用項目
           03  RECE10-XXPREFNUM            PIC X(02).
      *診療年月検索用項目
           03  RECE10-XXSRYYM              PIC 9(06).
      *対象保険者番号検索用項目 
           03  RECE10-XXSTHKNJANUM         PIC X(08).
           03  RECE10-XXEDHKNJANUM         PIC X(08).
      *審査支払機関検索用項目
           03  RECE10-XXTEISYUTUSAKI       PIC 9(01).
