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
      *    請求管理(新）       COPY (CPSKYMAIN.INC)              *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *
      *  04.07.01    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *                                     直接請求フラグの追加
      *
      *  04.08.01    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *                                     保険区分（自賠責用）の追加
      ************************************************************
       01  SKYMAIN-REC.
      *ＫＥＹ
           03  SKYMAIN-KEY.
      *医療機関ＩＤ
               05  SKYMAIN-HOSPNUM          PIC 9(2).
      *診療年月
               05  SKYMAIN-SRYYM            PIC  9(06).
      *入外区分
               05  SKYMAIN-NYUGAIKBN        PIC  X(01).
      *患者ＩＤ
               05  SKYMAIN-PTID             PIC  9(10).
      *レセプト科（空白の時ゼロをセット）
               05  SKYMAIN-RECEKA           PIC  X(02).
      *審査支払機関
               05  SKYMAIN-TEISYUTUSAKI     PIC  9(01).
      *レセプト種別
               05  SKYMAIN-RECESYUBETU      PIC  X(04).
      *保険者番号
               05  SKYMAIN-HKNJANUM         PIC  X(08).
      *補助区分（船員保険のときのみ）
               05  SKYMAIN-HOJOKBN-KEY      PIC  X(01).
      *公費ＩＤ（公費単独のみ）
               05  SKYMAIN-KOHID-KEY        PIC  9(10).
      *適用開始年月日
               05  SKYMAIN-TEKSTYMD         PIC  X(08).
      *分割区分
               05  SKYMAIN-RECEIPT-KBN      PIC  X(10).
      *患者番号
           03  SKYMAIN-PTNUM                PIC  X(20).
      *県内・県外区分（１：県内　２：県外　）
           03  SKYMAIN-PREFKBN              PIC  X(01).
      *県番号
           03  SKYMAIN-PREFNUM              PIC  X(02).
      *年齢
           03  SKYMAIN-AGE                  PIC  9(03).
      *給付割合
           03  SKYMAIN-KYURATE              PIC  9(03).
      *在総診区分
           03  SKYMAIN-ZAITAKU              PIC  X(01).
      *請求年月
           03  SKYMAIN-SKYYM                PIC  X(06).
      *返戻年月
           03  SKYMAIN-HENREIYM             PIC  X(06).
      *請求区分
           03  SKYMAIN-SKYKBN               PIC  X(01).
      *請求区分（レセ電対象）
           03  SKYMAIN-SKYKBN-RECEDEN       PIC  X(01).
      *訂正区分
           03  SKYMAIN-TEISEIKBN            PIC  X(01).
      *主保険−保険番号
           03  SKYMAIN-HKNNUM               PIC  X(03).
      *主保険−保険ＩＤ
           03  SKYMAIN-HKNID                PIC  9(10).
      *主保険−本人家族区分
           03  SKYMAIN-HONKZKKBN            PIC  X(01).
      *主保険−補助区分
           03  SKYMAIN-HOJOKBN              PIC  X(01).
      *主保険−継続区分
           03  SKYMAIN-CONTKBN              PIC  X(01).
      *主保険−記号
           03  SKYMAIN-KIGO                 PIC  X(80).
      *主保険−番号
           03  SKYMAIN-NUM                  PIC  X(80).
      *老人保険−保険番号
           03  SKYMAIN-RJNHKNNUM            PIC  X(03).
      *老人保険−保険ＩＤ
           03  SKYMAIN-RJNHKNID             PIC  9(10).
      *公費情報
           03  SKYMAIN-KOHINF               OCCURS 4.
      *公費−保険番号
               05  SKYMAIN-KOHNUM           PIC  X(03).
      *実日数情報
           03  SKYMAIN-JNISSUINF.
      *実日数
               05  SKYMAIN-JNISSU           PIC  9(02).
      *診療区分別点数情報
           03  SKYMAIN-SRYKBNINF           OCCURS  45.
      *基本点数
               05  SKYMAIN-KHNTEN          PIC  9(08).
      *回数
               05  SKYMAIN-KAISU           PIC  9(05).
      *明細点数情報
               05  SKYMAIN-TENSUINF.
      *明細点数
                   07  SKYMAIN-TENSU       PIC  S9(08).
      *合計点数情報
           03  SKYMAIN-TOTALTENINF.
      *合計点数
               05  SKYMAIN-TOTALTEN        PIC S9(08).
      *負担金額情報
           03  SKYMAIN-FTNMONEYINF.
      *負担金額
               05  SKYMAIN-FTNMONEY        PIC S9(09).
      *負担金額（一部負担額）
               05  SKYMAIN-FTNMONEY-FTN    PIC S9(09).
      *負担金額（控除額）
               05  SKYMAIN-FTNMONEY-KOJ    PIC S9(09).
      *
      *食事療養費情報
           03  SKYMAIN-SHOKUJIINF.
      *食事日数
               05  SKYMAIN-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  SKYMAIN-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  SKYMAIN-SHOKUJIFTN      PIC  9(09).
      *
      *一部負担金・食事療養費標準負担額区分
           03  SKYMAIN-SHOKUJIFTNKBN       PIC  X(01).
      *
      *薬剤一部負担金情報
           03  SKYMAIN-YKZFTNINF.
      *薬剤一部負担金
               05  SKYMAIN-YKZFTN          PIC  9(05).
      *
      *請求額情報（医療費）
           03  SKYMAIN-SKYIRYO-INF.
      *実日数
               05  SKYMAIN-SKYIRYO-NISSU   PIC 9(02).
      *請求点
               05  SKYMAIN-SKYIRYO-TOTALTEN
                                           PIC S9(08).
      *患者負担
               05  SKYMAIN-SKYIRYO-ICHIBUFTN
                                           PIC S9(09).
      *保険者負担
               05  SKYMAIN-SKYIRYO-HOKENJA PIC S9(09).
      *公費負担
               05  SKYMAIN-SKYIRYO-KOHFTN  PIC S9(09).
      *
      *請求額情報（食事）
           03  SKYMAIN-SKYSKJ-INF.
      *回数
               05  SKYMAIN-SKYSKJ-KAISU     PIC 9(02).
      *請求円
               05  SKYMAIN-SKYSKJ-RYOYOHI   PIC 9(09).
      *患者負担
               05  SKYMAIN-SKYSKJ-ICHIBUFTN PIC 9(09).
      *保険者負担
               05  SKYMAIN-SKYSKJ-HOKENJA   PIC 9(09).
      *公費負担
               05  SKYMAIN-SKYSKJ-KOHFTN    PIC 9(09).
      *保険組合せ情報
           03  SKYMAIN-HKNCOMBI-INF.
               05  SKYMAIN-HKNCOMBI         PIC 9(04)   OCCURS 10.
      *
      *特記事項
           03  SKYMAIN-TOKKIINF.
               05  SKYMAIN-TOKKI          PIC  X(02)   OCCURS  5.
      *
      *特記事項略称
           03  SKYMAIN-TOKKINMINF.
               05  SKYMAIN-TOKKINM        PIC  X(12)   OCCURS  5.
      *
      *フラグ
           03  SKYMAIN-FLGINF.
               05  SKYMAIN-FLG            PIC  9(01)   OCCURS  5.
      *
      *データエリア
           03  SKYMAIN-RESERVE-AREA       PIC  X(50).
      *
      *返戻指示フラグ
           03  SKYMAIN-HENREI-SIJI-FLG    PIC  9(01).
      *直接請求フラグ
           03  SKYMAIN-DIRECT-FLG         PIC X(01).
      *
      *レセプト記載外公費情報作成フラグ（1:記載外公費情報作成）
           03  SKYMAIN-ETCCREATEFLG        PIC  9(01).
      *保険区分（５：自賠責（労災準拠）、６：自賠責（健保準拠））
           03  SKYMAIN-HKNKBN-RSI          PIC X(01).
      *(総括表用)
      *主保険−保険番号
           03  SKYMAIN-HKNNUM-S            PIC  X(03).
      *保険者番号 
           03  SKYMAIN-HKNJANUM-S          PIC  X(08).
      *登録年月日
           03  SKYMAIN-CREYMD              PIC  X(08).
      *登録時間
           03  SKYMAIN-CREHMS              PIC  X(06).
      *更新年月日
           03  SKYMAIN-UPDYMD              PIC  X(08).
      *更新時間
           03  SKYMAIN-UPDHMS              PIC  X(06).
      *主キー更新用項目
           03  SKYMAIN-UPHKNJANUM          PIC X(10).
      *保険者番号検索用項目
           03  SKYMAIN-XXPREFNUM           PIC X(02).
      *診療年月検索用項目
           03  SKYMAIN-XXSRYYM             PIC 9(06).
      *対象保険者番号検索用項目 
           03  SKYMAIN-XXSTHKNJANUM        PIC X(08).
           03  SKYMAIN-XXEDHKNJANUM        PIC X(08).
      *審査支払機関検索用項目
           03  SKYMAIN-XXTEISYUTUSAKI      PIC 9(01).


