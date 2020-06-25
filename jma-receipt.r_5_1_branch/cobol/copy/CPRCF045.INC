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
      *    レセプト明細書     COPY (CPRCF045.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03.01.22  データ区分の追加
      *  01.00.02    NACL-門脇    03.03.03  剤番号桁修正
      *  01.00.03    NACL-門脇    03.09.29  包括対象検査情報の追加
      *                                     検査等実施判断情報の追加
      *                                     各種番号等情報の追加
      *  01.00.04    NACL-門脇    04.06.07  繰り返しを集団項目へ変更
      *
      *  02.06.01    NACL-藤原    05/08/22  検体検査コメントの追加
      *
      *  02.09.01    NACL-藤原    06/04/13  きざみ値計算識別
      *                                     検査等実施判断グループの追加
      *  03.05.01    NACL-門脇    07/05/28  グループ診療対応
      *  04.01.01    NACL-門脇    07/11/26  特定器材商品名記載対応
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *
      *  04.07.01    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.02    NACL-門脇    14/09/11  点数マスタの診療区分追加
      *
      *  04.08.01    NACL-門脇    16/01/12  点検用レセプト院外処方
      *                                     一般名記載対応
      *  04.08.02    NACL-門脇    16/04/20  平成２８年４月改正対応
      *
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE45-REC.
           03  RECE45-KEY.
               05  RECE45-KEY1.
      *医療機関ＩＤ
                   07  RECE45-HOSPNUM     PIC  9(02).
      *診療年月
                   07  RECE45-SRYYM       PIC  9(06).
      *入外区分
                   07  RECE45-NYUGAIKBN   PIC  X(01).
      *患者ＩＤ
                   07  RECE45-PTID        PIC  9(10).
      *レセプト科
                   07  RECE45-RECEKA      PIC  X(02).
      *保険ＩＤ        
                   07  RECE45-HKNID       PIC  9(10).
      *適用開始年月日
                   07  RECE45-TEKSTYMD    PIC  X(08). 
      *剤番号
               05  RECE45-ZAINUM          PIC  9(08).
      *連番号
               05  RECE45-RENNUM          PIC  9(02).
      *患者番号
           03  RECE45-PTNUM               PIC  X(20).
      *診療科
           03  RECE45-SRYKA               PIC  X(02).
      *診療区分
           03  RECE45-SRYKBN              PIC  X(02).
      *診療種別区分
           03  RECE45-SRYSYUKBN           PIC  X(03).
      *自費金額
           03  RECE45-JIHIMONEY           PIC  9(07).
      *
           03  RECE45-SRYACT-TBL          OCCURS  5.
      *診療行為情報
             05  RECE45-SRYACTINF.
      *診療コード
                 07  RECE45-SRYCD         PIC  X(09).
      *診療種別区分
                 07  RECE45-SRYSYUKBN-X   PIC  X(03).
      *数量
                 07  RECE45-SRYSURYO      PIC  9(05)V9(05).
      *回数
                 07  RECE45-SRYKAISU      PIC  9(03).
      *単位コード
                 07  RECE45-TANICD        PIC  9(03).
      *単位名称有効桁数
                 07  RECE45-TANIMOJISU    PIC  9(01).
      *単位名称
                 07  RECE45-TANINAME      PIC  X(24).
      *点数識別区分
                 07  RECE45-TENSIKIBETU   PIC  9(01).
      *点数
                 07  RECE45-TEN           PIC  9(09)V9(02).
      *明細請求フラグ
                 07  RECE45-MEISKYFLG     PIC  X(01).
      *自動算定区分
                 07  RECE45-AUTOKBN       PIC  X(01).
      *名称入力番号
                 07  RECE45-INPUTNUM      PIC  9(03).
      *点数欄集計先識別
                 07  RECE45-TENTTLKBN     PIC  9(03).
      *データ区分
                 07  RECE45-DATAKBN       PIC  9(01).
      *包括対象検査
                 07  RECE45-HOUKNSKBN     PIC  9(02).
      *検査等実施判断
                 07  RECE45-KNSJISKBN     PIC  9(01).
      *検査等実施判断グループ
                 07  RECE45-KNSJISGRPKBN  PIC  9(02).
      *各種番号等
                 07  RECE45-CDKBNINF.
                     09  RECE45-CDKBN-KBN        PIC  X(01).
                     09  RECE45-CDKBN-SYO        PIC  9(01).
                     09  RECE45-CDKBN-BU         PIC  9(02).
                     09  RECE45-CDKBN-KBNNUM     PIC  9(03).
                     09  RECE45-CDKBN-KBNNUM-EDA PIC  9(02).
                     09  RECE45-CDKBN-KOUBAN     PIC  9(02).
                     09  RECE45-KOUHYOJYUNNUM    PIC  9(09).
      *告示等識別区分１
                 07  RECE45-KOKUJISKBKBN1 PIC  9(01).
      *告示等識別区分２
                 07  RECE45-KOKUJISKBKBN2 PIC  9(01).
      *通則加算対象外区分
                 07  RECE45-TUSOKUGAIKBN  PIC  9(01).
      *時間外加算区分
                 07  RECE45-TIMEKSNKBN    PIC  9(01).
      *きざみ値計算識別
                 07  RECE45-KZMCOMPSIKIBETU
                                          PIC  9(01).
      *注加算コード
                 07  RECE45-CHUKSNINF.
      *コード
                     09  RECE45-CHUKSNCD         PIC  X(04).
      *通番
                     09  RECE45-CHUKSNTUBAN      PIC  X(01).
      *検体検査コメント
                 07  RECE45-KENTAICOMMENT        PIC  9(01).
      *入力コメント
                 07  RECE45-INPUTCOMENT          PIC  X(140).
      *名称情報
             05  RECE45-NAMEINF.
      *漢字有効桁数
                 07  RECE45-YUKOKETA      PIC  9(03).
      *漢字名称
                 07  RECE45-NAME          PIC  X(300).
      *識別フラグ情報
             05  RECE45-SIKFLGINF.
      *識別フラグ
                 07  RECE45-SIKFLG        PIC  X(01).
      *特定器材種別１
             05  RECE45-TOKUKIZAISBT1     PIC  9(01).
      *酸素等区分
             05  RECE45-SANSOKBN          PIC  9(01).
      *レセプト編集情報
             05  RECE45-SSTKIJUNCDTBL     OCCURS 10.
                 07  RECE45-SSTKIJUNCD    PIC  9(03).
      *その他情報
             05  RECE45-SONOTAINF.
      *レセプト電算対象外フラグ
                 07  RECE45-RECEDENFLG       PIC  X(01).
      *レセプト対象外フラグ
                 07  RECE45-RECEPTFLG        PIC  X(01).
      *レセ電用特定器材商品名
                 07  RECE45-RECEDENNAME      PIC  X(300).
      *レセ用特定器材商品名編集フラグ
                 07  RECE45-RECEHENFLG       PIC  X(01).
      *関係コメント関連設定フラグ
                 07  RECE45-SETCOMATFLG      PIC  9(03).
      *点数マスタの診療区分
                 07  RECE45-TNSSRYKBN        PIC  X(02).
      *
      *医療情報連携用情報
             05  RECE45-IRYORENKEIINF.
      *一般名記載院外処方フラグ
                 07  RECE45-INGAIFLG         PIC  X(01).
      *一般名記載院外処方フラグ（レセプト用）
                 07  RECE45-INGAIFLG-RECE    PIC  X(01).
      *一般名記載院外処方フラグ（湿布薬コメント用）
                 07  RECE45-INGAIFLG-SIPPU   PIC  X(01).
      *一般名コード（薬価基準コード）
                 07  RECE45-YAKKAKJNCD       PIC  X(12).
      *一般名処方加算区分
                 07  RECE45-GENERIC-KBN      PIC  X(01).
      *一般名称
                 07  RECE45-GENERICNAME      PIC  X(200).
      *漢字有効桁数
                 07  RECE45-GENERICYUKOKETA  PIC  9(03).
      *
      *湿布薬情報
             05  RECE45-SIPPUINF.
                 07  RECE45-SIPPUFLG         PIC  X(01).
      *
