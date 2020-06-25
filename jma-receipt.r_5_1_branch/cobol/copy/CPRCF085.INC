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
      *    労災レセプト明細書（入院）     COPY (CPRCF085.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.06.07  繰り返しを集団項目へ変更
      *
      *  02.06.01    NACL-藤原    05/08/22  検体検査コメントの追加
      *
      *  02.09.01    NACL-藤原    06/04/13  きざみ値計算識別
      *                                     検査等実施判断グループの追加
      *  03.05.01    NACL-門脇    07.06.01  グループ診療対応
      *  04.01.01    NACL-門脇    07/11/26  特定器材商品名記載対応
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *
      *  04.07.01    NACL-門脇    13/01/21  労災レセ電対応
      *
      *  04.08.01    NACL-門脇    16/01/12  点検用レセプト院外処方
      *                                     一般名記載対応
      *
      *  05.00.01    NACL-門脇    20/03/30  令和２年４月改正対応
      ************************************************************
      *01  RECE85-REC.
           03  RECE85-KEY.
               05  RECE85-KEY1.
      *医療機関ＩＤ
                   07  RECE85-HOSPNUM     PIC  9(02).
      *診療年月
                   07  RECE85-SRYYM       PIC  9(06).
      *入外区分
                   07  RECE85-NYUGAIKBN   PIC  X(01).
      *患者ＩＤ
                   07  RECE85-PTID        PIC  9(10).
      *レセプト科
                   07  RECE85-RECEKA      PIC  X(02).
      *保険ＩＤ        
                   07  RECE85-HKNID       PIC  9(10).
      *適用開始年月日
                   07  RECE85-TEKSTYMD    PIC  X(08).
      *剤番号
               05  RECE85-ZAINUM          PIC  9(08).
      *連番号
               05  RECE85-RENNUM          PIC  9(02).
      *患者番号
           03  RECE85-PTNUM               PIC  X(20).
      *診療科
           03  RECE85-SRYKA               PIC  X(02).
      *診療区分
           03  RECE85-SRYKBN              PIC  X(02).
      *診療種別区分
           03  RECE85-SRYSYUKBN           PIC  X(03).
      *自費金額
           03  RECE85-JIHIMONEY           PIC  9(07).
      *
           03  RECE85-SRYACT-TBL          OCCURS  5.
      *診療行為情報
             05  RECE85-SRYACTINF.
      *診療コード
                 07  RECE85-SRYCD         PIC  X(09).
      *診療種別区分
                 07  RECE85-SRYSYUKBN-X   PIC  X(03).
      *数量
                 07  RECE85-SRYSURYO      PIC  9(05)V9(05).
      *回数
                 07  RECE85-SRYKAISU      PIC  9(03).
      *単位コード
                 07  RECE85-TANICD        PIC  9(03).
      *単位名称有効桁数
                 07  RECE85-TANIMOJISU    PIC  9(01).
      *単位名称
                 07  RECE85-TANINAME      PIC  X(24).
      *点数識別区分
                 07  RECE85-TENSIKIBETU   PIC  9(01).
      *点数
                 07  RECE85-TEN           PIC  9(09)V9(02).
      *明細請求フラグ
                 07  RECE85-MEISKYFLG     PIC  X(01).
      *自動算定区分
                 07  RECE85-AUTOKBN       PIC  X(01).
      *名称入力番号
                 07  RECE85-INPUTNUM      PIC  9(03).
      *点数欄集計先識別
                 07  RECE85-TENTTLKBN     PIC  9(03).
      *データ区分
                 07  RECE85-DATAKBN       PIC  9(01).
      *包括対象検査
                 07  RECE85-HOUKNSKBN     PIC  9(02).
      *検査等実施判断
                 07  RECE85-KNSJISKBN     PIC  9(01).
      *検査等実施判断グループ
                 07  RECE85-KNSJISGRPKBN  PIC  9(02).
      *各種番号等
                 07  RECE85-CDKBNINF.
                     09  RECE85-CDKBN-KBN        PIC  X(01).
                     09  RECE85-CDKBN-SYO        PIC  9(01).
                     09  RECE85-CDKBN-BU         PIC  9(02).
                     09  RECE85-CDKBN-KBNNUM     PIC  9(03).
                     09  RECE85-CDKBN-KBNNUM-EDA PIC  9(02).
                     09  RECE85-CDKBN-KOUBAN     PIC  9(02).
                     09  RECE85-KOUHYOJYUNNUM    PIC  9(09).
      *告示等識別区分１
                 07  RECE85-KOKUJISKBKBN1 PIC  9(01).
      *告示等識別区分２
                 07  RECE85-KOKUJISKBKBN2 PIC  9(01).
      *通則加算対象外区分
                 07  RECE85-TUSOKUGAIKBN  PIC  9(01).
      *時間外加算区分
                 07  RECE85-TIMEKSNKBN    PIC  9(01).
      *きざみ値計算識別
                 07  RECE85-KZMCOMPSIKIBETU
                                          PIC  9(01).
      *注加算コード
                 07  RECE85-CHUKSNINF.
      *コード
                     09  RECE85-CHUKSNCD         PIC  X(04).
      *通番
                     09  RECE85-CHUKSNTUBAN      PIC  X(01).
      *検体検査コメント
                 07  RECE85-KENTAICOMMENT        PIC  9(01).
      *入力コメント
                 07  RECE85-INPUTCOMENT          PIC  X(140).
      *名称情報
             05  RECE85-NAMEINF.
      *漢字有効桁数
                 07  RECE85-YUKOKETA      PIC  9(03).
      *漢字名称
                 07  RECE85-NAME          PIC  X(300).
      *識別フラグ情報
             05  RECE85-SIKFLGINF.
      *識別フラグ
                 07  RECE85-SIKFLG        PIC  X(01).
      *特定器材種別１
             05  RECE85-TOKUKIZAISBT1     PIC  9(01).
      *酸素等区分
             05  RECE85-SANSOKBN          PIC  9(01).
      *レセプト編集情報
             05  RECE85-SSTKIJUNCDTBL     OCCURS 10.
                 07  RECE85-SSTKIJUNCD    PIC  9(03).
      *その他情報
             05  RECE85-SONOTAINF.
      *レセプト電算対象外フラグ
                 07  RECE85-RECEDENFLG       PIC  X(01).
      *レセプト対象外フラグ
                 07  RECE85-RECEPTFLG        PIC  X(01).
      *レセ電用特定器材商品名
                 07  RECE85-RECEDENNAME      PIC  X(300).
      *レセ用特定器材商品名編集フラグ
                 07  RECE85-RECEHENFLG       PIC  X(01).
      *関係コメント関連設定フラグ
                 07  RECE85-SETCOMATFLG      PIC  9(03).
      *
      *医療情報連携用情報
             05  RECE85-IRYORENKEIINF.
      *一般名記載院外処方フラグ
                 07  RECE85-INGAIFLG         PIC  X(01).
      *一般名記載院外処方フラグ（レセプト用）
                 07  RECE85-INGAIFLG-RECE    PIC  X(01).
      *一般名コード（薬価基準コード）
                 07  RECE85-YAKKAKJNCD       PIC  X(12).
      *一般名処方加算区分
                 07  RECE85-GENERIC-KBN      PIC  X(01).
      *一般名称
                 07  RECE85-GENERICNAME      PIC  X(200).
      *漢字有効桁数
                 07  RECE85-GENERICYUKOKETA  PIC  9(03).
      *
