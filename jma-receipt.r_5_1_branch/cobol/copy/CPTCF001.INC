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
      *    月次統計データ（基本情報）     COPY (CPTCF001.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.02.01    NACL-藤原    08/04/10  平成２０年４月改正対応
      *                                     就学前区分追加
      *
      *  04.05.01    NACL-藤原    12/07/02  住所欄に番地方書を追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      ************************************************************
      *01  TOUKEI01-REC.
           03  TOUKEI01-KEY.
      *        医療機関ＩＤ
               05  TOUKEI01-HOSPNUM        PIC 9(2).
      *        診療年月
               05  TOUKEI01-SRYYM          PIC  9(06).
      *        入外区分
               05  TOUKEI01-NYUGAIKBN      PIC  X(01).
      *        患者ＩＤ
               05  TOUKEI01-PTID           PIC  9(10).
      *        入所中区分
               05  TOUKEI01-RECEIPT-KBN    PIC  X(10).
      *        レセプト診療科
               05  TOUKEI01-RECEKA         PIC  X(02).
      *        主保険保険番号
               05  TOUKEI01-HKNNUM         PIC  X(03).
      *        公費ＩＤ
               05  TOUKEI01-KOHID          PIC  9(10).
      *        保険者番号        
               05  TOUKEI01-HKNJANUM       PIC  X(08).
      *        本人家族区分
               05  TOUKEI01-HONKZKKBN      PIC  X(01).
      *        補助区分
               05  TOUKEI01-HOJOKBN        PIC  X(01).
      *        公費ＩＤ（公費単独時使用）
               05  TOUKEI01-KOH-KOHID      PIC  9(10).
      *
      *        主保険ＩＤ
           03  TOUKEI01-HKNID              PIC  9(10).
      *
      *    主保険情報
           03  TOUKEI01-HKN-AREA.
               05  TOUKEI01-HKN-TBL        OCCURS  5.
      *            主保険−保険ＩＤ
                   07  TOUKEI01-THKNID     PIC  9(10).
      *            主保険−保険者番号
      *****        07  TOUKEI01-THKNJANUM  PIC  X(08).
      *            主保険−記号
                   07  TOUKEI01-TKIGO      PIC  X(80).
      *            主保険−番号
                   07  TOUKEI01-TNUM       PIC  X(80).
      *            被保険者名
                   07  TOUKEI01-THIHKNJANAME
                                           PIC  X(100).
      *            資格取得年月日
                   07  TOUKEI01-TSKKGETYMD PIC  X(08).
      *            適用開始年月日
                   07  TOUKEI01-TTEKSTYMD  PIC  X(08). 
      *            適用終了年月日
                   07  TOUKEI01-TTEKEDYMD  PIC  X(08). 
      *            主保険−継続区分
                   07  TOUKEI01-TCONTKBN   PIC  X(01).
      *            主保険−補助区分
                   07  TOUKEI01-THOJOKBN   PIC X(01).
      *            主保険−本人家族区分
                   07  TOUKEI01-THONKZKKBN PIC  X(01).
      *        主保険−法別番号
               05  TOUKEI01-HKNHBTNUM      PIC  X(02).
      *        主保険−公費種別区分
               05  TOUKEI01-HKNKOHSBTKBN   PIC X(01).
      *
      *    老人保健情報
           03  TOUKEI01-RJNHKN.
      *        老人保険−保険番号
               05  TOUKEI01-RJNHKNNUM      PIC  X(03).
      *        老人支払区分
               05  TOUKEI01-RJNPAYKBN      PIC  X(02).
      *        老人保険−法別番号
               05  TOUKEI01-RJNHBTNUM      PIC  X(02).
      *        老人保険−市町村番号
               05  TOUKEI01-RJNFTNJANUM    PIC  X(08).
      *        老人保険−受給者番号
               05  TOUKEI01-RJNJKYSNUM     PIC  X(07).
      *        老人適用開始年月日
               05  TOUKEI01-RJNTEKSTYMD    PIC  X(08).
      *        老人適用終了年月日
               05  TOUKEI01-RJNTEKEDYMD    PIC  X(08).
      * 
      *    公費情報
           03 TOUKEI01-KOHI-AREA.
               05  TOUKEI01-KOHI-TBL       OCCURS  10.
      *            公費ＩＤ
                   07  TOUKEI01-TKOHID     PIC  9(10).
      *            公費保険番号
                   07  TOUKEI01-TKOHHKNNUM PIC  X(03).
      *            公費支払区分
                   07  TOUKEI01-TKOHPAYKBN PIC  X(02).
      *            公費−法別番号
                   07  TOUKEI01-TKOHHBTNUM PIC  X(02).
      *            公費−負担者番号
                   07  TOUKEI01-TKOHFTNJANUM
                                           PIC  X(08).
      *            公費−受給者番号
                   07  TOUKEI01-TKOHJKYSNUM
                                           PIC  X(20).
      *            公費−レセプト請求区分
                   07  TOUKEI01-TRECESKYKBN PIC  X(01).
      *            適用開始年月日
                   07  TOUKEI01-TKOHTEKSTYMD    PIC  X(08).
      *            適用終了年月日
                   07  TOUKEI01-TKOHTEKEDYMD    PIC  X(08).
      *
      *    労災情報
           03  TOUKEI01-HKN.
      *        労災−労災保険区分
               05  TOUKEI01-HKNKBN          PIC  X(01).
      *        労災−患者交付番号
               05  TOUKEI01-KOFUNUM         PIC  X(14).
      *        労災−被保険者名
               05  TOUKEI01-HIHKNJANAME     PIC  X(100).
      *        労災−事業所名称
               05  TOUKEI01-JIGYOUNAME      PIC  X(80).
      *        労災−所在地都道府県名
               05  TOUKEI01-PREFNAME        PIC  X(20).
      *        労災−所在地都道府県区分
               05  TOUKEI01-PREFKBN         PIC  X(01).
      *        労災−所在地郡市区名
               05  TOUKEI01-CITYNAME        PIC  X(80).
      *        労災−所在地郡市区区分
               05  TOUKEI01-CITYKBN         PIC  X(01).
      *        労災−新継再別
               05  TOUKEI01-SINKEI          PIC  X(01).
      *        労災−転記事由
               05  TOUKEI01-TENKI           PIC  X(01).
      *        労災−傷病年月日
               05  TOUKEI01-SHOBYOYMD       PIC  X(08).
      *        労災−四肢特例区分
               05  TOUKEI01-SISIKBN         PIC  X(01).
      *        労災−療養開始年月日
               05  TOUKEI01-RYOSTYMD        PIC  X(08).
      *        労災−療養終了年月日
               05  TOUKEI01-RYOEDYMD        PIC  X(08).
      *        労災−損傷区分
               05  TOUKEI01-SONSHOUKBN      PIC  X(02).
      *        労災−損傷区分−枝番
               05  TOUKEI01-SONSHOUKBN-EDANUM
                                            PIC  X(01).
      *        労災−アフター療養開始年月日
               05  TOUKEI01-SINSATUYMD      PIC  X(08).
      *        労災−アフター療養終了年月日
               05  TOUKEI01-SINSATUEND      PIC  X(08).
      *        労災−災害区分
               05  TOUKEI01-SAIGAIKBN       PIC  X(01).
      *        労災−自賠責請求区分
               05  TOUKEI01-JIBAISEIKBN     PIC  X(01).
      *
      *    患者情報
           03  TOUKEI01-PTINF-AREA.
      *        患者番号  
               05  TOUKEI01-PTNUM           PIC  X(20).
      *        カナ氏名
               05  TOUKEI01-KANANAME        PIC  X(50).
      *        漢字氏名
               05  TOUKEI01-NAME            PIC  X(100).
      *        性別
               05  TOUKEI01-SEX             PIC  X(01).
      *        生年月日
               05  TOUKEI01-BIRTHDAY        PIC  9(08).
      *        年齢
               05  TOUKEI01-AGE             PIC  9(03).
      *        就学前区分（１：就学前）
               05  TOUKEI01-SYUGAKUKBN      PIC 9(01).
      *        自宅−郵便番号
               05  TOUKEI01-HOME-POST       PIC  X(07).
      *        自宅−住所
               05  TOUKEI01-HOME-ADRS       PIC  X(400).
      *        自宅−昼電話番号
               05  TOUKEI01-HOME-TEL1       PIC  X(15).
      *       地方公共団体コード
               05  TOUKEI01-LPUBCD          PIC  X(05).
      *    保険組合せ
           03 TOUKEI01-HKNCOMBIINF.
               05  TOUKEI01-HKNCOMBI        PIC  9(04)  OCCURS  10.
      *    診療科情報
           03 TOUKEI01-SRYKAINF.
               05  TOUKEI01-SRYKA           PIC  X(02)  OCCURS  20.
      *
      *    レセプト種別
           03  TOUKEI01-RECESYUBETU         PIC  9(04).
      *
      *    未訂正区分
           03  TOUKEI01-TEISEIKBN           PIC  X(01).
