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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBT020.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 月次統計ＣＳＶデータ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/11/12    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/02/01  病名コード２１に変更
      *
      *  03.03.01    NACL-藤原    06/10/10  主保険がない場合、精神入院は
      *                                     公費単独のレセプトとする
      *  03.03.02    NACL-藤原    06/12/15  入院の包括のデータ作成の修正
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/08/01  クライアント保存対応
      *
      *  04.04.02    NACL-藤原    09/01/14  月途中に医保と生保の併用から
      *                                     生保単独になったときのまとめ
      *
      *  04.05.01    NACL-藤原    10/02/12  拡張漢字対応
      *  04.05.02    NACL-藤原    12/06/22  処理結果画面への件数表示
      *  04.05.03    NACL-藤原    12/07/02  住所欄に番地方書を追加
      *  04.05.04    NACL-藤原    12/11/15  剤点数等のマイナス表記対応
      *
      *  04.06.01    NACL-藤原    10/06/21  日本語化（見出し）対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    月次統計データ（基本情報）
           SELECT  TOUKEI01-FILE   ASSIGN  TOUKEI01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI01-KEY
                                   FILE    STATUS  IS  STS-TOUKEI01.
      *
      *    月次統計データ（診療情報）
           SELECT  TOUKEI02-FILE   ASSIGN  TOUKEI02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI02-KEY
                                   FILE    STATUS  IS  STS-TOUKEI02.
      *
      *    月次統計データ（病名情報）
           SELECT  TOUKEI03-FILE   ASSIGN  TOUKEI03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI03-KEY
                                   FILE    STATUS  IS  STS-TOUKEI03.
      *
      *    ＣＳＶデータ（診療情報） 
           SELECT  TOUKEI04-FILE   ASSIGN  TOUKEI04PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI04.
      *
      *    ＣＳＶデータ（病名情報）
           SELECT  TOUKEI05-FILE   ASSIGN  TOUKEI05PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI05.
      *    エラーファイル
           SELECT  TOUKEIERR-FILE  ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    月次統計データ（基本情報）
       FD  TOUKEI01-FILE.
       01  TOUKEI01-REC. 
           COPY    "CPTCF001.INC".
           03  TOUKEI01-SEIHO-OCC.
               05  TOUKEI01-SEIHO-KEY
                                   PIC X(50)   OCCURS  10.
      *
      *    月次統計データ（診療情報）
       FD  TOUKEI02-FILE.
       01  TOUKEI02-REC. 
           COPY    "CPTCF002.INC".
      *
      *    月次統計データ（病名情報）
       FD  TOUKEI03-FILE.
       01  TOUKEI03-REC. 
           COPY    "CPTCF003.INC".
      *
      *    ＣＳＶデータ（診療情報）
       FD  TOUKEI04-FILE.
       01  TOUKEI04-REC            PIC X(3000). 
      *
      *    ＣＳＶデータ（病名情報）
       FD  TOUKEI05-FILE.
       01  TOUKEI05-REC            PIC X(3000). 
      *
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    月次統計データ（基本情報）
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI01PARA//.
      *
      *    月次統計データ（診療情報）
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI02PARA//.
      *
      *    月次統計データ（病名情報）
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI03PARA//.
      *
      *    ＣＳＶデータ（診療情報）
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //TOUKEI04PARA//.
      *01  TOUKEI04PARA.
      *****03  FILLER                  PIC  X(05) VALUE "/tmp/".
       01  TOUKEI04PARA-BASENAME.
           03  TOUKEI04PARA-HOSPNUM    PIC  9(02).
           03  TOUKEI04PARA-FILE-ID    PIC  X(12) VALUE "toukei01.dat".
      *
      *    ＣＳＶデータ（病名情報）
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //TOUKEI05PARA//.
      *01  TOUKEI05PARA.
      *****03  FILLER                  PIC  X(05) VALUE "/tmp/".
       01  TOUKEI05PARA-BASENAME.
           03  TOUKEI05PARA-HOSPNUM    PIC  9(02).
           03  TOUKEI05PARA-FILE-ID    PIC  X(12) VALUE "toukei02.dat".
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //TOUKEIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TOUKEI01                    PIC X(02).
           03  STS-TOUKEI02                    PIC X(02).
           03  STS-TOUKEI03                    PIC X(02).
           03  STS-TOUKEI04                    PIC X(02).
           03  STS-TOUKEI05                    PIC X(02).
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-TOUKEI02                    PIC 9(01).
           03  FLG-TOUKEI03                    PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TOUKEI01                    PIC 9(06).
           03  CNT-TOUKEI04                    PIC 9(06).
           03  CNT-TOUKEI05                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03). 
           03  IDY                             PIC 9(03). 
           03  IDZ                             PIC 9(03).
           03  IDW                             PIC 9(03).
           03  IDX1                            PIC 9(03). 
           03  IDX2                            PIC 9(03). 
           03  IDX3                            PIC 9(03). 
           03  IDXX                            PIC 9(03). 
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPID             PIC X(24).
               05  WRK-PARA-HOSPCD             PIC X(07).
               05  WRK-PARA-SRYYM              PIC X(06).
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-DATAKBN            PIC X(01).
               05  WRK-PARA-KOJINKBN           PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-FILENM1            PIC X(150).
               05  WRK-PARA-FILENM2            PIC X(150).
               05  WRK-PARA-FILE1              PIC X(20).
               05  WRK-PARA-FILE2              PIC X(20).
               05  WRK-PARA-FILENM1-FOLDER     PIC X(150).
               05  WRK-PARA-FILENM2-FOLDER     PIC X(150).
      *
           03  WRK-TOUKEIERR               PIC X(200). 
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-SYOKBN              PIC X(01).
           03  WRK-KOH-MAX             PIC 9(03).
           03  WRK-RENNUM              PIC 9(03).
      *
      *     右づめ編集用     
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-REC                 PIC X(3000).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(400).
      *********05  WRK-NUM             PIC ZZZZZZZZZ9.99999. 
               05  WRK-NUM             PIC ---------9.99999. 
               05  WRK-NAGASA          PIC 9(03).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *
      *    ワーク主保険情報
           03  WRK-HKN-AREA.
      *        主保険−保険ＩＤ
               05  WRK-HKN-HKNID       PIC  9(10).
      *        主保険−記号
               05  WRK-HKN-KIGO        PIC  X(80).
      *        主保険−番号
               05  WRK-HKN-NUM         PIC  X(80).
      *        被保険者名
               05  WRK-HKN-HIHKNJANAME
                                       PIC  X(100).
      *        資格取得年月日
               05  WRK-HKN-SKKGETYMD   PIC  X(08).
      *        適用開始年月日
               05  WRK-HKN-TEKSTYMD    PIC  X(08). 
      *        適用終了年月日
               05  WRK-HKN-TEKEDYMD    PIC  X(08). 
      *        主保険−継続区分
               05  WRK-HKN-CONTKBN     PIC  X(01).
      *        主保険−補助区分
               05  WRK-HKN-HOJOKBN     PIC X(01).
      *        主保険−本人家族区分
               05  WRK-HKN-HONKZKKBN   PIC  X(01).
      * 
      *    ワーク公費情報
           03  WRK-KOHI-AREA.
      *        公費ＩＤ
               07  WRK-KOHID          PIC  9(10).
      *        公費保険番号
               07  WRK-KOHHKNNUM      PIC  X(03).
      *        公費支払区分
               07  WRK-KOHPAYKBN      PIC  X(02).
      *        公費−法別番号
               07  WRK-KOHHBTNUM      PIC  X(02).
      *        公費−負担者番号
               07  WRK-KOHFTNJANUM    PIC  X(08).
      *        公費−受給者番号
               07  WRK-KOHJKYSNUM     PIC  X(20).
      *        公費−レセプト請求区分
               07  WRK-RECESKYKBN     PIC  X(01).
      *        適用開始年月日
               07  WRK-KOHTEKSTYMD    PIC  X(08).
      *        適用終了年月日
               07  WRK-KOHTEKEDYMD    PIC  X(08).
      *
       01  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(12)   VALUE   "医療機関ＩＤ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "医療機関コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "診療年月".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "レコード識別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "レセプト種別コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "入外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者ＩＤ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "漢字氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "性別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "生年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "レセ電診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "剤番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "剤内番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療行為区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療種別区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "剤点数識別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "剤点数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "剤回数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "手技点数１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "手技点数２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "薬剤点数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "材料点数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "保険組合せ番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "明細数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "明細診療行為コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "明細数量".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "明細回数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "点数識別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "点数金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "明細名称入力番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "明細自費金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "明細単位コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "明細単位名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "明細名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "明細正式名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "薬価基準コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "入外中外来識別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "医師コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "１日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "２日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "３日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "４日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "５日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "６日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "７日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "８日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "９日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１０日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１１日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１２日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１３日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１４日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１５日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１６日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１７日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１８日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "１９日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２０日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２１日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２２日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２３日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２４日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２５日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２６日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２７日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２８日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "２９日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "３０日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "３１日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "保険番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "保険ＩＤ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "保険者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "本人家族区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "補助区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "継続区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "記号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "資格取得年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "適用開始年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "適用終了年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "被保険者名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費番号１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費ＩＤ１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "支払区分１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "負担者番号１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "受給者番号１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "適用開始年月日１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "適用終了年月日１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費番号２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費ＩＤ２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "支払区分２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "負担者番号２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "受給者番号２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "適用開始年月日２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "適用終了年月日２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費番号３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費ＩＤ３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "支払区分３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "負担者番号３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "受給者番号３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "適用開始年月日３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "適用終了年月日３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費番号４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費ＩＤ４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "支払区分４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "負担者番号４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "受給者番号４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "適用開始年月日４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "適用終了年月日４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "労災保険区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "労災患者交付番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "労災事業所名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "労災新継再別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "労災転帰事由".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "労災傷病年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "労災四肢特例区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "労災療養開始年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "労災療養終了年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "労災損傷区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "労災損傷区分枝番".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(26)   VALUE
               "労災アフター療養開始年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(26)   VALUE
               "労災アフター療養終了年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "労災災害区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "労災自賠責請求区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "郵便番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "地方公共団体コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "住所".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "電話番号".
      *
       01  CSV-HEAD-02.
           03  FILLER              PIC X(12)   VALUE   "医療機関ＩＤ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "医療機関コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "診療年月".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "レコード識別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "レセプト種別コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "入外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者ＩＤ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "漢字氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "性別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "生年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "レセ電診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "診療開始日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "連番".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "病名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "病名文字数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "疑いフラグ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "主病名フラグ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "慢性疾患区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "保険組合せ番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "レセプト表示フラグ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "転帰区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(06)   VALUE   "転帰日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名編集フラグ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "保険病名フラグ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード５".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード６".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード７".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード８".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "病名コード９".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１０".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１５".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１６".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１７".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１８".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード１９".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード２０".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "病名コード２１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "郵便番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "地方公共団体コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "住所".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "電話番号".
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012". 
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    原爆保険番号 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".       
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPID
                                               WRK-PARA-HOSPCD
                                               WRK-PARA-SRYYM
                                               WRK-PARA-TERMID
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-KOJINKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               TOUKEIERR
                                               WRK-PARA-FILENM1
                                               WRK-PARA-FILENM2
                                               WRK-PARA-FILE1
                                               WRK-PARA-FILE2
                                               WRK-PARA-FILENM1-FOLDER
                                               WRK-PARA-FILENM2-FOLDER
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPA-HOSPNUM         TO  TOUKEI04PARA-HOSPNUM
                                           TOUKEI05PARA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBT020"      TO  JOB-PGID
           MOVE    "ＣＳＶデータ作成"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "TOKEI010"          TO  TOUKEI01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI01PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI01PARA-HOSPNUM
      *
           MOVE    "TOKEI020"          TO  TOUKEI02PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI02PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI02PARA-HOSPNUM
      *
           MOVE    "TOKEI030"          TO  TOUKEI03PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI03PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI03PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOUKEIERR           TO  SGETTEMP-BASENAMES  (1)
           MOVE    TOUKEI01PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    TOUKEI02PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    TOUKEI03PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           MOVE    TOUKEI04PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    TOUKEI05PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  TOUKEI01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  TOUKEI02PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  TOUKEI03PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  TOUKEI04PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  TOUKEI05PARA
      *
           MOVE   SGETTEMP-TEMPDIR     TO  WRK-SGETTEMP-TEMPDIR
      *
           OPEN    I-O                 TOUKEI01-FILE
                                       TOUKEI02-FILE
                                       TOUKEI03-FILE
                   OUTPUT              TOUKEI04-FILE
                                       TOUKEI05-FILE
      *
           PERFORM 900-TOUKEI01-READ-SEC   
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
      *    月次統計データ（診療情報）
           IF      WRK-PARA-DATAKBN    =   ZERO    OR  "1"
               MOVE    "1"                 TO  WRK-SYOKBN
      *
               INITIALIZE                      TOUKEI02-REC
               MOVE    TOUKEI01-KEY        TO  TOUKEI02-KEY1
      *
               START   TOUKEI02-FILE
                       KEY IS    >=   TOUKEI02-KEY
                   INVALID
                       MOVE    1           TO  FLG-TOUKEI02
                   NOT INVALID
                       PERFORM 900-TOUKEI02-READ-SEC
               END-START
      *     
               PERFORM UNTIL   FLG-TOUKEI02        =   1
                       OR      TOUKEI01-KEY    NOT =   TOUKEI02-KEY1
                   PERFORM 2001-FILE-HENSYU-SEC
                   PERFORM 900-TOUKEI02-READ-SEC
               END-PERFORM
      *
      *        生保まとめの生保単独分の診療情報
      *
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       10
                       OR      TOUKEI01-SEIHO-KEY (IDXX)
                                       =       SPACE
                   INITIALIZE                      TOUKEI02-REC
                   MOVE    TOUKEI01-SEIHO-KEY (IDXX)
                                               TO  TOUKEI02-KEY1
      *
                   START   TOUKEI02-FILE
                           KEY IS    >=   TOUKEI02-KEY
                       INVALID
                           MOVE    1           TO  FLG-TOUKEI02
                       NOT INVALID
                           PERFORM 900-TOUKEI02-READ-SEC
                   END-START
      *     
                   PERFORM UNTIL   FLG-TOUKEI02        =   1
                           OR      TOUKEI01-SEIHO-KEY (IDXX)
                                                   NOT =   TOUKEI02-KEY1
                       PERFORM 2001-FILE-HENSYU-SEC
                       PERFORM 900-TOUKEI02-READ-SEC
                   END-PERFORM
               END-PERFORM
           END-IF       
      *    
      *    月次統計データ（病名情報）
           IF      WRK-PARA-DATAKBN    =   ZERO    OR  "2"
               MOVE    "2"                 TO  WRK-SYOKBN
      *
               INITIALIZE                      TOUKEI03-REC
               MOVE    TOUKEI01-HOSPNUM    TO  TOUKEI03-HOSPNUM
               MOVE    TOUKEI01-SRYYM      TO  TOUKEI03-SRYYM
               MOVE    TOUKEI01-NYUGAIKBN  TO  TOUKEI03-NYUGAIKBN
               MOVE    TOUKEI01-PTID       TO  TOUKEI03-PTID
               MOVE    TOUKEI01-RECEIPT-KBN
                                           TO  TOUKEI03-RECEIPT-KBN
               MOVE    TOUKEI01-RECEKA     TO  TOUKEI03-RECEKA
      *
               START   TOUKEI03-FILE
                       KEY IS    >=   TOUKEI03-KEY
                   INVALID
                       MOVE    1           TO  FLG-TOUKEI03
                   NOT INVALID
                       PERFORM 900-TOUKEI03-READ-SEC
               END-START
      *     
               PERFORM UNTIL   FLG-TOUKEI03    =   1
                       OR      TOUKEI01-KEY (1:31)
                                           NOT =   TOUKEI03-KEY (1:31)
                   PERFORM 2002-FILE-HENSYU-SEC
                   PERFORM 900-TOUKEI03-READ-SEC
               END-PERFORM
           END-IF       
      *
           PERFORM 900-TOUKEI01-READ-SEC   
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ＣＳＶファイル（診療データ）処理
      *****************************************************************
       2001-FILE-HENSYU-SEC         SECTION.
      *
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
           IF      CNT-TOUKEI04        =   ZERO
               PERFORM 2003-CSV-HEAD-HENSYU-SEC
           END-IF
      * 
      *    基本情報取得
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *    医療機関ＩＤ      
           MOVE    WRK-PARA-HOSPID     TO  WRK-DATA
           MOVE    24                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    医療機関コード
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    WRK-PARA-HOSPCD     TO  WRK-DATA
               MOVE    7                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療年月
           MOVE    WRK-PARA-SRYYM      TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    レコード識別
      *****MOVE    ?                   TO  WRK-NUM
      *****MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC  
      *    レセプト種別
           MOVE    TOUKEI01-RECESYUBETU
                                       TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    入外区分
           MOVE    TOUKEI01-NYUGAIKBN  TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *    患者情報
      *  
      *    患者番号
           MOVE    TOUKEI01-PTNUM      TO  WRK-DATA
           MOVE    20                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    患者ＩＤ
           MOVE    TOUKEI01-PTID       TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC  
      *    カナ氏名
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-KANANAME   TO  WRK-DATA
               MOVE    50                  TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    氏名
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-NAME       TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    性別
           MOVE    TOUKEI01-SEX        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    生年月日
           MOVE    TOUKEI01-BIRTHDAY   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療情報
      *
      *    診療科
           MOVE    TOUKEI02-SRYKA      TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    レセ電診療科
           MOVE    TOUKEI02-RECESRYKA  TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    剤番号
           MOVE    TOUKEI02-ZAINUM     TO  WRK-NUM
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    剤内番号
           MOVE    TOUKEI02-RENNUM     TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    診療行為区分
           MOVE    TOUKEI02-SRYKBN     TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    診療種別区分
           MOVE    TOUKEI02-SRYSYUKBN  TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    剤点数識別
           IF      TOUKEI02-SRYKBN     =   "95"    OR  "96"
               MOVE    1                   TO  WRK-NUM
           ELSE
               MOVE    3                   TO  WRK-NUM
           END-IF
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    剤点数
           MOVE    TOUKEI02-ZAITEN     TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    剤回数
           MOVE    TOUKEI02-ZAIKAISU   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    手技点数１
           MOVE    TOUKEI02-SYUTEN1    TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    手技点数２
           MOVE    TOUKEI02-SYUTEN2    TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    薬剤点数
           MOVE    TOUKEI02-YKZTEN     TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    器材点数
           MOVE    TOUKEI02-KIZAITEN   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    保険組合せ
           MOVE    TOUKEI02-HKNCOMBI   TO  WRK-NUM
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    明細数
           MOVE    TOUKEI02-MEISAISU   TO  WRK-NUM
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    診療行為コード
           MOVE    TOUKEI02-SRYCD      TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    数量
           MOVE    TOUKEI02-SRYSURYO   TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           MOVE    5                   TO  WRK-SYOSUU     
           PERFORM 5005-NUM-HENSYU-SEC    
      *    回数
           MOVE    TOUKEI02-SRYKAISU   TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    点数識別
           MOVE    TOUKEI02-TENSIKIBETU
                                       TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    点数
           MOVE    TOUKEI02-TEN        TO  WRK-NUM
           MOVE    11                  TO  WRK-NAGASA
           MOVE    2                   TO  WRK-SYOSUU     
           PERFORM 5005-NUM-HENSYU-SEC    
      *    名称入力番号
           MOVE    TOUKEI02-INPUTNUM   TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    自費金額
           MOVE    TOUKEI02-JIHIMONEY  TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    単位コード
           MOVE    TOUKEI02-TANICD     TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    単位名称
           MOVE    TOUKEI02-TANINAME   TO  WRK-DATA
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    名称
           MOVE    TOUKEI02-NAME       TO  WRK-DATA
           MOVE    100                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    正式名称
           MOVE    TOUKEI02-FORMALNAME TO  WRK-DATA
           MOVE    200                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    薬価基準コード
           MOVE    TOUKEI02-YAKKAKJNCD TO  WRK-DATA
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    入外中外来識別
      *****MOVE    ??                  TO  WRK-DATA
      *****MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    ドクターコード
           MOVE    TOUKEI02-DRCD       TO  WRK-DATA
           MOVE    5                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
      *カレンダー情報
      *
      *    カレンダー
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       31
               MOVE    TOUKEI02-KAISU (IDX1)   TO  WRK-NUM
               MOVE    3                       TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM
      *
      *保険情報
      * 
      *    保険番号
           MOVE    TOUKEI02-HKNNUM     TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    保険ＩＤ
           MOVE    TOUKEI02-HKNID      TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    保険者番号
           IF      TOUKEI02-HKNNUM NOT =   SPACE   AND "9999999"    
               MOVE    TOUKEI02-HKNJANUM   TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
      *    保険情報取得
           INITIALIZE                  WRK-HKN-AREA
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       5
                   OR      TOUKEI01-THKNID (IDX1)  =   ZERO
               IF      TOUKEI02-HKNID      =   TOUKEI01-THKNID (IDX1)
                   MOVE    TOUKEI01-HKN-TBL (IDX1) TO  WRK-HKN-AREA
                   MOVE    5                       TO  IDX1
               END-IF
           END-PERFORM
      *    本人家族区分
           MOVE    WRK-HKN-HONKZKKBN   TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    補助区分
           MOVE    WRK-HKN-HOJOKBN     TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    継続区分     
           MOVE    WRK-HKN-CONTKBN     TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    記号
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    WRK-HKN-KIGO        TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    番号
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    WRK-HKN-NUM         TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    資格取得日
           MOVE    WRK-HKN-SKKGETYMD   TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    資格開始日
           MOVE    WRK-HKN-TEKSTYMD    TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    資格終了日
           MOVE    WRK-HKN-TEKEDYMD    TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    被保険者名
           IF      WRK-PARA-KOJINKBN   =   "1"
               EVALUATE    TOUKEI01-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                       MOVE    TOUKEI01-HIHKNJANAME
                                                   TO  WRK-DATA
                   WHEN    OTHER
                       MOVE    WRK-HKN-HIHKNJANAME TO  WRK-DATA
               END-EVALUATE
               MOVE    100                 TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    
      *公費情報
      *
           MOVE    ZERO                TO  WRK-KOH-MAX
      *
           IF      TOUKEI01-KOHID      >   ZERO
               MOVE    TOUKEI01-RJNHKNNUM  TO  WRK-KOHHKNNUM
               MOVE    TOUKEI01-KOHID      TO  WRK-KOHID
               MOVE    TOUKEI01-RJNPAYKBN  TO  WRK-KOHPAYKBN
               MOVE    TOUKEI01-RJNFTNJANUM
                                           TO  WRK-KOHFTNJANUM
               MOVE    TOUKEI01-RJNJKYSNUM TO  WRK-KOHJKYSNUM
               MOVE    TOUKEI01-RJNTEKSTYMD
                                           TO  WRK-KOHTEKSTYMD
               MOVE    TOUKEI01-RJNTEKEDYMD
                                           TO  WRK-KOHTEKEDYMD
               PERFORM 20011-KOH-HENSYU-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       4
                   OR      WRK-KOH-MAX     =   4 
               INITIALIZE                  WRK-KOHI-AREA
               IF      TOUKEI02-TKOHID (IDX1)  >   ZERO
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       10
                           OR      TOUKEI01-TKOHID (IDX2)  =   ZERO
                       IF      TOUKEI02-TKOHID (IDX1)  =
                                               TOUKEI01-TKOHID (IDX2)
                           MOVE    TOUKEI01-KOHI-TBL (IDX2)
                                                       TO  WRK-KOHI-AREA
                           PERFORM 20011-KOH-HENSYU-SEC
                           MOVE    10                  TO  IDX2
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       4
                   OR      WRK-KOH-MAX     =   4 
               INITIALIZE                  WRK-KOHI-AREA
               PERFORM 20011-KOH-HENSYU-SEC
           END-PERFORM
      *
      *労災情報
      *    
      *    保険区分
           MOVE    TOUKEI01-HKNKBN         TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    患者交付番号
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-KOFUNUM        TO  WRK-DATA
               MOVE    14                      TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    事業所名称
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-JIGYOUNAME     TO  WRK-DATA
               MOVE    40                      TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    新継再別
           MOVE    TOUKEI01-SINKEI         TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    転帰事由
           MOVE    TOUKEI01-TENKI          TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    傷病年月日
           MOVE    TOUKEI01-SHOBYOYMD      TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    四肢特例区分
           MOVE    TOUKEI01-SISIKBN        TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    療養開始年月日
           MOVE    TOUKEI01-RYOSTYMD       TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    療養終了年月日
           MOVE    TOUKEI01-RYOEDYMD       TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    損傷区分
           MOVE    TOUKEI01-SONSHOUKBN     TO  WRK-DATA
           MOVE    2                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    損傷区分枝番
           MOVE    TOUKEI01-SONSHOUKBN-EDANUM
                                           TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    アフター療養開始年月日
           MOVE    TOUKEI01-SINSATUYMD     TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    アフター療養終了年月日
           MOVE    TOUKEI01-SINSATUEND     TO  WRK-NUM
           MOVE    8                       TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    災害区分
           MOVE    TOUKEI01-SAIGAIKBN      TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    自賠責請求区分
           MOVE    TOUKEI01-JIBAISEIKBN    TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *患者属性情報
      *
      *    郵便番号
           MOVE    TOUKEI01-HOME-POST      TO  WRK-DATA
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    地方公共団体コード
           MOVE    TOUKEI01-LPUBCD         TO  WRK-DATA
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    住所
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-HOME-ADRS      TO  WRK-DATA
               MOVE    400                     TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    電話番号
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-HOME-TEL1      TO  WRK-DATA
               MOVE    15                      TO  WRK-NAGASA
           END-IF
           MOVE    1                       TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      * 
           PERFORM 5001-FILE-WRITE-SEC
           .
       2001-FILE-SYORI-EXT.
           EXIT. 
      *            
      *
      *****************************************************************
      *    ＣＳＶファイル（基本データ）公費情報編集処理
      *****************************************************************
       20011-KOH-HENSYU-SEC        SECTION.
      *
      *    公費保険番号     
           MOVE    WRK-KOHHKNNUM       TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    公費ＩＤ
           MOVE    WRK-KOHID           TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    支払区分
           MOVE    WRK-KOHPAYKBN       TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    負担者番号
           MOVE    WRK-KOHFTNJANUM     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    受給者番号
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    WRK-KOHJKYSNUM      TO  WRK-DATA
               MOVE    20                  TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    適用開始日
           MOVE    WRK-KOHTEKSTYMD     TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    適用終了日
           MOVE    WRK-KOHTEKEDYMD     TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
           ADD     1                   TO  WRK-KOH-MAX
           .
       20011-KOH-HENSYU-EXT.
           EXIT. 
      *            
      *
      *****************************************************************
      *    ＣＳＶファイル（病名データ）処理
      *****************************************************************
       2002-FILE-HENSYU-SEC         SECTION.
      *
      *    労災・自賠責のときは保険組合せでチェック
           EVALUATE    TOUKEI01-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
               WHEN    WRK-CONS-JIBAI-HKNNUM
                   IF      TOUKEI03-HKNCOMBI   =   ZERO
                       GO  TO  2002-FILE-HENSYU-EXT
                   END-IF
      *
                   MOVE    ZERO            TO  IDZ  
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       10
                           OR      TOUKEI01-HKNCOMBI (IDX)   =   ZERO 
                       IF      TOUKEI03-HKNCOMBI   =
                                            TOUKEI01-HKNCOMBI (IDX)
                           MOVE    IDX             TO  IDZ
                           MOVE    10              TO  IDX
                       END-IF
                   END-PERFORM
                   IF      IDZ            =   ZERO          
                       GO  TO  2002-FILE-HENSYU-EXT
                   END-IF
           END-EVALUATE 
      *
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
           IF      CNT-TOUKEI05        =   ZERO
               PERFORM 2003-CSV-HEAD-HENSYU-SEC
           END-IF
      *            
      *基本情報
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *    医療機関ＩＤ      
           MOVE    WRK-PARA-HOSPID     TO  WRK-DATA
           MOVE    24                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    医療機関コード
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    WRK-PARA-HOSPCD     TO  WRK-DATA
               MOVE    7                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療年月
           MOVE    WRK-PARA-SRYYM      TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    レコード識別
      *****MOVE    ?                   TO  WRK-NUM
      *****MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC  
      *    レセプト種別
           MOVE    TOUKEI01-RECESYUBETU
                                       TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    入外区分
           MOVE    TOUKEI01-NYUGAIKBN  TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *
      *患者情報
      *  
      *    患者番号
           MOVE    TOUKEI01-PTNUM      TO  WRK-DATA
           MOVE    20                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    患者ＩＤ
           MOVE    TOUKEI01-PTID       TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC  
      *    カナ氏名
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-KANANAME   TO  WRK-DATA
               MOVE    50                  TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    氏名
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-NAME       TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    性別
           MOVE    TOUKEI01-SEX        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    生年月日
           MOVE    TOUKEI01-BIRTHDAY   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *病名情報
      *
      *    診療科
           MOVE    TOUKEI03-SRYKA      TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    レセ電診療科
           MOVE    TOUKEI03-RECESRYKA  TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療開始日
           MOVE    TOUKEI03-SRYYMD     TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    連番
           MOVE    TOUKEI03-RENNUM     TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    病名
           MOVE    TOUKEI03-BYOMEI     TO  WRK-DATA
           MOVE    200                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    病名文字数
           MOVE    TOUKEI03-BYOMEIMOJI TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    疑いフラグ
           MOVE    TOUKEI03-UTAGAIFLG  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    主病名フラグ
           MOVE    TOUKEI03-SYUBYOFLG  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    慢性疾患区分
           MOVE    TOUKEI03-MANSEIKBN  TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    保険組合せ
           MOVE    TOUKEI03-HKNCOMBI   TO  WRK-NUM
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    レセプト表示区分
           MOVE    TOUKEI03-REZEFLG    TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    転帰区分
           MOVE    TOUKEI03-TENKIKBN   TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    転帰日付
           MOVE    TOUKEI03-TENKIYMD   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    病名編集フラグ
           MOVE    TOUKEI03-BYOMEIHENFLG
                                       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    保険病名フラグ
           MOVE    TOUKEI03-HKNBYOMEIFLG
                                       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    病名コード数
           MOVE    TOUKEI03-BYOMEICDSU TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    病名コード
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       21
               MOVE    TOUKEI03-BYOMEICD  (IDX1)   TO  WRK-DATA
               MOVE    7                           TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
           END-PERFORM
      *
      *患者属性情報
      *
      *    郵便番号
           MOVE    TOUKEI01-HOME-POST      TO  WRK-DATA
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    地方公共団体コード
           MOVE    TOUKEI01-LPUBCD         TO  WRK-DATA
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    住所
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-HOME-ADRS      TO  WRK-DATA
               MOVE    200                     TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    電話番号
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI01-HOME-TEL1      TO  WRK-DATA
               MOVE    15                      TO  WRK-NAGASA
           END-IF
           MOVE    1                       TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      * 
           PERFORM 5001-FILE-WRITE-SEC
           .
       2002-FILE-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       2003-CSV-HEAD-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   STRING  CSV-HEAD-01         DELIMITED  BY  SIZE
                           WRK-KAIGYO          DELIMITED  BY  SIZE
                           INTO                WRK-REC
                   END-STRING
                   MOVE    WRK-REC             TO  TOUKEI04-REC  
                   WRITE   TOUKEI04-REC 
               WHEN    "2"
                   STRING  CSV-HEAD-02         DELIMITED  BY  SIZE
                           WRK-KAIGYO          DELIMITED  BY  SIZE
                           INTO                WRK-REC
                   END-STRING
                   MOVE    WRK-REC             TO  TOUKEI05-REC  
                   WRITE   TOUKEI05-REC 
           END-EVALUATE
           .
       2003-CSV-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    改行コード編集 
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING        
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    WRK-REC             TO  TOUKEI04-REC  
                   WRITE   TOUKEI04-REC 
                   ADD     1                   TO  CNT-TOUKEI04
               WHEN    "2"
                   MOVE    WRK-REC             TO  TOUKEI05-REC  
                   WRITE   TOUKEI05-REC 
                   ADD     1                   TO  CNT-TOUKEI05
           END-EVALUATE
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    右ずめ編集処理
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *              
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN                            
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
           .
       5003-DATA-RIGHT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
               CONTINUE
           ELSE                   
               PERFORM VARYING IDW FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW <       1
                   IF      WRK-DATA (IDW:1)    NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA (1:IDW)
                                               TO  WRK-REC
                       ELSE
                           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-REC
                           END-STRING
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH        
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW       
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ                  
                       MOVE    10                TO  IDW
                   END-IF
               END-PERFORM
           END-IF    
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR         =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR         TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-TOUKEIERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TOUKEI01-FILE
                   TOUKEI02-FILE
                   TOUKEI03-FILE
                   TOUKEI04-FILE
                   TOUKEI05-FILE
      *
           IF      CNT-TOUKEI01           =   ZERO  
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        ファイル保存情報更新
               PERFORM 3001-FILE-INFO-INSERT-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           COMPUTE JOB-UPDCNT  =   CNT-TOUKEI04    +
                                   CNT-TOUKEI05
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TOUKEI01  CNT-IN :" CNT-TOUKEI01
           DISPLAY "TOUKEI04  CNT-OUT:" CNT-TOUKEI04
           DISPLAY "TOUKEI05  CNT-OUT:" CNT-TOUKEI05
           DISPLAY "*** ORCBT020 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-RENNUM
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
      *
      *    月次統計データ（診療情報）
           IF    ( WRK-PARA-DATAKBN    =   ZERO    OR  "1" )
           AND   ( WRK-PARA-FILE1      =   "6"             )
      *****AND   ( CNT-TOUKEI04        >   ZERO            )
               ADD     1               TO  WRK-RENNUM
      *
               MOVE    "toukei1.sh"    TO  SFILESV-SHELLID  (WRK-RENNUM)
               MOVE    1               TO  SFILESV-SHORI-RENNUM
                                                            (WRK-RENNUM)
               MOVE    WRK-RENNUM      TO  SFILESV-RENNUM   (WRK-RENNUM)
               MOVE    WRK-PARA-SRYYM  TO  SFILESV-SRYYM    (WRK-RENNUM)
      *********MOVE    "/tmp/"         TO  SFILESV-FOR-FOLDER
               MOVE    WRK-SGETTEMP-TEMPDIR
                                       TO  SFILESV-FOR-FOLDER
                                                            (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM1
                                       TO  SFILESV-FOR-DATA (WRK-RENNUM)
      *********MOVE    WRK-PARA-FILENM1-FOLDER
      *********                        TO  SFILESV-TO-FOLDER
      *********                                             (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM1
                                       TO  SFILESV-TO-DATA  (WRK-RENNUM)
               MOVE    CNT-TOUKEI04    TO  SFILESV-CNT-ALL  (WRK-RENNUM)
               MOVE    "月次統計データ（診療情報）"
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
               MOVE    "1"             TO  SFILESV-DATA-TYPE(WRK-RENNUM) 
           END-IF
      *
      *    月次統計データ（病名情報）
           IF    ( WRK-PARA-DATAKBN    =   ZERO    OR  "2" )
           AND   ( WRK-PARA-FILE2      =   "6"             )
      *****AND   ( CNT-TOUKEI05        >   ZERO            )
               ADD     1               TO  WRK-RENNUM
      *
               MOVE    "toukei1.sh"    TO  SFILESV-SHELLID  (WRK-RENNUM)
               MOVE    1               TO  SFILESV-SHORI-RENNUM
                                                            (WRK-RENNUM)
               MOVE    WRK-RENNUM      TO  SFILESV-RENNUM   (WRK-RENNUM)
               MOVE    WRK-PARA-SRYYM  TO  SFILESV-SRYYM    (WRK-RENNUM)
      *********MOVE    "/tmp/"         TO  SFILESV-FOR-FOLDER
               MOVE    WRK-SGETTEMP-TEMPDIR
                                       TO  SFILESV-FOR-FOLDER
                                                            (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM2
                                       TO  SFILESV-FOR-DATA (WRK-RENNUM)
      *********MOVE    WRK-PARA-FILENM2-FOLDER
      *********                        TO  SFILESV-TO-FOLDER
      *********                                             (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM2
                                       TO  SFILESV-TO-DATA  (WRK-RENNUM)
               MOVE    CNT-TOUKEI05    TO  SFILESV-CNT-ALL  (WRK-RENNUM)
               MOVE    "月次統計データ（病名情報）"
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
               MOVE    "1"             TO  SFILESV-DATA-TYPE(WRK-RENNUM) 
           END-IF
      *
           IF      WRK-RENNUM          >   ZERO   
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    月次統計データ（基本情報）読込
      *****************************************************************
       900-TOUKEI01-READ-SEC             SECTION.
      *
           READ    TOUKEI01-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-END
               NOT AT  END
                   ADD     1         TO  CNT-TOUKEI01
           END-READ
           .
       900-TOUKEI01-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月次統計データ（診療情報）読込
      *****************************************************************
       900-TOUKEI02-READ-SEC             SECTION.
      *
           READ    TOUKEI02-FILE   NEXT
               AT  END
                   MOVE    1           TO  FLG-TOUKEI02
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOUKEI02
           END-READ
           .
       900-TOUKEI02-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月次統計データ（病名情報）読込
      *****************************************************************
       900-TOUKEI03-READ-SEC             SECTION.
      *
           READ    TOUKEI03-FILE   NEXT
               AT  END
                   MOVE    1           TO  FLG-TOUKEI03
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOUKEI03
           END-READ
           .
       900-TOUKEI03-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
