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
       PROGRAM-ID.             ORCBG032.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 未コード化傷病名使用割合一覧表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  18/03/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    18/03/28  平成３０年４月改正対応
      *  04.08.02    NACL-藤原    18/05/10  施設入所中の処理の修正
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  TOUKEIERR-FILE  ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //TOUKEIERR//.
      *
           COPY    "HCMSL80.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNCOMBI                    PIC 9(01).
           03  FLG-HKNNUM                      PIC 9(01).
           03  FLG-PTHKNINF                    PIC 9(01).
           03  FLG-PTKOHINF                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-PTBYOMEI                    PIC 9(01).
           03  FLG-BYOMEI                      PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-SRYACCT                     PIC 9(01).
           03  FLG-PT-HDIST-DAY                PIC 9(01).
           03  FLG-SYUKA                       PIC 9(01).
           03  FLG-PTNYUINRRK                  PIC 9(01).
           03  FLG-PTNYUINRRK22                PIC 9(01).
           03  FLG-PTNYUINRRK23                PIC 9(01).
      *
           03  FLG-LASTHKNID                   PIC 9(01).
           03  FLG-SEIHO                       PIC 9(01).
           03  FLG-WRITE                       PIC 9(01).
           03  FLG-CHK                         PIC 9(01).
           03  FLG-ERR                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-PAGE                        PIC 9(06).
           03  CNT-LINE                        PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDA                             PIC 9(04).
           03  IDB                             PIC 9(04).
           03  IDX                             PIC 9(04).
           03  IDX1                            PIC 9(04).
           03  IDX2                            PIC 9(04).
           03  IDX3                            PIC 9(04).
           03  IDX4                            PIC 9(04).
           03  IDX5                            PIC 9(04).
           03  IDX6                            PIC 9(04).
           03  IDX7                            PIC 9(04).
           03  IDY                             PIC 9(04).
           03  IDZ                             PIC 9(04).
           03  IDXX                            PIC 9(04).
           03  IDYY                            PIC 9(04).
           03  IDZZ                            PIC 9(04).
           03  IDXD                            PIC 9(04).
           03  IDY2                            PIC 9(04).
           03  IDW                             PIC 9(04).
      *
           03  IDZ4                            PIC 9(04).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
               05  KEY-N-HKNCOMBI              PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
               05  KEY-O-HKNCOMBI              PIC 9(04).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-CNT-PTNUM           PIC 9(08).
           03  SUM-OCC-G                   OCCURS  2.
               05  SUM-OCC                 OCCURS  3.
                   07  SUM-PTBYOMEI    PIC 9(08).
                   07  SUM-PTBYOMEI-NG PIC 9(08).
                   07  SUM-PTBYOMEI-G  PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC X(02).
           03  WRK-PARA-STSRYYM        PIC X(06). 
           03  WRK-PARA-EDSRYYM        PIC X(06).
      *
           03  WRK-TOUKEIERR           PIC X(200). 
           03  WRK-ERRMSG              PIC X(300).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN          PIC X(01).
               05  WRK-TEISEIKBN       PIC X(01).
      *
           03  WRK-DATAKBN             PIC X(01).
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
           03  WRK-SEIHO-FLG           PIC 9(01).
      *
           03  WRK-SRYYM               PIC X(06).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-WARI                PIC 9(3)V9(3).
      *
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM         PIC  9(06).
               05  WRK-SRYDD           PIC  9(02).
      *
           03  ST-IDX                  PIC 9(03).
      *
           03  WRK-RECEIPT-KBN         PIC X(10).
      *
      *    後期高齢者非該当フラグ
           03  WRK-KOUKI-FLG           PIC 9(01).
      *
           03  WRK-TEKSTYMD.
               05  WRK-TEKSTYYMM       PIC 9(06).
               05  WRK-TEKSTDD         PIC 9(02).
      *
           03  WRK-YM              PIC 9(06).
      *
           03  WRK-HKNCOMBINUM-G.
               05  WRK-HKNCOMBINUM PIC 9(04).
      *
      *    レセプト種別判定用領域
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-THH         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-TMM         PIC 9(02).
      *
       01  TOUKEI01-REC. 
           COPY    "CPTCF001.INC".
           03  TOUKEI01-HKNCOMBIINF-AREA.
               05  TOUKEI01-HKNCOMBI-G     OCCURS  10.
                   07  TOUKEI01-HKNNUMHC
                                   PIC X(03).
                   07  TOUKEI01-KOHNUMHCINF    OCCURS  4.
                       09  TOUKEI01-KOHNUMHC
                                   PIC X(03).
                       09  TOUKEI01-KOHIDHC
                                   PIC 9(10).
               03  TOUKEI01-SYOKBN
                                   PIC X(01).
      *診療回数テーブル
           03  RECE61-DAY-AREA.
               05  RECE61-DAY             PIC  9(03)
                                               OCCURS   31.
      *診療回数テーブル（入院基本料を算定していないとき）
           03  RECE61-DAY-1-AREA.
               05  RECE61-DAY-1           PIC  9(03)
                                               OCCURS   31.
      *入院料情報
           03  RECE61-DOUJITSU-AREA.
      *同日再入院（入院分）の保険組合せ
               05  RECE61-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *同日再入院（入院分）の食事開始時間区分
               05  RECE61-DOUJITSU-SKJKBN PIC X(01)   OCCURS  31.
      *入院区分
               05  RECE61-NYUINDAY-TBL.
                   07  RECE61-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *     
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).   
      *
       01  WRK-TOUKEI01-TABLE.
           02  WRK-TOUKEI01-OCC    OCCURS  100.
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //WRK-TOUKEI01//.
           03  WRK-TOUKEI01-HKNCOMBIINF-AREA.
               05  WRK-TOUKEI01-HKNCOMBI-G    OCCURS  10.
                   07  WRK-TOUKEI01-HKNNUMHC
                                   PIC X(03).
                   07  WRK-TOUKEI01-KOHNUMHCINF    OCCURS  4.
                       09  WRK-TOUKEI01-KOHNUMHC
                                   PIC X(03).
                       09  WRK-TOUKEI01-KOHIDHC
                                   PIC 9(10).
               03  WRK-TOUKEI01-SYOKBN
                                   PIC X(01).
      *診療回数テーブル
           03  WRK-RECE61-DAY-AREA.
               05  WRK-RECE61-DAY             PIC  9(03)
                                               OCCURS   31.
      *診療回数テーブル（入院基本料を算定していないとき）
           03  WRK-RECE61-DAY-1-AREA.
               05  WRK-RECE61-DAY-1           PIC  9(03)
                                               OCCURS   31.
      *入院料情報
           03  WRK-RECE61-DOUJITSU-AREA.
      *同日再入院（入院分）の保険組合せ
               05  WRK-RECE61-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *同日再入院（入院分）の食事開始時間区分
               05  WRK-RECE61-DOUJITSU-SKJKBN PIC X(01)   OCCURS  31.
      *入院区分
               05  WRK-RECE61-NYUINDAY-TBL.
                   07  WRK-RECE61-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *     
           02  WRK-TOUKEI01-SEIHO-OCC
                                   OCCURS  100.
               03  WRK-TOUKEI01-SEIHO-KEY
                                   PIC X(50)   OCCURS  10.
       01  WRK-TOUKEI01-TABLE-MAX  PIC 9(03)   VALUE   100.
      *     
       01  WRK-SEIHO-TOUKEI01-REC. 
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //WRK-SEIHO-TOUKEI01//.
           03  WRK-SEIHO-TOUKEI01-HKNCOMBIINF-AREA.
               05  WRK-SEIHO-TOUKEI01-HKNCOMBI-G   OCCURS  10.
                   07  WRK-SEIHO-TOUKEI01-HKNNUMHC
                                   PIC X(03).
                   07  WRK-SEIHO-TOUKEI01-KOHNUMHCINF    OCCURS  4.
                       09  WRK-SEIHO-TOUKEI01-KOHNUMHC
                                   PIC X(03).
                       09  WRK-SEIHO-TOUKEI01-KOHIDHC
                                   PIC 9(10).
       *
      *    ワーク生保まとめ
       01  TBL-SEIHO-TOUKEI01-AREA.
           02  TBL-SEIHO-TOUKEI01-REC  OCCURS  50. 
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //TBL-SEIHO-TOUKEI01//.
           03  TBL-SEIHO-TOUKEI01-HKNCOMBIINF-AREA.
               05  TBL-SEIHO-TOUKEI01-HKNCOMBI-G   OCCURS  10.
                   07  TBL-SEIHO-TOUKEI01-HKNNUMHC
                                   PIC X(03).
                   07  TBL-SEIHO-TOUKEI01-KOHNUMHCINF    OCCURS  4.
                       09  TBL-SEIHO-TOUKEI01-KOHNUMHC
                                   PIC X(03).
                       09  TBL-SEIHO-TOUKEI01-KOHIDHC
                                   PIC 9(10).
           02  TBL-SEIHO-TOUKEI01-OCC  OCCURS  50.
               03  TBL-SEIHO-TOUKEI01-PRTFLG
                                   PIC 9(01).
           02  TBL-SEIHO-TOUKEI01-MAX
                                   PIC 9(03).
      *
       01  WRK-KOH-TOUKEI01-REC. 
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //WRK-KOH-TOUKEI01//.
      *
      *    ワーク患者病名
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO//
                                   BY         //WRK-PTBYO//.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"
                   REPLACING   //SPA-//    BY //WRK-SPA-//.
      *
           COPY    "MCPAREA".
      *
       01  WRK-RECE02-AREA. 
      **** 02  WRK-RECE02-REC      OCCURS  100. 
           COPY    "CPRCF062.INC"  REPLACING  //RECE62//
                                   BY         //WRK-RECE02//.
       01  WRK-RECE631-AREA.
           02  WRK-RECE631-REC      OCCURS  1000. 
           COPY    "CPRCF0631.INC"  REPLACING  //RECE631//
                                   BY         //WRK-RECE631//.
      * 
      *    保険番号マスタ退避ワーク
       01  WRK-HKNNUM-AREA.
           03  WRK-HKNNUM-IDX                  PIC 9(05).
           03  WRK-HKNNUM-MAX                  PIC 9(05).
           03  WRK-CHK-HKNNUM                  PIC X(03).
           03  WRK-CHK-PAYKBN                  PIC X(02).
           03  WRK-CHK-TEKSTYMD                PIC X(08).
           03  WRK-CHK-TEKEDYMD                PIC X(08).
       01  TABLE-HKNNUM-AREA.
         02  TBL-HKNNUM-REC                    OCCURS  2000.
           COPY    "CPHKNNUM.INC"  REPLACING   //HKN-//
                                   BY          //TBL-HKN-//.        
       01  TBL-HKNNUM-CONST-MAX                PIC 9(05)
                                               VALUE   2000.
      *
      *    施設入所情報（９５０、９５１）退避用領域
       01  TABLE-PTKOHINF-AREA.
           02  TBL-PTKOHINF-REC        OCCURS  100.
           COPY    "CPPTKOHINF.INC"
                                   REPLACING   //PTKOH//
                                   BY          //TBL-PTKOH//.
           02  TBL-PTKOHINF-MAX    PIC 9(03).
      *
      *    患者レセプト分割情報退避用領域
       01  TBL-PT-HDIST-DAY-T.
         02  TBL-PT-HDIST-DAY-REC      OCCURS  10.
           COPY    "CPPT-HDIST-DAY.INC"
                                   REPLACING   //PT-HDIST-DAY//
                                   BY          //TBL-PT-HDIST-DAY//.
         02  TBL-PT-HDIST-DAY-MAX  PIC 9(02).
      *
       01  WRK-CONS-AREA.
      *    
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012". 
      *    中国残留邦人等保険番号 
           03  WRK-CONS-ZANRYUU-HKNNUM PIC X(03)   VALUE   "025". 
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    原爆保険番号 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".       
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM  PIC X(03)   VALUE   "975".
      *    特別療養費 
           03  WRK-CONS-TOKU-40-HKNNUM PIC X(03)   VALUE   "040".
      *    公害保険番号 
           03  WRK-CONS-TOKU-68-HKNNUM PIC X(03)   VALUE   "068".
      *    公害保険番号 
           03  WRK-CONS-TOKU-69-HKNNUM PIC X(03)   VALUE   "069".
      *
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".   
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4. 
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".   
      *
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.
           03  WRK-CONS-SRYYM-201004   PIC 9(06)   VALUE   201004.
      *
         01  HEAD-01.
             03  FILLER              PIC X(04)   VALUE   SPACE.
             03  H01-STSRYYM         PIC X(16)   VALUE   SPACE.
             03  FILLER              PIC X(01)   VALUE   SPACE.
             03  H01-FL              PIC X(02)   VALUE   SPACE.
             03  FILLER              PIC X(01)   VALUE   SPACE.
             03  H01-EDSRYYM         PIC X(16)   VALUE   SPACE.
             03  FILLER              PIC X(03)   VALUE   SPACE.
             03  FILLER              PIC X(30)   VALUE
                 "未コード化傷病名使用割合一覧表".
             03  FILLER              PIC X(15)   VALUE   SPACE.
             03  H01-SYSYMD          PIC X(09)   VALUE   SPACE.
             03  FILLER              PIC X(01)   VALUE   SPACE.
             03  FILLER              PIC X(04)   VALUE
                 "作成".
      *
         01  HEAD-02.
             03  FILLER              PIC X(15)   VALUE   SPACE.
             03  FILLER              PIC X(22)   VALUE
                 "診療年月".
             03  FILLER              PIC X(10)   VALUE
                 "入外区分".
             03  FILLER              PIC X(14)   VALUE
                 "傷病名総件数".
             03  FILLER              PIC X(22)   VALUE
                 "未コード化傷病名件数".
             03  FILLER              PIC X(06)   VALUE   SPACE.
             03  FILLER              PIC X(10)   VALUE
                 "割合（％）".
      *
         01  BODY-01.
             05  FILLER              PIC X(15).
             05  B01-SRYYM           PIC X(22).
             05  B01-NYUGAIKBN       PIC X(10).
             05  FILLER              PIC X(02).
             05  B01-CNT             PIC ZZ,ZZZ,ZZ9.
             05  FILLER              PIC X(12).
             05  B01-CNT-NG          PIC ZZ,ZZZ,ZZ9.
             05  FILLER              PIC X(8).
             05  B01-WARIAI          PIC ZZ9.99.
             05  B01-FL              PIC X(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *     
           COPY    "CPSK2005.INC"  REPLACING   //SYS-2005//
                                   BY          //S-25//.
           COPY    "CPSK200501.INC"
                                   REPLACING   //SYS-200501//
                                   BY          //S-2501//.
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者レセプト分割
       01  PT-HDIST-DAY-REC.
           COPY    "CPPT-HDIST-DAY.INC".
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    公費付加情報取得サブ
           COPY    "CPORCSKOHPLUS.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *    患者自己負担金レセプト記載判定
           COPY    "CPORCSFTNJGNCHK.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    レセプト種別返却サブ
           COPY    "CPORCSSYUBETUGET.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    入院料と異なる食事算定保険組み合わせ取得サブ
           COPY    "CPORCSSKJHKN.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SUM-AREA
                                       WRK-TOUKEI01-TABLE
                                       WRK-HKNNUM-AREA
                                       TABLE-HKNNUM-AREA  
                                       SPA-AREA
           MOVE    SPACE           TO  BODY-01
           INITIALIZE                  BODY-01
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       TOUKEIERR
                                       WRK-PARA-STSRYYM
                                       WRK-PARA-EDSRYYM
           END-UNSTRING 
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "STSRYYM=" WRK-PARA-STSRYYM
           DISPLAY "EDSRYYM=" WRK-PARA-EDSRYYM
      *
           IF      WRK-PARA-EDSRYYM    =   SPACE
               MOVE   WRK-PARA-STSRYYM     TO  WRK-PARA-EDSRYYM
           END-IF
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOUKEIERR           TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOUKEIERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBG032"          TO  JOB-PGID
           MOVE    "未コード化傷病名使用割合一覧表"
                                       TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    期間指定のチェック（上限一年間）
           IF      WRK-PARA-STSRYYM    >   WRK-PARA-EDSRYYM
               MOVE    "期間設定に誤りがあります"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE 
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-PARA-STSRYYM    TO  LNK-DAY6-KIJUN (1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    11                  TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               IF      LNK-DAY6-KEISAN(1:6)
                                           <   WRK-PARA-EDSRYYM
                   MOVE    "期間指定は上限一年間までです"
                                              TO  WRK-TOUKEIERR
                  PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
      *    レセプト編集区分情報
           INITIALIZE                      S-25-REC
           MOVE    "2005"              TO  S-25-KANRICD
           MOVE    "*"                 TO  S-25-KBNCD
           MOVE    WRK-PARA-STSRYYM    TO  S-25-STYUKYMD (1:6)
           MOVE    "01"                TO  S-25-STYUKYMD (7:2)
           MOVE    S-25-STYUKYMD       TO  S-25-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  S-25-HOSPNUM
           MOVE    S-25-REC            TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  S-25-REC
           ELSE    
               MOVE    SPACE               TO  S-25-REC
           END-IF
           IF      S-25-RJNGNBKKBN     =   SPACE
               MOVE    ZERO                TO  S-25-RJNGNBKKBN
           END-IF
           IF      S-25-BYOMEIHENKBN1  =   SPACE
               MOVE    ZERO                TO  S-25-BYOMEIHENKBN1
           END-IF
           IF      S-25-TENKIHENKBN    =   SPACE
               MOVE    ZERO                TO  S-25-TENKIHENKBN
           END-IF
      *         
           INITIALIZE                      S-2501-REC
           MOVE    "2005"              TO  S-2501-KANRICD
           MOVE    "01"                TO  S-2501-KBNCD
           MOVE    WRK-PARA-STSRYYM    TO  S-2501-STYUKYMD (1:6)
           MOVE    "01"                TO  S-2501-STYUKYMD (7:2)
           MOVE    S-2501-STYUKYMD     TO  S-2501-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  S-2501-HOSPNUM
           MOVE    S-2501-REC          TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  S-2501-REC
           ELSE    
               INITIALIZE                  S-2501-REC
           END-IF
           IF      S-2501-TENKIBYOMEI-KBN
                                   =   SPACE
               MOVE    ZERO            TO  S-2501-TENKIBYOMEI-KBN
           END-IF
           IF      S-2501-NYUTAIINREKI-NYU-KSIKBN
                                   =   SPACE
               MOVE    "0"             TO
                                       S-2501-NYUTAIINREKI-NYU-KSIKBN
           END-IF
      *
           DISPLAY "BYOMEIHENKBN1    ="
                                    S-25-BYOMEIHENKBN1
           DISPLAY "RJNGNBKKBN       ="
                                    S-25-RJNGNBKKBN
           DISPLAY "TENKIBYOMEI-KBN="
                                    S-2501-TENKIBYOMEI-KBN
           DISPLAY "TENKIHENKBN      ="
                                    S-25-TENKIHENKBN
           DISPLAY "NYUTAIINREKI-NYU-KSIKBN="
                                    S-2501-NYUTAIINREKI-NYU-KSIKBN
      *
      *    期間
           IF      WRK-PARA-STSRYYM    =   WRK-PARA-EDSRYYM
               CONTINUE
           ELSE
               MOVE    WRK-PARA-STSRYYM    TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2)
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (1:16)
                                           TO  H01-STSRYYM
               MOVE   "〜"                 TO  H01-FL
           END-IF
           MOVE    WRK-PARA-EDSRYYM    TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (1:16)
                                       TO  H01-EDSRYYM
      *
      *    作成日取得
           INITIALIZE                      ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1    TO  H01-SYSYMD
      *
           MOVE    SPACE               TO  HCMSL80
      *
           MOVE    HEAD-01             TO  HCMSL80-MOJI (1)
           MOVE    HEAD-02             TO  HCMSL80-MOJI (3)
      *
           MOVE    4                   TO  CNT-LINE
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    WRK-PARA-STSRYYM    TO  WRK-SRYYM
      *
           PERFORM             UNTIL   WRK-SRYYM  >  WRK-PARA-EDSRYYM
               DISPLAY "SRYYM=" WRK-SRYYM
               INITIALIZE              SUM-OCC-G (1)
      *
      *        診療年月
               MOVE    WRK-SRYYM       TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
      *
               MOVE    WRK-SYMD        TO  WRK-STYMD
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY7-AREA
               MOVE    "71"            TO  LNK-DAY7-IRAI
               MOVE    WRK-SRYYM       TO  LNK-DAY7-YM
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *        医療機関情報
               INITIALIZE                  SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
                                               SYS-1001-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
               ELSE
                   MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-TOUKEIERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  200-MAIN-EXT
               END-IF
      *
      *        患者番号構成管理情報
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
               MOVE    WRK-SYMD            TO  SYS-1009-STYUKYMD
                                               SYS-1009-EDYUKYMD 
               MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-HJNKSIKBN  
                                           TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               ELSE
                   MOVE    "患者番号構成管理情報が取得できませんでした"
                                               TO  WRK-TOUKEIERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  200-MAIN-EXT
               END-IF
      *
      *        診療会計読み込み（患者ＩＤ、保険組合せ順）
               MOVE    "2"                 TO  WRK-PARA-NYUGAIKBN
      *
               INITIALIZE                      SRYACCT-REC
               MOVE    WRK-PARA-HOSPNUM    TO  ACCT-HOSPNUM
               MOVE    WRK-SRYYM           TO  ACCT-SRYYM
               MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key36"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key36"             TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               ELSE
                   INITIALIZE                      SRYACCT-REC
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
      *
               PERFORM 2001-NYUINGAI-HENSYU-SEC
                                UNTIL   FLG-SRYACCT     =   1
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key36"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
      *        入院会計読み込み（患者ＩＤ、診療科、剤識別区分順）
               MOVE    "1"                 TO  WRK-PARA-NYUGAIKBN
      *
               INITIALIZE                      NYUINACCT-REC
               MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
               MOVE    WRK-SRYYM           TO  NACCT-SRYYM
               MOVE    WRK-PARA-NYUGAIKBN  TO  NACCT-NYUGAIKBN
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                      NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM 2001-NYUIN-HENSYU-SEC
                                   UNTIL   FLG-NYUINACCT   =   1
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               MOVE    1                   TO  IDX7
               PERFORM 2009-SUM-HENSYU-SEC
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SRYYM           TO  LNK-DAY6-KIJUN (1:6)
               MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  WRK-SRYYM
           END-PERFORM
      *
           MOVE    2                   TO  IDX7
           PERFORM 2009-SUM-HENSYU-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    診療会計編集処理（入院外）
      *****************************************************************
       2001-NYUINGAI-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  WRK-TOUKEI01-TABLE
                                       WRK-RECE02-AREA
                                       WRK-RECE631-AREA
      *
      *    テスト患者のときは読み飛ばす
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    ACCT-PTID           TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SRYACCT PTID=" ACCT-PTID
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
           IF   (  FLG-PTINF           =   ZERO 
               AND  PTINF-TSTPTNUMKBN   =   "1" )
           OR   (  SYUKA-MISEIKYU-FLG  =   "01" )
           OR   (  FLG-PTINF           =   "1"  )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key36"             TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
      *        施設入所情報
               PERFORM 500-SISETU-HENSYU-SEC
      *
      *        患者レセプト分割情報情報
               PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  COMB-HOSPNUM
                   MOVE    KEY-N-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-HKNNUM     =
                                           WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM   OR
                                           WRK-CONS-KOUGAI-HKNNUM  OR
                                           WRK-CONS-TOKU-40-HKNNUM OR
                                           WRK-CONS-TOKU-68-HKNNUM OR
                                           WRK-CONS-TOKU-69-HKNNUM 
                           MOVE    "1"            TO  WRK-TEISEIKBN
                       ELSE     
                           IF      COMB-TEKSTYMD   <=  WRK-STYMD
                           AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                               MOVE    SPACE           TO  WRK-UPDKBN
                           ELSE
                              MOVE    "1"              TO  WRK-UPDKBN
                           END-IF
                       END-IF
                   ELSE
                       MOVE    "1"             TO  WRK-TEISEIKBN
                   END-IF
      *
                   IF      WRK-TEISEIKBN       =   SPACE
                       MOVE    "2"                 TO  WRK-DATAKBN
                       PERFORM 2001-FILE-SYORI-SEC
                   END-IF
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key36"             TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
      *
               IF    ( TBL-PT-HDIST-DAY-MAX
                                           =   ZERO )
               AND   ( TBL-PTKOHINF-MAX    =   ZERO )
                   MOVE    1               TO  ST-IDX
                   MOVE    SPACE           TO  WRK-RECEIPT-KBN
      *            基本ファイル出力処理
                   PERFORM   2002-KIHON-WRITE-SEC
               ELSE
                   MOVE    ZERO            TO  ST-IDX
                   PERFORM         UNTIL   ST-IDX  =   999
                       MOVE    SPACE           TO  WRK-RECEIPT-KBN
                       MOVE    ZERO            TO  ST-IDX
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       100
                               OR      WRK-TOUKEI01-HOSPNUM (IDXX)
                                               =   ZERO
                           IF      WRK-TOUKEI01-SYOKBN (IDXX)
                                                   =   SPACE
                               MOVE    WRK-TOUKEI01-RECEIPT-KBN (IDXX)
                                                   TO  WRK-RECEIPT-KBN
                               MOVE    IDXX    TO  ST-IDX
                               MOVE    100     TO  IDXX
                           END-IF 
                       END-PERFORM
      *
                       IF      ST-IDX          >   ZERO
      *                    基本ファイル出力処理
                           PERFORM   2002-KIHON-WRITE-SEC
                       ELSE
                           MOVE    999             TO  ST-IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       2001-NYUINGAI-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    入院会計編集処理
      *****************************************************************
       2001-NYUIN-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  WRK-TOUKEI01-TABLE
                                       WRK-RECE02-AREA
                                       WRK-RECE631-AREA
      *     
      *    テスト患者のときは読み飛ばす
           MOVE    WRK-PARA-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    NACCT-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "NYUINACCT PTID=" NACCT-PTID
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
                   " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
           IF   (  FLG-PTINF           =   ZERO 
               AND  PTINF-TSTPTNUMKBN   =   "1" )
           OR   (  SYUKA-MISEIKYU-FLG  =   "01" )
               PERFORM     UNTIL   FLG-NYUINACCT   =   1 
                           OR      NACCT-PTID  NOT =   PTINF-PTID
                   MOVE    "tbl_nyuinacct" TO  MCP-TABLE
                   MOVE    "key13"         TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
      *        施設入所情報
               PERFORM 500-SISETU-HENSYU-SEC
      *
      *        患者レセプト分割情報情報
               PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
               INITIALIZE                      WRK-DAY-AREA
                                               WRK-HKNCOMBI-TABLE
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      NACCT-PTID  NOT =   PTINF-PTID
      *            剤識別区分別テーブル編集
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       31
                       IF      NACCT-DAY  (IDX1)   >   ZERO
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                       END-IF
                   END-PERFORM
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
      *        同日再入院のチェック
               PERFORM 500-ORCSNYUINDAY-CHECK-SEC
      *
      *        入院料と異なる食事算定保険組み合わせのチェック
               IF    ( WRK-SRYYM          >=  "201710" )
                   PERFORM 500-ORCSSKJHKN-CHECK-SEC
               END-IF
      *
      *        入院会計の保険組合せをまとめる
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF  ( ( WRK-DAY (1 IDX)     >   ZERO )  OR
                         ( LNK-NYUINDAY-KBN (IDX)
                                               =   "1"  )    )  AND
                       (   WRK-DAY (2 IDX)     >   ZERO      )
                       MOVE    WRK-DAY (2 IDX) TO  WRK-HKNCOMBINUM
                       MOVE    IDX             TO  IDXD
                       PERFORM 20011-NYUINACCT-HENSYU-SEC
                   END-IF
               END-PERFORM        
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   MOVE    WRK-HKNCOMBI-REC (IDX2)
                                               TO  HKNCOMBI-REC
      *
                   MOVE    "1"                 TO  WRK-DATAKBN
                   PERFORM 2001-FILE-SYORI-SEC
               END-PERFORM
      *
      *        診療会計編集処理
               PERFORM 20012-NYUIN-SRYACCT-HENSYU-SEC
      *
               IF    ( TBL-PT-HDIST-DAY-MAX
                                           =   ZERO )
               AND   ( TBL-PTKOHINF-MAX    =   ZERO )
                   MOVE    1               TO  ST-IDX
                   MOVE    SPACE           TO  WRK-RECEIPT-KBN
      *            基本ファイル出力処理
                   PERFORM   2002-KIHON-WRITE-SEC
               ELSE
                   MOVE    ZERO            TO  ST-IDX
                   PERFORM         UNTIL   ST-IDX  =   999
                       MOVE    SPACE           TO  WRK-RECEIPT-KBN
                       MOVE    ZERO            TO  ST-IDX
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       100
                               OR      WRK-TOUKEI01-HOSPNUM (IDXX)
                                               =   ZERO
                           IF      WRK-TOUKEI01-SYOKBN (IDXX)
                                                   =   SPACE
                               MOVE    WRK-TOUKEI01-RECEIPT-KBN (IDXX)
                                                   TO  WRK-RECEIPT-KBN
                               MOVE    IDXX    TO  ST-IDX
                               MOVE    100     TO  IDXX
                           END-IF 
                       END-PERFORM
      *
                       IF      ST-IDX          >   ZERO
      *                    基本ファイル出力処理
                           PERFORM   2002-KIHON-WRITE-SEC
                       ELSE
                           MOVE    999             TO  ST-IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF    
           .
       2001-NYUIN-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       20011-NYUINACCT-HENSYU-SEC           SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    WRK-PARA-HOSPNUM
                                   TO  COMB-HOSPNUM
           MOVE    PTINF-PTID      TO  COMB-PTID
           MOVE    WRK-HKNCOMBINUM TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
           PERFORM 910-HKNCOMBI-INV-SEC
           IF      FLG-HKNCOMBI    =   ZERO
      *        医保以外対象外
               IF      COMB-HKNNUM     =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM   OR
                                           WRK-CONS-KOUGAI-HKNNUM  OR
                                           WRK-CONS-TOKU-40-HKNNUM OR
                                           WRK-CONS-TOKU-68-HKNNUM OR
                                           WRK-CONS-TOKU-69-HKNNUM
                   GO  TO  20011-NYUINACCT-HENSYU-EXT
               END-IF
           ELSE
      *        存在しない場合対象外
               GO  TO  20011-NYUINACCT-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *        保険組合せ追加
               IF      WRK-COMB-HKNCOMBINUM (IDX1)
                                       =   ZERO
                   MOVE    HKNCOMBI-REC    TO  WRK-HKNCOMBI-REC (IDX1)
               END-IF
      *
      *        適用範囲かチェック（範囲外のとき２をセット）
               IF      WRK-HKNCOMBINUM =   WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)
                                       NOT =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDXD            TO  WRK-DD
                       IF      WRK-COMB-TEKSTYMD (IDX1)
                                               <=  WRK-YMD
                       AND     WRK-COMB-TEKEDYMD (IDX1)
                                               >=  WRK-YMD
                           CONTINUE
                       ELSE
                           MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                       END-IF
                   END-IF
                   MOVE    10              TO  IDX1
               END-IF
           END-PERFORM
           .
       20011-NYUINACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    診療会計編集処理（入院）
      *****************************************************************
       20012-NYUIN-SRYACCT-HENSYU-SEC           SECTION.
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
           MOVE    SPACE               TO  WRK-ERR-AREA
           INITIALIZE                      WRK-ERR-AREA
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    WRK-PARA-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    PTINF-PTID          TO  ACCT-PTID
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key37"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT =   1
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               INITIALIZE                  HKNCOMBI-REC
               MOVE    WRK-PARA-HOSPNUM    TO  COMB-HOSPNUM
               MOVE    PTINF-PTID          TO  COMB-PTID
               MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 910-HKNCOMBI-INV-SEC
               IF      FLG-HKNCOMBI    =   ZERO
                   IF      COMB-HKNNUM     =
                                           WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM   OR
                                           WRK-CONS-KOUGAI-HKNNUM  OR
                                           WRK-CONS-TOKU-40-HKNNUM OR
                                           WRK-CONS-TOKU-68-HKNNUM OR
                                           WRK-CONS-TOKU-69-HKNNUM 
                       MOVE    "1"            TO  WRK-TEISEIKBN
                   ELSE     
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                       ELSE
                          MOVE    "1"              TO  WRK-UPDKBN
                       END-IF
                   END-IF
               ELSE
                   MOVE    "1"             TO  WRK-TEISEIKBN
               END-IF
      *
               IF      WRK-TEISEIKBN       =   SPACE
                   MOVE    "2"                 TO  WRK-DATAKBN
                   PERFORM 2001-FILE-SYORI-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key37"             TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       20012-NYUIN-SRYACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       2001-FILE-SYORI-SEC              SECTION.
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "1"
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                       MOVE    "1"         TO  WRK-TEISEIKBN
                   END-IF        
               WHEN    "2"
                   IF      WRK-UPDKBN          =   "1"
      *                保険の訂正がされているか
                       IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                           MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX             >=  WRK-DD
                                   OR      WRK-TEISEIKBN   =   "1"
                               IF      ACCT-DAY (1 IDX)    >   ZERO
                                   MOVE    "1"     TO  WRK-TEISEIKBN
                               END-IF
                           END-PERFORM
                       ELSE
                           IF      COMB-TEKSTYMD(1:6)
                                                   >   WRK-STYMD (1:6)
                               MOVE    "1"     TO  WRK-TEISEIKBN
                           END-IF
                       END-IF      
                       IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                           MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                           ADD     1                   TO  WRK-DD
                           PERFORM VARYING IDX FROM    WRK-DD  BY  1
                                   UNTIL   IDX             >   31
                                   OR      WRK-TEISEIKBN   =   "1"
                               IF      ACCT-DAY (1 IDX)    >   ZERO
                                   MOVE    "1"     TO  WRK-TEISEIKBN
                               END-IF
                           END-PERFORM
                       ELSE
                           IF      COMB-TEKEDYMD(1:6)
                                                   <   WRK-EDYMD (1:6)
                               MOVE    "1"         TO  WRK-TEISEIKBN
                           END-IF    
                       END-IF    
                   END-IF
           END-EVALUATE
      *
           IF      WRK-TEISEIKBN       =   "1"
               GO  TO  2001-FILE-SYORI-EXT
           END-IF
      *
      *    年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    PTINF-HOSPNUM   TO  AGECHK-HOSPNUM
           MOVE    PTINF-PTID      TO  AGECHK-PTID
           MOVE    WRK-SRYYM       TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
      *    老人保険使用のとき
           IF      COMB-KOH1HKNNUM     =   "027"
               GO  TO  2001-FILE-SYORI-EXT
           END-IF
      *   退職国保で６５歳以上のとき
           IF    ( COMB-HKNNUM         =   "067" )
           AND   ( AGECHK-NENREI       >=  65    )
               GO  TO  2001-FILE-SYORI-EXT
           END-IF
      *    政府管掌を使用のとき
           IF      COMB-HKNNUM         =   "001"
               GO  TO  2001-FILE-SYORI-EXT
           END-IF
      *
      *   主保険情報読み出し
           INITIALIZE                      PTHKNINF-REC
           IF      COMB-HKNID          NOT =   ZERO
               INITIALIZE                  PTHKNINF-REC
               MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
               MOVE    COMB-PTID           TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                               PTHKN-TEKEDYMD
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               PERFORM 910-PTHKNINF-INV-SEC
           END-IF
      *
      *    ファイル編集処理
           INITIALIZE                      TOUKEI01-REC
           MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI01-HOSPNUM
           MOVE    WRK-SRYYM           TO  TOUKEI01-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  TOUKEI01-NYUGAIKBN
           MOVE    PTINF-PTID          TO  TOUKEI01-PTID
           MOVE    COMB-HKNNUM         TO  TOUKEI01-HKNNUM
           MOVE    PTHKN-HONKZKKBN     TO  TOUKEI01-HONKZKKBN
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  TOUKEI01-KOHID
           ELSE
               MOVE    ZERO                TO  TOUKEI01-KOHID
               EVALUATE    PTHKN-HKNNUM
                   WHEN    "002" 
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   "A"
                           WHEN   "B"
                           WHEN   "C"
                           WHEN   "9"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           WHEN   "D"
                           WHEN   "E"
                           WHEN   "F"
                           WHEN   "8"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           WHEN   "G"
                           WHEN   "H"
                           WHEN   "I"
                           WHEN   "7"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                       END-EVALUATE
                   WHEN    "031"  THRU    "034"  
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   "B"
                           WHEN   "9"   
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           WHEN   "E"
                           WHEN   "8"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           WHEN   "H"
                           WHEN   "7"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                       END-EVALUATE
                   WHEN    "060" 
                   WHEN    "068" 
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   "1"
                           WHEN   "4"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           WHEN   "2"
                           WHEN   "5"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           WHEN   "3"
                           WHEN   "6"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                       END-EVALUATE
                   WHEN    "040" 
                   WHEN    "039" 
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   "1"
                           WHEN   "3"
                               MOVE    "0"   TO  TOUKEI01-HONKZKKBN
                       END-EVALUATE             
                   WHEN    OTHER 
                       EVALUATE   PTHKN-HOJOKBN
                           WHEN   "9"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           WHEN   "8"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           WHEN   "7"
                               MOVE    "0" TO  TOUKEI01-HONKZKKBN
                       END-EVALUATE
               END-EVALUATE
           END-IF
      *
           MOVE    PTHKN-HKNJANUM      TO  TOUKEI01-HKNJANUM
           IF    ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOH1HKNNUM     =   "020"
                                       OR  "030"
                                       OR  "053" )
               MOVE    COMB-TEKSTYMD       TO  TOUKEI01-HKNJANUM
               MOVE    COMB-KOH1ID         TO  TOUKEI01-KOH-KOHID
           END-IF
      *
           EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       IF    ( WRK-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                       OR    ( AGECHK-NENREI <=  74                    )
                           MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-HOJOKBN
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                               WHEN    "D"
                                   MOVE    "1" TO  TOUKEI01-HOJOKBN
                               WHEN    "B"
                               WHEN    "H"
                               WHEN    "E"
                                   MOVE    "2" TO  TOUKEI01-HOJOKBN
                               WHEN    "C"
                               WHEN    "I"
                               WHEN    "F"
                                   MOVE    "3" TO  TOUKEI01-HOJOKBN
                               WHEN    OTHER
                                   MOVE    PTHKN-HOJOKBN
                                               TO  TOUKEI01-HOJOKBN
                           END-EVALUATE
                       END-IF
                   END-IF
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-HOJOKBN
                   END-IF
               WHEN    "060" 
               WHEN    "068" 
                   EVALUATE    WRK-PARA-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0" TO  TOUKEI01-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1" TO  TOUKEI01-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2" TO  TOUKEI01-HOJOKBN
                               WHEN   "3"
                                   MOVE    "3" TO  TOUKEI01-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1" TO  TOUKEI01-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2" TO  TOUKEI01-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3" TO  TOUKEI01-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    "040" 
               WHEN    "039" 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "1"
                       WHEN   "3"
                           MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-HOJOKBN
                   END-EVALUATE             
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                       WHEN   "7"
                           MOVE    PTHKN-HOJOKBN
                                               TO  TOUKEI01-HOJOKBN
                   END-EVALUATE             
           END-EVALUATE
      *
      *    施設入所の有無
           IF    ( TOUKEI01-RECEIPT-KBN (1:1)
                                           =   SPACE )
           AND   ( TBL-PTKOHINF-MAX        >   ZERO  )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       TBL-PTKOHINF-MAX
                   IF    ( TBL-PTKOH-TEKSTYMD (IDX1)
                                           <=  COMB-TEKSTYMD           )
                   AND   ( COMB-TEKEDYMD   <=  TBL-PTKOH-TEKEDYMD(IDX1))
                       IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "950"
                           MOVE    "1"     TO  TOUKEI01-RECEIPT-KBN
                                                                   (1:1)
                       END-IF
                       IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "951"
                           MOVE    "2"     TO  TOUKEI01-RECEIPT-KBN
                                                                   (1:1)
                       END-IF
                       IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "948"
                           MOVE    "3"     TO  TOUKEI01-RECEIPT-KBN
                                                                   (1:1)
                       END-IF
                       IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "949"
                           MOVE    "4"     TO  TOUKEI01-RECEIPT-KBN
                                                                   (1:1)
                       END-IF
                       MOVE    TBL-PTKOHINF-MAX
                                           TO  IDX1
                   END-IF
               END-PERFORM
           END-IF
      *
      *    患者レセプト分割情報の有無
           IF    ( TOUKEI01-RECEIPT-KBN (2:3)
                                           =   SPACE )
           AND   ( TBL-PT-HDIST-DAY-MAX    >   ZERO  )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       TBL-PT-HDIST-DAY-MAX
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL   IDX4    >       2
                       PERFORM VARYING IDX5    FROM    1   BY  1
                               UNTIL   IDX5    >       10
                               OR      TBL-PT-HDIST-DAY-HKNCOMBI
                                                       (IDX1 IDX4 IDX5)
                                               =   ZERO
                         IF      TBL-PT-HDIST-DAY-DLTFLG
                                                       (IDX1 IDX4 IDX5)
                                                       =   "1"
                           CONTINUE
                         ELSE
                           IF      COMB-HKNCOMBINUM    =
                                   TBL-PT-HDIST-DAY-HKNCOMBI
                                                       (IDX1 IDX4 IDX5)
                               MOVE    TBL-PT-HDIST-DAY-DAY (IDX1)
                                                       TO
                                               TOUKEI01-RECEIPT-KBN(2:2)
                               IF      IDX4            =   1
                                   MOVE    "1"         TO
                                               TOUKEI01-RECEIPT-KBN(4:1)
                               ELSE
                                   MOVE    "2"         TO
                                               TOUKEI01-RECEIPT-KBN(4:1)
                               END-IF
                               IF    ( TBL-PT-HDIST-DAY-SRYYM   (IDX1)
                                           =   COMB-TEKSTYMD (1:6) AND
                                       TBL-PT-HDIST-DAY-DAY (IDX1)
                                           =   COMB-TEKSTYMD (7:2)     )
                               OR    ( TBL-PT-HDIST-DAY-SRYYM   (IDX1)
                                           =   COMB-TEKEDYMD (1:6) AND
                                       TBL-PT-HDIST-DAY-DAY (IDX1)
                                           =   COMB-TEKEDYMD (7:2)     )
                                   MOVE    10              TO  IDX5
                                   MOVE    2               TO  IDX4
                                   MOVE    TBL-PT-HDIST-DAY-MAX
                                                           TO  IDX1   
                               END-IF
                           END-IF
                         END-IF 
                       END-PERFORM
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           IF      WRK-TEISEIKBN       =   "1"
               GO  TO  2001-FILE-SYORI-EXT
           END-IF
      *
           PERFORM VARYING IDXX    FROM   1   BY  1
                   UNTIL   IDXX    >      WRK-TOUKEI01-TABLE-MAX
               IF      WRK-TOUKEI01-HOSPNUM (IDXX) =   ZERO
                   PERFORM 20011-TOUKEI01-WRITE-SEC
                   MOVE    WRK-TOUKEI01-TABLE-MAX
                                              TO  IDXX
               ELSE
                   IF      WRK-TOUKEI01-KEY (IDXX) =
                                               TOUKEI01-KEY
                       PERFORM 20011-TOUKEI01-REWRITE-SEC
                       MOVE    WRK-TOUKEI01-TABLE-MAX
                                                   TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
      *
           INITIALIZE                  WRK-SEIHO-TOUKEI01-REC
      *    公費単独の場合（生保、中国残留邦人等、自立支援）
           IF    ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOHHKNNUM (1)  =   "012"
                                       OR  "015"
                                       OR  "016"
                                       OR  "021"
                                       OR  "025" )
           AND   ( COMB-KOHHKNNUM (2)  =   SPACE )
               PERFORM 20013-SEIHO-TOUKEI01-WRITE-SEC
           END-IF
           .
       2001-FILE-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       20011-TOUKEI01-WRITE-SEC              SECTION.
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
           IF      TOUKEI01-HKNNUM NOT =   SPACE
      *        主保険情報 
               MOVE    PTHKN-HKNID     TO  TOUKEI01-HKNID
                                           TOUKEI01-THKNID     (1)
               MOVE    PTHKN-TEKSTYMD  TO  TOUKEI01-TTEKSTYMD  (1)
               MOVE    PTHKN-TEKEDYMD  TO  TOUKEI01-TTEKEDYMD  (1)
               MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-THOJOKBN   (1)
               IF      PTHKN-HKNNUM    =   "002"
                   IF    ( WRK-SRYYM     <   WRK-CONS-SRYYM-200804 )
                   OR    ( AGECHK-NENREI <=  74                    )  
                       CONTINUE
                   ELSE
                       EVALUATE    PTHKN-HOJOKBN
                           WHEN    "A"
                           WHEN    "G"
                           WHEN    "D"
                               MOVE    "1" TO  TOUKEI01-THOJOKBN (1)
                           WHEN    "B"
                           WHEN    "H"
                           WHEN    "E"
                               MOVE    "2" TO  TOUKEI01-THOJOKBN (1)
                           WHEN    "C"
                           WHEN    "I"
                           WHEN    "F"
                               MOVE    "3" TO  TOUKEI01-THOJOKBN (1)
                       END-EVALUATE
                   END-IF
               END-IF
               MOVE    PTHKN-HONKZKKBN TO  TOUKEI01-THONKZKKBN (1)
      *        法別番号
               INITIALIZE                      HKNNUM-REC
               MOVE    WRK-PARA-HOSPNUM    TO  HKN-HOSPNUM
               MOVE    PTHKN-HKNNUM        TO  HKN-HKNNUM
               MOVE    ZERO                TO  HKN-PAYKBN   
               MOVE    WRK-SYMD            TO  HKN-TEKSTYMD
                                               HKN-TEKEDYMD
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               PERFORM 900-HKNNUM-CHK-SEC
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    HKN-HKNKOHSBTKBN    TO  TOUKEI01-HKNKOHSBTKBN
               END-IF
           END-IF
      *
      *    患者番号
           INITIALIZE                  ORCSPTIDAREA
           MOVE    TOUKEI01-HOSPNUM
                                   TO  SPTID-HOSPNUM
           MOVE    TOUKEI01-PTID   TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM     TO  TOUKEI01-PTNUM
      *
           DISPLAY "PTNUM="  TOUKEI01-PTNUM
      *
      *    年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    TOUKEI01-HOSPNUM
                                   TO  AGECHK-HOSPNUM
           MOVE    TOUKEI01-PTID   TO  AGECHK-PTID
           MOVE    WRK-SRYYM  TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RC       =   ZERO
               MOVE  AGECHK-NENREI     TO  TOUKEI01-AGE
               MOVE  AGECHK-SYUGAKUKBN TO  TOUKEI01-SYUGAKUKBN
           END-IF
      *
           MOVE    COMB-HKNCOMBINUM
                                   TO  TOUKEI01-HKNCOMBI   (1)
           MOVE    COMB-HKNNUM     TO  TOUKEI01-HKNNUMHC   (1)
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       4
               MOVE    COMB-KOHHKNNUM (IDX1)   TO
                                           TOUKEI01-KOHNUMHC (1 IDX1)
               MOVE    COMB-KOHID     (IDX1)   TO
                                           TOUKEI01-KOHIDHC  (1 IDX1)
           END-PERFORM
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "1"
                   PERFORM VARYING IDX3    FROM    1   BY  1
                           UNTIL   IDX3    >       31
                     IF  ( ( WRK-DAY (1 IDX3)      >   ZERO ) OR
                           ( LNK-NYUINDAY-KBN (IDX3)
                                                   =   "1"  ) ) AND
                         (   WRK-DAY (2 IDX3)      >   ZERO   )
                        MOVE   WRK-DAY(2 IDX3)     TO
                                                   RECE61-DAY  (IDX3)
                     END-IF
                     IF    ( WRK-DAY (1 IDX3)      =   ZERO )
                     AND   ( LNK-NYUINDAY-KBN(IDX3)
                                               NOT =   "1"  )
                     AND   ( WRK-DAY (2 IDX3)      >   ZERO )
                        MOVE   WRK-DAY (2 IDX3)    TO
                                                   RECE61-DAY-1 (IDX3)
                     END-IF
                   END-PERFORM
               WHEN    "2"
                   MOVE    ACCT-SRYKA      TO  TOUKEI01-SRYKA  (1)
           END-EVALUATE
      *
           MOVE    TOUKEI01-REC    TO  WRK-TOUKEI01-OCC (IDXX)
           .
       20011-TOUKEI01-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更処理
      *****************************************************************
       20011-TOUKEI01-REWRITE-SEC            SECTION.
      *
           MOVE    WRK-TOUKEI01-OCC (IDXX) TO  TOUKEI01-REC
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
      *    主保険情報 
           IF      TOUKEI01-HKNNUM     NOT =   SPACE
                 MOVE    1                       TO  FLG-LASTHKNID
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       5
                     IF    ( TOUKEI01-TTEKSTYMD (IDX)
                                       =   PTHKN-TEKSTYMD )
                     AND   ( TOUKEI01-TTEKEDYMD (IDX)
                                       =   PTHKN-TEKEDYMD )
                         MOVE    5              TO  IDX
                     ELSE
                         IF      TOUKEI01-TTEKSTYMD (IDX)  =   SPACE
                             MOVE    PTHKN-HKNID
                                           TO  TOUKEI01-THKNID    (IDX)
                             MOVE    PTHKN-TEKSTYMD
                                           TO  TOUKEI01-TTEKSTYMD (IDX)
                             MOVE    PTHKN-TEKEDYMD
                                           TO  TOUKEI01-TTEKEDYMD (IDX)
                             MOVE    PTHKN-HOJOKBN
                                           TO  TOUKEI01-THOJOKBN  (IDX)
                             IF      PTHKN-HKNNUM    =   "002"
                                 IF    ( WRK-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                                 OR    ( AGECHK-NENREI <=  74          )  
                                     CONTINUE
                                 ELSE
                                     EVALUATE    PTHKN-HOJOKBN
                                       WHEN    "A"
                                       WHEN    "G"
                                       WHEN    "D"
                                         MOVE    "1" 
                                           TO  TOUKEI01-THOJOKBN (IDX)
                                       WHEN    "B"
                                       WHEN    "H"
                                       WHEN    "E"
                                         MOVE    "2"
                                           TO  TOUKEI01-THOJOKBN (IDX)
                                       WHEN    "C"
                                       WHEN    "I"
                                       WHEN    "F"
                                         MOVE    "3"
                                           TO  TOUKEI01-THOJOKBN (IDX)
                                     END-EVALUATE
                                 END-IF
                             END-IF
                             MOVE    PTHKN-HONKZKKBN
                                           TO  TOUKEI01-THONKZKKBN(IDX)
                             MOVE    5     TO  IDX
                         ELSE
                             IF      TOUKEI01-TTEKSTYMD (IDX)
                                                   >=  PTHKN-TEKSTYMD
                                 MOVE    ZERO   TO  FLG-LASTHKNID
                             END-IF    
                         END-IF
                     END-IF    
                 END-PERFORM
      *    
                 IF      FLG-LASTHKNID       =   1
                     MOVE    PTHKN-HKNID         TO  TOUKEI01-HKNID
                 END-IF
           END-IF
      *    
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      TOUKEI01-HKNCOMBI    (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       TOUKEI01-HKNCOMBI   (IDX)
                   MOVE    COMB-HKNNUM         TO
                                       TOUKEI01-HKNNUMHC   (IDX)
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       4
                       MOVE    COMB-KOHHKNNUM (IDX1)   TO
                                           TOUKEI01-KOHNUMHC (IDX IDX1)
                       MOVE    COMB-KOHID     (IDX1)   TO
                                           TOUKEI01-KOHIDHC  (IDX IDX1)
                   END-PERFORM
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       TOUKEI01-HKNCOMBI   (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "2"    
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       20
                       IF      TOUKEI01-SRYKA    (IDX) =   SPACE
                           MOVE    ACCT-SRYKA  TO  TOUKEI01-SRYKA (IDX)
                           MOVE    20          TO  IDX
                       ELSE
                           IF      TOUKEI01-SRYKA  (IDX)
                                                       =   ACCT-SRYKA
                               MOVE    20          TO  IDX
                           END-IF
                       END-IF    
                   END-PERFORM
           END-EVALUATE 
      *
           MOVE    TOUKEI01-REC    TO  WRK-TOUKEI01-OCC (IDXX)
           .
       20011-TOUKEI01-REWRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費番号チェック処理
      *****************************************************************
       20012-KOHID-CHECK-SEC              SECTION.
      *
      *    老人保険以外の公費情報を編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   4
                   OR      COMB-KOHID (IDX)    =   ZERO
               IF      COMB-KOHHKNNUM (IDX)    =   "027"
                   CONTINUE
               ELSE
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY         >   10
                       IF      COMB-KOHID (IDX)
                                           =   TOUKEI01-TKOHID (IDY)
                           MOVE    10              TO  IDY
                       ELSE  
                           IF      TOUKEI01-TKOHID (IDY)   =   ZERO
                               PERFORM 200121-KOHID-HENSYU-SEC
                               MOVE    10              TO  IDY
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20012-KOHID-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費番号編集処理
      *****************************************************************
       200121-KOHID-HENSYU-SEC                SECTION.
      *
           MOVE    COMB-KOHID (IDX)    TO  TOUKEI01-TKOHID    (IDY)
           MOVE    COMB-KOHHKNNUM(IDX) TO  TOUKEI01-TKOHHKNNUM(IDY)
    *
           MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    TOUKEI01-PTID       TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDX)    TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTKOHINF-READ-SEC
           IF       FLG-PTKOHINF       =   ZERO
      *        地方公費のときレセプト請求区分を取得
               IF    ( COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8" )
               OR    ( COMB-KOHHKNNUM (IDX)(1:2)
                                           =  "99"             )
                   EVALUATE    COMB-HKNNUM
                       WHEN    "060"
                       WHEN    "067" 
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                                                               OR  "5"
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       WHEN    "039"                
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                                                               OR  "4"
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       WHEN    OTHER                
                           IF      HKN-RECESKYKBN      =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                   END-EVALUATE
                   IF     TOUKEI01-TRECESKYKBN (IDY)   =   1
      *                地方公費レセ記載判定
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    WRK-PARA-NYUGAIKBN
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
                                               SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE       "2"  TO  TOUKEI01-TRECESKYKBN(IDY)
                       END-IF
                   END-IF
               ELSE
      *            全国公費のとき（長期）
                   IF      HKN-HKNKOHSBTKBN    =   "2" 
                       GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            全国公費のとき（生保）
      **>          IF   COMB-KOHHKNNUM (IDX)   =   "012" OR "021"
      *                レセ記載判定
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    WRK-PARA-NYUGAIKBN
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
                                               SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE    "2"     TO  TOUKEI01-TRECESKYKBN(IDY)
                       ELSE
                           MOVE    "1"     TO  TOUKEI01-TRECESKYKBN(IDY)
                       END-IF
      **>              GO  TO  200121-KOHID-HENSYU-EXT
      **>          END-IF
      *            全国公費のとき（老人原爆）
                   IF    ( TOUKEI01-RJNHKNNUM
                                       NOT =   SPACE                   )
                   AND   ( COMB-KOHHKNNUM (IDX)
                                           =   WRK-CONS-GENBAKU-HKNNUM )
                   AND   ( TOUKEI01-TRECESKYKBN(IDY)  =  "1"           )
                       IF      TOUKEI01-HKNNUM =   "060"   OR  "067"
                           IF      S-25-RJNGNBKKBN =   "1" OR  ZERO
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       ELSE                
                           IF      S-25-RJNGNBKKBN =   "2" OR  ZERO
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       END-IF
      **>              GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            全国公費のとき（長期・老人原爆・生保・精神以外）
      **>          MOVE    "1"         TO  TOUKEI01-TRECESKYKBN(IDY)
               END-IF       
           END-IF
           .
       200121-KOHID-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理（生保単独のときのみ）
      *****************************************************************
       20013-SEIHO-TOUKEI01-WRITE-SEC              SECTION.
      *
           MOVE    TOUKEI01-REC        TO  WRK-SEIHO-TOUKEI01-REC
      *
           INITIALIZE                      TOUKEI01-REC
           MOVE    WRK-SEIHO-TOUKEI01-KEY
                                       TO  TOUKEI01-KEY
           MOVE    COMB-TEKSTYMD       TO  TOUKEI01-HKNJANUM
      *    生活保護期間重複対応のため
           IF      COMB-KOHHKNNUM (1)  =   "012"
               MOVE    COMB-KOH1ID         TO  TOUKEI01-KOH-KOHID
           END-IF
      *
           PERFORM VARYING IDXX    FROM   1   BY  1
                   UNTIL   IDXX    >      WRK-TOUKEI01-TABLE-MAX
               IF      WRK-TOUKEI01-HOSPNUM (IDXX) =   ZERO
                   PERFORM 20011-TOUKEI01-WRITE-SEC
                   MOVE    WRK-TOUKEI01-TABLE-MAX
                                              TO  IDXX
               ELSE
                   IF      WRK-TOUKEI01-KEY (IDXX) =
                                               TOUKEI01-KEY
                       PERFORM 20011-TOUKEI01-REWRITE-SEC
                       MOVE    WRK-TOUKEI01-TABLE-MAX
                                                   TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20013-SEIHO-TOUKEI01-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本ファイル出力処理
      *****************************************************************
       2002-KIHON-WRITE-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIHO 
           INITIALIZE                  TBL-SEIHO-TOUKEI01-AREA
                                       WRK-KOH-TOUKEI01-REC
      *
           MOVE    ZERO                TO  WRK-SEIHO-FLG
      *
      *    公費単独の退避（生保、中国残留邦人等、自立支援）と他の
      *    公費単独のチェック（精神入院は除く）    
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-TOUKEI01-TABLE-MAX
                   OR      WRK-TOUKEI01-HOSPNUM (IDXX) =   ZERO
             IF      WRK-RECEIPT-KBN   =
                                       WRK-TOUKEI01-RECEIPT-KBN (IDXX)
               MOVE    WRK-TOUKEI01-OCC (IDXX)     TO  TOUKEI01-REC
      *
      *        公費単独の場合（精神入院・心身喪失・包括以外）
               IF      TOUKEI01-HKNNUM     =   SPACE
                   IF    ( TOUKEI01-TKOHHKNNUM (1) =   "020"   OR
                                                       "030"     )
                   OR    ( TOUKEI01-HKNJANUM       =   "9999999" )
                       CONTINUE
                   ELSE 
                       IF      TOUKEI01-HKNJANUM       =   SPACE
                           IF    ( TOUKEI01-TKOHHKNNUM (1) =
                                                   "012"   OR  "025"
                                               OR  "015"   OR  "016"
                                               OR  "021"            )
                               IF    ( TOUKEI01-TKOHHKNNUM  (2)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (3)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (4)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (5)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (6)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (7)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (8)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (9)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (10)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                                   CONTINUE
                               ELSE
      *****************************IF    ( TOUKEI01-TKOHHKNNUM  (2)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************AND   ( TOUKEI01-TKOHHKNNUM  (3)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************AND   ( TOUKEI01-TKOHHKNNUM  (4)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************    生保、中国残留邦人等、自立支援のみ
      *****************************    の公費単独の場合退避
      *****************************    MOVE    TOUKEI01-REC      TO
      *****************************                WRK-KOH-TOUKEI01-REC
      *****************************ELSE
                                       MOVE    1           TO  FLG-SEIHO
      *****************************END-IF
                               END-IF
                           ELSE
                               MOVE    1           TO  FLG-SEIHO
                           END-IF
                       ELSE
      *                    生保単独の場合退避
                           ADD     1        TO  TBL-SEIHO-TOUKEI01-MAX
                           MOVE    TOUKEI01-REC
                                            TO  TBL-SEIHO-TOUKEI01-REC
                                                (TBL-SEIHO-TOUKEI01-MAX)
                       END-IF
                  END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           IF      FLG-SEIHO           =   1
               INITIALIZE                  TBL-SEIHO-TOUKEI01-AREA
           END-IF    
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-TOUKEI01-TABLE-MAX
                   OR      WRK-TOUKEI01-HOSPNUM (IDXX) =   ZERO
             IF      WRK-RECEIPT-KBN   =
                                       WRK-TOUKEI01-RECEIPT-KBN (IDXX)
               MOVE    WRK-TOUKEI01-OCC (IDXX)     TO  TOUKEI01-REC
      *
               MOVE    ZERO            TO  FLG-WRITE 
      *
      *        公費単独の場合（精神入院・心身喪失・包括以外）
               IF      TOUKEI01-HKNNUM     =   SPACE
                   IF    ( TOUKEI01-TKOHHKNNUM (1) =   "020"   OR
                                                       "030"     )
                   OR    ( TOUKEI01-HKNJANUM       =   "9999999" )
                       CONTINUE
                   ELSE 
                       IF      TOUKEI01-HKNJANUM       =   SPACE
      *                    公費単独の場合（生保、中国残留邦人等、自立支援）
                           IF    ( TOUKEI01-TKOHHKNNUM (1) =
                                                   "012"   OR  "025"
                                               OR  "015"   OR  "016"
                                               OR  "021"            )
      *                        同じ公費の公費単独の場合
                               IF    ( TOUKEI01-TKOHHKNNUM  (2)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (3)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (4)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (5)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (6)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (7)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (8)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (9)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (10)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                                   MOVE    1           TO  FLG-WRITE
                               ELSE
                                   IF      FLG-SEIHO            =   1
                                       CONTINUE
                                   ELSE 
                                      MOVE    1           TO  FLG-WRITE
                                   END-IF
                               END-IF
                           ELSE
                               IF    ( FLG-SEIHO       =   1     )
                               AND   ( TOUKEI01-TKOHHKNNUM (1)
                                                   NOT =   "053" )
                                   CONTINUE
                               ELSE 
                                  MOVE    1           TO  FLG-WRITE
                               END-IF
                           END-IF
                       ELSE
                           MOVE    1                TO  FLG-WRITE 
                       END-IF
                  END-IF
               END-IF
      *
      *        社保で公費併用の場合（生保、中国残留邦人等、自立支援、児童保護）
               IF    ( TOUKEI01-HKNNUM   NOT =   SPACE )
               AND   ( TOUKEI01-TKOHHKNNUM (1)
                                           =   "012"   OR  "025"   OR 
                                               "015"   OR  "016"   OR
                                               "021"   OR  "053"      )
               AND   ( TOUKEI01-TKOHHKNNUM (2)
                                           =   SPACE   OR  "972"   OR
                                               "974"                  )
                   IF      TOUKEI01-HKNNUM   =
                                               "060"   OR  "067"   OR
                                               "068"   OR  "069"   OR
                                               "039"   OR  "040"
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDA FROM   1   BY  1
                               UNTIL   IDA >      TBL-SEIHO-TOUKEI01-MAX
                           PERFORM VARYING IDYY    FROM    1   BY  1
                                   UNTIL   IDYY    >       10
                                   OR      TOUKEI01-TKOHID (IDYY)
                                                   =       ZERO
                               IF      TBL-SEIHO-TOUKEI01-TKOHID(IDA 1)
                                           =   TOUKEI01-TKOHID (IDYY)
                                   MOVE    1           TO
                                       TBL-SEIHO-TOUKEI01-PRTFLG  (IDA)
                                   MOVE    1           TO  WRK-SEIHO-FLG
                                   PERFORM VARYING IDB FROM    1   BY  1
                                           UNTIL   IDB >       10
                                       IF      WRK-TOUKEI01-SEIHO-KEY
                                                           (IDXX IDB)
                                                           =   SPACE 
                                           MOVE
                                             TBL-SEIHO-TOUKEI01-KEY(IDA)
                                                       TO
                                                 WRK-TOUKEI01-SEIHO-KEY
                                                              (IDXX IDB)
                                           MOVE    10  TO  IDB
                                       END-IF
                                   END-PERFORM
                                   PERFORM VARYING IDB FROM    1   BY  1
                                           UNTIL   IDB >       10
                                       IF      TOUKEI01-HKNCOMBI (IDB)
                                                           =   ZERO
                                           MOVE
                                           TBL-SEIHO-TOUKEI01-HKNCOMBI
                                                              (IDA 1)
                                                           TO
                                               TOUKEI01-HKNCOMBI   (IDB)
                                           MOVE
                                           TBL-SEIHO-TOUKEI01-HKNCOMBI-G
                                                              (IDA 1)
                                                           TO
                                               TOUKEI01-HKNCOMBI-G (IDB)
                                       END-IF
                                       IF      TOUKEI01-HKNCOMBI (IDB)
                                                   =
                                         TBL-SEIHO-TOUKEI01-HKNCOMBI
                                                                 (IDA 1)
                                           MOVE    10          TO  IDB
                                       END-IF
                                   END-PERFORM    
                                   MOVE    10          TO  IDYY
                                   MOVE    TBL-SEIHO-TOUKEI01-MAX
                                                       TO  IDA
                               END-IF
                           END-PERFORM      
                       END-PERFORM
                   END-IF                     
               END-IF
      *
               IF      FLG-WRITE            =   ZERO
      *            レセプト種別
                   PERFORM 20021-RECESYUBETU-SET-SEC
      *
                   PERFORM 20023-HENSYU-SEC
               END-IF
      *
               MOVE    "1"              TO  WRK-TOUKEI01-SYOKBN (IDXX) 
             END-IF
           END-PERFORM
      *
      *    公費単独レセの出力処理
           IF    ( WRK-SEIHO-FLG           =   ZERO )
           AND   (  WRK-KOH-TOUKEI01-HOSPNUM
                                           >   ZERO )
      *        主保険とのまとめがなく生保、中国残留邦人等、自立支援以外との
      *        公費単独の場合
               MOVE    WRK-KOH-TOUKEI01-REC  TO  TOUKEI01-REC
      *        レセプト種別
               PERFORM 20021-RECESYUBETU-SET-SEC
      *
               PERFORM 20023-HENSYU-SEC
      *
           ELSE
               IF      TBL-SEIHO-TOUKEI01-MAX    >   ZERO
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   TBL-SEIHO-TOUKEI01-MAX
                       IF      TBL-SEIHO-TOUKEI01-PRTFLG (IDXX)
                                               =   1
                           CONTINUE
                       ELSE
                           MOVE    TBL-SEIHO-TOUKEI01-REC (IDXX)
                                               TO  TOUKEI01-REC
      *                    レセプト種別
                           PERFORM 20021-RECESYUBETU-SET-SEC
      *
                           PERFORM 20023-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *    病名ファイル出力処理
           IF      WRK-TOUKEI01-HOSPNUM (1)    >   ZERO
               PERFORM 20022-BYOMEI-HENSYU-SEC
           END-IF
           .
       2002-KIHON-WRITE-EXT.
           EXIT. 
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       20021-RECESYUBETU-SET-SEC              SECTION.
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      TOUKEI01-TEISEIKBN  =   "1"
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    公費が４つを超える時
           IF      TOUKEI01-TKOHHKNNUM (5)
                                   NOT =   SPACE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
           INITIALIZE                  ORCSSYUBETUGETAREA
           MOVE    1               TO  LNK-SYUBETUGET-SYOFLG
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    WRK-SRYYM  TO  LNK-SYUBETUGET-SRYYM
           MOVE    TOUKEI01-HKNNUM TO  LNK-SYUBETUGET-HKNNUM
           MOVE    1               TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
                   OR      TOUKEI01-TTEKSTYMD (IDX)  =   SPACE
               IF      TOUKEI01-TTEKSTYMD (IDX)
                                       >   TOUKEI01-TTEKSTYMD (IDY)
                   MOVE    IDX             TO  IDY
               END-IF
           END-PERFORM
           MOVE    TOUKEI01-THONKZKKBN (IDY)
                                   TO  LNK-SYUBETUGET-HKN-HONKZKKBN
           MOVE    TOUKEI01-THOJOKBN (IDY)
                                   TO  LNK-SYUBETUGET-HKN-HOJOKBN
           MOVE    TOUKEI01-HKNKOHSBTKBN
                                   TO  LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
           MOVE    TOUKEI01-AGE    TO  LNK-SYUBETUGET-AGE
           MOVE    TOUKEI01-SYUGAKUKBN
                                   TO  LNK-SYUBETUGET-SYUGAKUKBN
      *****MOVE    TOUKEI01-PREFNUM
      *****                        TO  LNK-SYUBETUGET-PREFNUM
           MOVE    TOUKEI01-RJNHKNNUM
                                   TO  LNK-SYUBETUGET-RJNHKNNUM
           MOVE    TOUKEI01-RJNPAYKBN
                                   TO  LNK-SYUBETUGET-RJNPAYKBN
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
               MOVE    TOUKEI01-TKOHHKNNUM (IDX)   TO
                                   LNK-SYUBETUGET-KOHNUM     (IDX)
               MOVE    TOUKEI01-TKOHHBTNUM (IDX)  TO
                                   LNK-SYUBETUGET-KOHHBTNUM  (IDX)
               IF      TOUKEI01-TRECESKYKBN (IDX) =   "1"
                   MOVE    "1"                 TO
                                       LNK-SYUBETUGET-RECESKYKBN (IDX)
               END-IF 
           END-PERFORM
      *
      *    後期高齢非該当の有無設定
           MOVE    ZERO            TO  WRK-KOUKI-FLG
      *
           IF    ( LNK-SYUBETUGET-AGE    >=  70                    )
           AND   ( LNK-SYUBETUGET-SRYYM  >=  WRK-CONS-SRYYM-200804 )
               IF    ( LNK-SYUBETUGET-HKNNUM
                                           =   SPACE   OR  "040"   
                                           OR  "039"   OR  "067"
                                           OR  "069"             )
               OR    ( LNK-SYUBETUGET-HKNNUM (1:2)
                                           =   "98"    OR  "90"
                                           OR  "91"               )
                   CONTINUE
               ELSE
                   IF   (  LNK-SYUBETUGET-HKNNUM
                                               =   "060"   OR  "068"  )
                   OR   (( LNK-SYUBETUGET-HKNNUM
                                           NOT =   "060"   AND "068" )
                           AND
                         ( LNK-SYUBETUGET-HKN-HOJOKBN
                                               =   SPACE             ))
                       MOVE      LNK-SYUBETUGET-SRYYM TO  WRK-TEKSTYYMM
                       IF   TOUKEI01-TTEKSTYMD (IDY)(1:6)  >=
                                                   LNK-SYUBETUGET-SRYYM
                            MOVE  TOUKEI01-TTEKSTYMD (IDY)(7:2)
                                                       TO  WRK-TEKSTDD
                       ELSE
                            MOVE  01                   TO  WRK-TEKSTDD
                       END-IF
      *
                       INITIALIZE                      PTKOHINF-REC
                       MOVE    WRK-PARA-HOSPNUM TO  PTKOH-HOSPNUM
                       MOVE    TOUKEI01-PTID    TO  PTKOH-PTID
                       MOVE    "976"               TO  PTKOH-KOHNUM
                       MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key4"              TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key4"              TO  MCP-PATHNAME
                           PERFORM 900-PTKOHINF-READ-N-SEC
                           PERFORM UNTIL   FLG-PTKOHINF    =   1
                               IF    (PTKOH-TEKSTYMD   <=  WRK-TEKSTYMD)
                               AND   (WRK-TEKSTYMD     <=
                                                       PTKOH-TEKEDYMD  )
                                   MOVE    1           TO  WRK-KOUKI-FLG
                                   MOVE    1           TO  FLG-PTKOHINF
                               ELSE
                                   MOVE    "tbl_ptkohinf"
                                                    TO  MCP-TABLE
                                   MOVE    "key4"   TO  MCP-PATHNAME
                                   PERFORM  900-PTKOHINF-READ-N-SEC
                               END-IF
                           END-PERFORM
                       END-IF
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM  900-CLOSE-SEC
                   END-IF
               END-IF
      *
               IF      WRK-KOUKI-FLG   =   1
                   MOVE    SPACE       TO  LNK-SYUBETUGET-HKN-HOJOKBN
               END-IF
           END-IF
           MOVE    TOUKEI01-BIRTHDAY   TO  LNK-SYUBETUGET-BIRTHDAY
      *
      *    国保のとき９７７の公費の有無
           IF    ( LNK-SYUBETUGET-BIRTHDAY
                                       <   19440402 )
           AND   ( LNK-SYUBETUGET-HKNNUM
                                       =   "060"    )
           AND   ( LNK-SYUBETUGET-SRYYM
                                       >   "201404" )
               MOVE    "99"                TO  LNK-SYUBETUGET-RJNPAYKBN
      *
               MOVE      LNK-SYUBETUGET-SRYYM
                                           TO  WRK-TEKSTYYMM
               IF   TOUKEI01-TTEKSTYMD (IDY)(1:6)
                                           >=  LNK-SYUBETUGET-SRYYM
                    MOVE  TOUKEI01-TTEKSTYMD (IDY)(7:2)
                                               TO  WRK-TEKSTDD
               ELSE
                   MOVE    01                  TO  WRK-TEKSTDD
               END-IF
      *
               INITIALIZE                      PTKOHINF-REC
               MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
               MOVE    TOUKEI01-PTID       TO  PTKOH-PTID
               MOVE    "977"               TO  PTKOH-KOHNUM
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PTKOHINF-READ-N-SEC
                   PERFORM UNTIL   FLG-PTKOHINF    =   1
                       IF    ( PTKOH-TEKSTYMD  <=  WRK-TEKSTYMD   )
                       AND   ( WRK-TEKSTYMD    <=  PTKOH-TEKEDYMD )
                           MOVE    SPACE           TO
                                               LNK-SYUBETUGET-RJNPAYKBN
                           MOVE    1               TO  FLG-PTKOHINF
                       ELSE
                           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                           MOVE    "key4"          TO  MCP-PATHNAME
                           PERFORM  900-PTKOHINF-READ-N-SEC
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
           END-IF
      *
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                       TO  TOUKEI01-RECESYUBETU
      *
      *    DISPLAY "TOUKEI01-RECESYUBETU: " TOUKEI01-RECESYUBETU
           .
        20021-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報編集処理
      *****************************************************************
       20023-HENSYU-SEC              SECTION.
      *
      *     DISPLAY "RECESYUBETU=" TOUKEI01-RECESYUBETU
      *
           IF      TOUKEI01-RECESYUBETU
                                       =   ZERO    OR    6666
                                       OR  8888
               GO  TO  20023-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
                   OR      TOUKEI01-TTEKSTYMD (IDX)  =   SPACE
               IF      WRK-RECE02-TEKSTYMD-X   =   SPACE
                   MOVE    TOUKEI01-TTEKEDYMD (IDX)
                                       TO  WRK-RECE02-TEKEDYMD-X
               ELSE
                   IF      WRK-RECE02-TEKEDYMD-X
                                           <   TOUKEI01-TTEKEDYMD (IDX)
                       MOVE    TOUKEI01-TTEKEDYMD (IDX)  
                                               TO  WRK-RECE02-TEKEDYMD-X
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
                   OR      TOUKEI01-HKNCOMBI (IDX) =   ZERO
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                   IF      WRK-RECE02-HKNCOMBI (IDY)   =   ZERO
                       MOVE    TOUKEI01-HKNCOMBI (IDX) TO
                                               WRK-RECE02-HKNCOMBI (IDY)
                       MOVE    TOUKEI01-HKNNUMHC (IDX)
                                           TO  WRK-RECE02-HKNNUMHC (IDY)
                       PERFORM VARYING IDY2    FROM    1   BY  1
                               UNTIL   IDY2    >       4
                           MOVE TOUKEI01-KOHNUMHC (IDX IDY2) TO
                                           WRK-RECE02-KOHNUMHC(IDY IDY2)
                           MOVE TOUKEI01-KOHIDHC  (IDX IDY2) TO
                                           WRK-RECE02-KOHIDHC (IDY IDY2)
                           IF    ( COMB-HKNNUM        =   SPACE )
                           AND   ( COMB-KOH1HKNNUM    =   "020"
                                                      OR  "030"
                                                      OR  "053" )
                               INITIALIZE                  PTKOHINF-REC
                               MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                               MOVE    TOUKEI01-PTID   TO  PTKOH-PTID
                               MOVE    COMB-KOHID (1)  TO  PTKOH-KOHID
                               MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                               PERFORM 900-PTKOHINF-READ-SEC
                               IF      WRK-RECE02-TEKSTYMD-X   =   SPACE
                                   MOVE    PTKOH-TEKEDYMD
                                               TO  WRK-RECE02-TEKEDYMD-X
                               ELSE
                                   IF      WRK-RECE02-TEKEDYMD-X
                                                   <   PTKOH-TEKEDYMD
                                       MOVE    PTKOH-TEKEDYMD
                                               TO  WRK-RECE02-TEKEDYMD-X
                                   END-IF
                               END-IF
                           END-IF
                       END-PERFORM
                   END-IF
                   IF      TOUKEI01-HKNCOMBI (IDX)     =
                                           WRK-RECE02-HKNCOMBI (IDY)
                       MOVE    10                      TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       20
                   OR      TOUKEI01-SRYKA  (IDX)   =   SPACE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                   IF      WRK-RECE02-SRYKA (IDY)  =   SPACE
                       MOVE    TOUKEI01-SRYKA (IDX)    TO
                                               WRK-RECE02-SRYKA (IDY)
                   END-IF
                   IF      TOUKEI01-SRYKA (IDX)     =
                                           WRK-RECE02-SRYKA (IDY)
                       MOVE    20                      TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    入退院履歴より情報取得
           PERFORM 200231-PTNYUINRRK-SYORI-SEC
      *
           .
        20023-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴より情報取得処理
      *****************************************************************
       200231-PTNYUINRRK-SYORI-SEC                   SECTION.
      *
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    TOUKEI01-PTID       TO  PTNYUINRRK-PTID
           MOVE    WRK-SRYYM           TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    WRK-SRYYM           TO  PTNYUINRRK-TAIINYMD (1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK23
           END-IF
           PERFORM                 UNTIL   FLG-PTNYUINRRK23    =   1
             IF  (   PTNYUINRRK-TENNYUYMD (1:6)
                                         >   WRK-SRYYM         )
             OR  ( ( PTNYUINRRK-TENNYUYMD
                                         >   WRK-RECE02-TEKEDYMD-X )
                  AND
                   ( WRK-RECE02-TEKEDYMD-X NOT =   SPACE           ) )
                 CONTINUE
             ELSE
      *
               IF      PTNYUINRRK-NYUINYMD (1:6)
                                               <   WRK-SRYYM
                   MOVE    1                   TO  IDX4
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX4
               END-IF 
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                               >   WRK-SRYYM
                   MOVE    31                  TO  IDX5
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX5
               END-IF
      *        保険組合せのチェック
      *       （会計に対象の組合せがないときは全て）
               IF    ( RECE61-DAY-AREA     =   ALL "0" )
                   PERFORM 2002311-PTNYUINRRK-HENSYU-SEC
               ELSE
                   PERFORM VARYING IDX FROM    IDX4    BY  1
                           UNTIL   IDX >       IDX5
                       IF    ( IDX             =   IDX4 )
                       AND   ( LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX)
                                               =
                                               PTNYUINRRK-HKNCOMBINUM )
      *                    同日再入院のとき再入院分の保険組合せをチェック
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       10
                                   OR  WRK-RECE02-HKNCOMBI (IDY)
                                                       =   ZERO
                               IF     LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                               (IDX)
                                           =   WRK-RECE02-HKNCOMBI (IDY)
                                   PERFORM 2002311-PTNYUINRRK-HENSYU-SEC
                                   MOVE    10              TO  IDY
                                   MOVE   IDX5             TO  IDX
                               END-IF
                           END-PERFORM
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       10
                                   OR  WRK-RECE02-HKNCOMBI (IDY)
                                               =       ZERO
                               IF      RECE61-DAY (IDX)
                                           =   WRK-RECE02-HKNCOMBI (IDY)
                                   PERFORM 2002311-PTNYUINRRK-HENSYU-SEC
                                   MOVE    10                  TO  IDY
                                   MOVE   IDX5                 TO  IDX
                               END-IF
      *                        入院基本料を算定しない入退院歴の記載
                               IF     S-2501-NYUTAIINREKI-NYU-KSIKBN
                                               =   "1"
                                 IF      RECE61-DAY-1 (IDX)
                                           =   WRK-RECE02-HKNCOMBI (IDY)
                                   PERFORM 2002311-PTNYUINRRK-HENSYU-SEC
                                   MOVE    10                  TO  IDY
                                   MOVE   IDX5                 TO  IDX
                                 END-IF
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-IF
             END-IF
      *
             PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    診療科情報が未編集のとき
           IF      WRK-RECE02-SRYKA (1)    =   SPACE
               MOVE    WRK-RECE02-LASTSRYKA    TO  WRK-RECE02-SRYKA (1)
           END-IF
           .
       200231-PTNYUINRRK-SYORI-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       2002311-PTNYUINRRK-HENSYU-SEC           SECTION.
      *
      *    診療科情報のセット
      *    退院日＜診療年月、転出日の前日＜診療年月のときはセットしない
           IF    ( PTNYUINRRK-TENSTUYMD    =  PTNYUINRRK-TAIINYMD )
           AND   ( PTNYUINRRK-TENNYUYMD    =  PTNYUINRRK-NYUINYMD )
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                           >=  WRK-SRYYM
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       20
                           OR      PTNYUINRRK-NYUINKA
                                           =   WRK-RECE02-SRYKA (IDW)
                       IF      WRK-RECE02-SRYKA (IDW)  =   SPACE  
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       10
                                   OR      WRK-RECE02-HKNCOMBI (IDXX)
                                                   =   ZERO
                               IF      WRK-RECE02-HKNCOMBI (IDXX)  =
                                               PTNYUINRRK-HKNCOMBINUM
                                   MOVE    PTNYUINRRK-NYUINKA
                                               TO  WRK-RECE02-SRYKA(IDW)
                                   MOVE    10      TO  IDXX
                               END-IF
                           END-PERFORM
                           MOVE    20          TO  IDW
                       END-IF
                   END-PERFORM
               END-IF    
           ELSE
               IF      PTNYUINRRK-TENSTUYMD
                                   NOT =   PTNYUINRRK-TAIINYMD
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    PTNYUINRRK-TENSTUYMD
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    -1              TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
                   IF      LNK-DAY6-KEISAN (1:6)
                                       >=  WRK-SRYYM
                     PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       20
                           OR      PTNYUINRRK-NYUINKA
                                           =   WRK-RECE02-SRYKA (IDW)
                       IF      WRK-RECE02-SRYKA (IDW)  =   SPACE  
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       10
                                   OR      WRK-RECE02-HKNCOMBI (IDXX)
                                                   =   ZERO
                               IF      WRK-RECE02-HKNCOMBI (IDXX)  =
                                               PTNYUINRRK-HKNCOMBINUM
                                   MOVE    PTNYUINRRK-NYUINKA  
                                               TO  WRK-RECE02-SRYKA(IDW)
                                   MOVE    10      TO  IDXX
                               END-IF
                           END-PERFORM
                           MOVE    20          TO  IDW
                       END-IF
                     END-PERFORM
                   END-IF
               END-IF
               IF      PTNYUINRRK-TENNYUYMD
                                   NOT =   PTNYUINRRK-NYUINYMD
                   IF      PTNYUINRRK-TENNYUYMD (1:6)
                                       <=  WRK-SRYYM
                     PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       20
                           OR      PTNYUINRRK-NYUINKA
                                           =   WRK-RECE02-SRYKA (IDW)
                       IF      WRK-RECE02-SRYKA (IDW)  =   SPACE  
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       10
                                   OR      WRK-RECE02-HKNCOMBI (IDXX)
                                                   =   ZERO
                               IF      WRK-RECE02-HKNCOMBI (IDXX)  =
                                               PTNYUINRRK-HKNCOMBINUM
                                   MOVE    PTNYUINRRK-NYUINKA  
                                               TO  WRK-RECE02-SRYKA(IDW)
                                   MOVE    10      TO  IDXX
                               END-IF
                           END-PERFORM
                           MOVE    20          TO  IDW
                       END-IF
                     END-PERFORM
                   END-IF
               END-IF
           END-IF
      *
      *    入院年月日・最終科
           IF      WRK-RECE02-LASTSRYKA    =   SPACE
               MOVE    PTNYUINRRK-NYUINKA  TO  WRK-RECE02-LASTSRYKA
           END-IF
           .
       2002311-PTNYUINRRK-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    病名情報出力処理
      *****************************************************************
       20022-BYOMEI-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTBYO-HOSPNUM
           MOVE    PTINF-PTID          TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL       FLG-PTBYOMEI     =   1
               PERFORM 200221-BYOMEI-WRITE-SEC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    件数
           MOVE    ZERO                TO  SUM-CNT-PTNUM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       1000
                   OR      WRK-RECE631-SRYYMD (IDX) =   SPACE
      *
               DISPLAY "SRYYMD=" WRK-RECE631-SRYYMD (IDX)
                             " " WRK-RECE631-BYOMEIHENFLG (IDX)
      *
               ADD     1               TO  SUM-PTBYOMEI (1 3)
                                           SUM-PTBYOMEI (2 3)
                                           CNT-IN
                                           SUM-CNT-PTNUM
               IF      WRK-PARA-NYUGAIKBN  =   "2"
                   ADD     1               TO  SUM-PTBYOMEI (1 1)
                                               SUM-PTBYOMEI (2 1)
               ELSE
                   ADD     1               TO  SUM-PTBYOMEI (1 2)
                                               SUM-PTBYOMEI (2 2)
               END-IF
               IF      WRK-RECE631-BYOMEIHENFLG (IDX)   =   "1"
                   ADD     1               TO  SUM-PTBYOMEI-NG (1 3)
                                               SUM-PTBYOMEI-NG (2 3)
                   IF      WRK-PARA-NYUGAIKBN  =   "2"
                       ADD     1               TO  SUM-PTBYOMEI-NG (1 1)
                       ADD     1               TO  SUM-PTBYOMEI-NG (2 1)
                   ELSE
                       ADD     1               TO  SUM-PTBYOMEI-NG (1 2)
                       ADD     1               TO  SUM-PTBYOMEI-NG (2 2)
                   END-IF
               END-IF
           END-PERFORM
      *
           DISPLAY "CNT=" SUM-CNT-PTNUM
           .
       20022-BYOMEI-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    患者病名出力処理
      *****************************************************************
       200221-BYOMEI-WRITE-SEC         SECTION.
      *
      *    診療科
           MOVE    ZERO                TO  FLG-CHK 
           IF      S-25-BYOMEIHENKBN1  =   ZERO
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                       OR      WRK-RECE02-SRYKA(IDX)   =   SPACE
                   IF      PTBYO-SRYKA      =   WRK-RECE02-SRYKA (IDX)
                       MOVE    1                TO  FLG-CHK
                       MOVE    20               TO  IDX
                   END-IF
               END-PERFORM
               IF      FLG-CHK              =   ZERO
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    入外区分
           IF      PTBYO-NYUGAIKBN      =   SPACE  OR
                                            WRK-PARA-NYUGAIKBN
               CONTINUE
           ELSE
               GO  TO  200221-BYOMEI-WRITE-EXT
           END-IF
      *
      *    保険組合せ
           IF      PTBYO-HKNCOMBI     =   ZERO
               CONTINUE
           ELSE
      ****     第三者行為のみの場合は対象外
      ****     IF      WRK-RECE02-DAISAN-GAITO =   1
      ****         GO  TO  200221-BYOMEI-WRITE-EXT
      ****     END-IF
      *
               MOVE    ZERO            TO  IDZ  
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                       OR      WRK-RECE02-HKNCOMBI (IDX)   =   ZERO
                   IF      PTBYO-HKNCOMBI  =   WRK-RECE02-HKNCOMBI (IDX)
                       MOVE    IDX             TO  IDZ
                       MOVE    10              TO  IDX
                   END-IF
               END-PERFORM
               IF      IDZ            >   ZERO
                   CONTINUE
               ELSE
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    公費ＩＤ（第三者行為）
           IF      PTBYO-KOHID     >   ZERO
               MOVE    ZERO            TO  IDZ
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       10
                       OR      WRK-RECE02-HKNCOMBI (IDX)
                                       =   ZERO 
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       4
                           OR      WRK-RECE02-KOHIDHC  (IDX IDY)
                                           =       ZERO
                       IF    ( WRK-RECE02-KOHNUMHC (IDX IDY)
                                               =  "970"        )
                       AND   ( WRK-RECE02-KOHIDHC  (IDX IDY)
                                               =   PTBYO-KOHID )
                           MOVE    IDX             TO  IDZ
                           MOVE    10              TO  IDX
                           MOVE    4               TO  IDY
                       END-IF
                   END-PERFORM
               END-PERFORM
               IF      IDZ            >   ZERO
                   CONTINUE
               ELSE
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    レセプト表示
           IF      PTBYO-REZEFLG          =   "1"
              GO  TO  200221-BYOMEI-WRITE-EXT
           END-IF
      *
      *    診療開始日
           IF      PTBYO-SRYYMD(1:6)    =   WRK-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    >   WRK-SRYYM
                   GO  TO  200221-BYOMEI-WRITE-EXT
               ELSE
      *            転帰日
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF    ( S-2501-TENKIBYOMEI-KBN
                                                   =   "0"  )
                       OR    ( PTBYO-REZEMM        =   ZERO )
                           IF      PTBYO-TENKIYMD(1:6) >=
                                                       WRK-SRYYM
                               CONTINUE
                           ELSE
                               GO  TO  200221-BYOMEI-WRITE-EXT
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
      *        レセプト表示期間
               MOVE    ZERO                   TO  WRK-YM
               IF      PTBYO-REZEMM           >   ZERO
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    PTBYO-SRYYMD        TO  LNK-DAY6-KIJUN 
                   MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN  =   PTBYO-REZEMM    -   1    
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN(1:6)
                                               TO  WRK-YM
                   IF    ( S-2501-TENKIBYOMEI-KBN
                                               =   "0"   )
                   OR    ( PTBYO-TENKIKBN      =   SPACE )
                       IF      WRK-YM              >=  WRK-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  200221-BYOMEI-WRITE-EXT
                       END-IF
                   END-IF
               END-IF                    
      *
      *        レセプト表示期間と転帰日の優先設定による判定
               IF    ( PTBYO-TENKIKBN      =   SPACE )
               OR    ( PTBYO-REZEMM        =   ZERO  )
                   CONTINUE
               ELSE
                   IF      S-2501-TENKIBYOMEI-KBN
                                               =   "1"
                       IF      WRK-YM              >=  WRK-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  200221-BYOMEI-WRITE-EXT
                       END-IF
                   END-IF    
               END-IF
           END-IF
      *
           IF      PTBYO-BYOMEIHENFLG  =   SPACE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                   IF      PTBYO-BYOMEICD (IDX) (1:4)  =   "ZZZC"
                                                       OR  "ZZZ("
                       MOVE    "1"         TO  PTBYO-BYOMEIHENFLG
                       MOVE    21          TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    廃止年月日が設定されているとき廃止年月日＜診療開始日なら
      *    未傷病名コードにする
      *    保険請求対象外のときは未傷病名コードにする
      *    保険請求対象外の傷病名コードのみのときはそのまま記録する
      *
           IF      PTBYO-BYOMEIHENFLG  =   "1"
                CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      PTBYO-BYOMEICD (IDX)   =   SPACE
                   IF      PTBYO-BYOMEICD(IDX)   =   ALL "9"
                       CONTINUE
                   ELSE  
                       IF      PTBYO-BYOMEICD(IDX)(1:3)
                                                   NOT =   "ZZZ"
                           MOVE    PTBYO-BYOMEICD(IDX)
                                                   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      BYO-HKNSKYKBN   =   1
                               IF      PTBYO-BYOMEICD (2)
                                                   NOT =   SPACE
                                   MOVE    "1" TO  PTBYO-BYOMEIHENFLG
                               END-IF
                               MOVE    21      TO  IDX
                           ELSE
                               IF      BYO-HAISIYMD    NOT =   SPACE
                                   IF      BYO-HAISIYMD    <
                                                       PTBYO-SRYYMD
                                       MOVE    "1"     TO
                                                   PTBYO-BYOMEIHENFLG
                                       MOVE    21      TO  IDX
                                   END-IF 
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      **     DISPLAY "BYOMEI=" PTBYO-SRYYMD " " PTBYO-BYOMEIHENFLG " " 
      **       PTBYO-BYOMEI 
      *
      *    同一開始日・病名・転帰区分でまとめる
           IF      S-25-TENKIHENKBN    =   ZERO
               MOVE    SPACE           TO  PTBYO-TENKIYMD
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       1000
               IF      WRK-RECE631-SRYYMD (IDX) =   SPACE
                   MOVE    PTBYO-SRYYMD    TO  WRK-RECE631-SRYYMD  (IDX)
                   MOVE    PTBYO-BYOMEI    TO  WRK-RECE631-BYOMEI  (IDX)
                   MOVE    PTBYO-HOSOKU-COMMENT
                                           TO
                                       WRK-RECE631-HOSOKU-COMMENT  (IDX)
                   MOVE    PTBYO-TENKIKBN  TO  WRK-RECE631-TENKIKBN(IDX)
                   MOVE    PTBYO-TENKIYMD  TO  WRK-RECE631-TENKIYMD(IDX)
                   MOVE    PTBYO-BYOMEIHENFLG
                                           TO
                                       WRK-RECE631-BYOMEIHENFLG(IDX)
               END-IF
               IF    ( PTBYO-SRYYMD    =   WRK-RECE631-SRYYMD   (IDX) )
               AND   ( PTBYO-BYOMEI    =   WRK-RECE631-BYOMEI   (IDX) )
               AND   ( PTBYO-HOSOKU-COMMENT
                                       =
                                   WRK-RECE631-HOSOKU-COMMENT   (IDX) )
               AND   ( PTBYO-TENKIKBN  =   WRK-RECE631-TENKIKBN (IDX) )
               AND   ( PTBYO-TENKIYMD  =   WRK-RECE631-TENKIYMD (IDX) )
                   MOVE    1000             TO  IDX
               END-IF
           END-PERFORM
           .
       200221-BYOMEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計編集処理
      *****************************************************************
       2009-SUM-HENSYU-SEC                SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       3
               IF      SUM-PTBYOMEI (IDX7 IDX)    =   ZERO
                   MOVE    ZERO            TO  WRK-WARI
               ELSE
                   COMPUTE WRK-WARI    =   SUM-PTBYOMEI-NG(IDX7 IDX)
                                       /   SUM-PTBYOMEI   (IDX7 IDX)
                                       *   100
               END-IF
      *
               MOVE    SPACE           TO  BODY-01
               EVALUATE    IDX
                   WHEN    1
                       IF      IDX7            =   1
                           MOVE    WRK-SRYYM       TO  WRK-SYMD (1:6)
                           MOVE    "01"            TO  WRK-SYMD (7:2)
                           PERFORM 800-SEIWA-HEN-SEC
                           MOVE    LNK-DAY2-EDTYMD3 (1:16)
                                                   TO  B01-SRYYM
                       ELSE
                           MOVE    "総合計"        TO  B01-SRYYM
                       END-IF
                       MOVE    "入院外"        TO  B01-NYUGAIKBN
                   WHEN    2
                       MOVE    SPACE           TO  B01-SRYYM
                       MOVE    "入　院"        TO  B01-NYUGAIKBN
                   WHEN    3
                       MOVE    SPACE           TO  B01-SRYYM
                       MOVE    "合　計"        TO  B01-NYUGAIKBN
               END-EVALUATE
               MOVE    SUM-PTBYOMEI    (IDX7 IDX)
                                           TO  B01-CNT
               MOVE    SUM-PTBYOMEI-NG (IDX7 IDX)
                                           TO  B01-CNT-NG
               MOVE    WRK-WARI            TO  B01-WARIAI
      *
               ADD     1               TO  CNT-LINE
               MOVE    BODY-01         TO  HCMSL80-MOJI (CNT-LINE)
           END-PERFORM
           ADD     1               TO  CNT-LINE
           .
       2009-SUM-HENSYU-EXT.
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
               MOVE    WRK-TOUKEIERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
                                           FLG-ERR
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBG032 "         DELIMITED  BY  SIZE
                   WRK-TOUKEIERR       DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      FLG-ERR         =   ZERO
      *        印刷処理
               PERFORM 3001-LST-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "*** ORCBG032 IN="  CNT-IN
           DISPLAY "*** ORCBG032 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       3001-LST-HENSYU-SEC            SECTION.
      *
           ADD     1                TO  CNT-PAGE
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"            TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                    TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                    TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                    TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                    TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                    TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                    TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                    TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                    TO  SPRT-PRIORITY
           MOVE    "HCMSL80.red"    TO  SPRT-PRTID
           MOVE    "未コード化傷病名使用割合一覧表"
                                    TO  SPRT-TITLE
           MOVE    HCMSL80          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       3001-LST-HENSYU-EXIT.
           EXIT.
      *
      *****************************************************************
      *    施設入所情報処理
      *****************************************************************
       500-SISETU-HENSYU-SEC             SECTION.
      *
           INITIALIZE                      TABLE-PTKOHINF-AREA
      *
           PERFORM VARYING IDZ4    FROM    1   BY  1
                   UNTIL   IDZ4    >       4
               INITIALIZE                      PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    PTINF-PTID          TO  PTKOH-PTID
               EVALUATE    IDZ4
                   WHEN    1
      *                ９５０（老併）
                       MOVE    "950"               TO  PTKOH-KOHNUM
                   WHEN    2
      *                ９５１（老健）
                       MOVE    "951"               TO  PTKOH-KOHNUM
                   WHEN    3
      *                ９４８（医併）
                       MOVE    "948"               TO  PTKOH-KOHNUM
                   WHEN    4
      *                ９４９（医療）
                       MOVE    "949"               TO  PTKOH-KOHNUM
               END-EVALUATE
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PTKOHINF-READ-N-SEC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF  
               END-IF
               PERFORM     UNTIL   FLG-PTKOHINF    =   1
                           OR      TBL-PTKOHINF-MAX
                                                   >=  100
                   IF      PTKOH-SAKJOKBN      =  SPACE
                       ADD     1               TO  TBL-PTKOHINF-MAX
                       MOVE    PTKOHINF-REC    TO  TBL-PTKOHINF-REC
                                                   (TBL-PTKOHINF-MAX)
      *
                       DISPLAY "   TBL-PTKOHINF-MAX=" TBL-PTKOHINF-MAX
                           " KOHNUM="
                                   TBL-PTKOH-KOHNUM   (TBL-PTKOHINF-MAX)
                           " ST="  TBL-PTKOH-TEKSTYMD (TBL-PTKOHINF-MAX)
                           " ED="  TBL-PTKOH-TEKEDYMD (TBL-PTKOHINF-MAX)
      *
                   END-IF
      *
                   MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                   MOVE    "key4"           TO  MCP-PATHNAME
                   PERFORM  900-PTKOHINF-READ-N-SEC
               END-PERFORM
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-PERFORM
           .
       500-SISETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者レセプト分割情報処理
      *****************************************************************
       500-PT-HDIST-DAY-HENSYU-SEC     SECTION.
      *
           INITIALIZE                      TBL-PT-HDIST-DAY-T
      *
           INITIALIZE                      PT-HDIST-DAY-REC
           MOVE    SPA-HOSPNUM         TO  PT-HDIST-DAY-HOSPNUM
           MOVE    PTINF-PTID          TO  PT-HDIST-DAY-PTID
           MOVE    WRK-SRYYM           TO  PT-HDIST-DAY-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  PT-HDIST-DAY-NYUGAIKBN
           MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PT-HDIST-DAY-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PT-HDIST-DAY
           END-IF
           PERFORM     UNTIL   FLG-PT-HDIST-DAY    =   1
                       OR      TBL-PT-HDIST-DAY-MAX
                                                   >=  10
               ADD     1                   TO  TBL-PT-HDIST-DAY-MAX
               MOVE    PT-HDIST-DAY-REC    TO  TBL-PT-HDIST-DAY-REC
                                               (TBL-PT-HDIST-DAY-MAX)
      *
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PT-HDIST-DAY-READ-N-SEC
           END-PERFORM
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       500-PT-HDIST-DAY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院チェック処理
      *****************************************************************
       500-ORCSNYUINDAY-CHECK-SEC          SECTION.
      *
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    WRK-SRYYM           TO  LNK-NYUINDAY-SANTEIYM
           MOVE    PTINF-PTID          TO  LNK-NYUINDAY-PTID
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
      *
           IF      LNK-NYUINDAY-RC     >   0
               DISPLAY "LNK-NYUINDAY-RC=" LNK-NYUINDAY-RC
               GO  TO  500-ORCSNYUINDAY-CHECK-EXT
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       31
               IF      LNK-NYUINDAY-DOUJITSU-KBN (IDXX)
                                               =   "1"
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                       TO  WRK-HKNCOMBINUM
                   MOVE    IDXX        TO  IDXD
                   PERFORM 20011-NYUINACCT-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           .
       500-ORCSNYUINDAY-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院料と異なる食事算定保険組み合わせチェック処理
      *****************************************************************
       500-ORCSSKJHKN-CHECK-SEC            SECTION.
      *
           INITIALIZE                  ORCSSKJHKN-AREA
           MOVE    SPA-HOSPNUM     TO  LNK-SKJHKN-HOSPNUM
           MOVE    PTINF-PTID      TO  LNK-SKJHKN-PTID
           MOVE    WRK-SRYYM       TO  LNK-SKJHKN-SANTEIYM
           CALL    "ORCSSKJHKN"    USING
                                       ORCSSKJHKN-AREA
                                       ORCSNYUINDAY-AREA
                                       SPA-AREA
           IF    ( LNK-SKJHKN-RC   =   ZERO )
               PERFORM VARYING IDXX   FROM    1   BY  1
                       UNTIL ( IDXX   >       31 )
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL ( IDZZ    >   2 )
                       IF    ( LNK-SKJHKN-HKNCOMBINUM  (IDXX IDZZ)
                                   NOT =   SPACE )
                           MOVE    LNK-SKJHKN-HKNCOMBINUM  (IDXX IDZZ)
                                       TO  WRK-HKNCOMBINUM-G
                           MOVE    IDXX    TO  IDXD
                           PERFORM 20011-NYUINACCT-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       500-ORCSNYUINDAY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求フラグチェック処理
      *****************************************************************
       800-MISEIKYU-CHECK-SEC          SECTION.
      *
           INITIALIZE                      SYUKA-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    NACCT-SRYYM         TO  SYUKA-SRYYM
           MOVE    PTINF-PTID          TO  SYUKA-PTID
           MOVE    "1"                 TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"              TO  SYUKA-RECESYUBETU
           MOVE    SYUKA-REC           TO  MCPDATA-REC
           MOVE    "tbl_syuka"         TO  WRK-MCP-TABLE
           MOVE    "key15"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           .
       800-MISEIKYU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE    ACCT-PTID           TO  KEY-N-PTID
           MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           IF      COMB-HKNCOMBINUM =   9999
               MOVE    ZERO                TO  FLG-HKNCOMBI
               GO  TO  910-HKNCOMBI-INV-EXT
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"     TO  MCP-TABLE
               MOVE    "key3"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3 "            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   MOVE    ZERO                TO  FLG-PTKOHINF
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               IF      PTKOH-SAKJOKBN      =   "1"
                   GO  TO  900-PTKOHINF-READ-N-SEC
               END-IF
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           MOVE    HKN-HKNNUM          TO  WRK-CHK-HKNNUM
           MOVE    HKN-PAYKBN          TO  WRK-CHK-PAYKBN
           MOVE    HKN-TEKSTYMD        TO  WRK-CHK-TEKSTYMD
           MOVE    HKN-TEKEDYMD        TO  WRK-CHK-TEKEDYMD
      *
           INITIALIZE                      HKNNUM-REC
      *
      *    保険番号マスタ退避ワークチェック
      *
           PERFORM VARYING WRK-HKNNUM-IDX    FROM    1   BY  1
                   UNTIL   WRK-HKNNUM-IDX    >   WRK-HKNNUM-MAX
                   OR      HKN-HKNNUM        NOT =   SPACE
               IF    ( WRK-CHK-HKNNUM  =
                                       TBL-HKN-HKNNUM (WRK-HKNNUM-IDX) )
               AND   ( WRK-CHK-PAYKBN  =
                                       TBL-HKN-PAYKBN (WRK-HKNNUM-IDX) )
               AND   ( TBL-HKN-TEKSTYMD (WRK-HKNNUM-IDX)
                                       <=  WRK-CHK-TEKSTYMD            )
               AND   ( TBL-HKN-TEKEDYMD(WRK-HKNNUM-IDX)
                                       >=  WRK-CHK-TEKEDYMD            )
                   MOVE    TBL-HKNNUM-REC (WRK-HKNNUM-IDX)
                                       TO  HKNNUM-REC
               END-IF
           END-PERFORM
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF      HKN-HKNNUM          =   SPACE
               INITIALIZE                      HKNNUM-REC
               MOVE    WRK-PARA-HOSPNUM    TO  HKN-HOSPNUM
               MOVE    WRK-CHK-HKNNUM      TO  HKN-HKNNUM
               MOVE    WRK-CHK-PAYKBN      TO  HKN-PAYKBN
               MOVE    WRK-CHK-TEKSTYMD    TO  HKN-TEKSTYMD
               MOVE    WRK-CHK-TEKEDYMD    TO  HKN-TEKEDYMD
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               MOVE    "tbl_hknnum"          TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC                =   ZERO
                   MOVE    "tbl_hknnum"            TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   PERFORM 900-HKNNUM-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
               END-IF
      *
               IF      FLG-HKNNUM          =   ZERO
      *            退避ワークに編集
                   IF      TBL-HKNNUM-CONST-MAX
                                               >   WRK-HKNNUM-MAX
                       COMPUTE WRK-HKNNUM-MAX
                                       =   WRK-HKNNUM-MAX  +   1
                       MOVE    HKNNUM-REC      TO
                                   TBL-HKNNUM-REC (WRK-HKNNUM-MAX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_hknnum"          TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
      *
       900-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号（主キー）読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC                =   ZERO
               MOVE    "tbl_hknnum"            TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタＲＥＡＤ
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC 
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者レセプト分割読込
      *****************************************************************
       900-PT-HDIST-DAY-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PT-HDIST-DAY-REC
               MOVE    ZERO                TO  FLG-PT-HDIST-DAY
           ELSE
               MOVE    1                   TO  FLG-PT-HDIST-DAY
               INITIALIZE                      PT-HDIST-DAY-REC
           END-IF
      *
           .
       900-PT-HDIST-DAY-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-SYUKA
                   MOVE    MCPDATA-REC          TO  SYUKA-REC
               ELSE
                   MOVE    1                    TO  FLG-SYUKA
                   INITIALIZE                       SYUKA-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２２）
      *****************************************************************
       900-PTNYUINRRK-KEY22-READ-SEC    SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key51"              TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK22
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN    =   "5"
                   IF      PTNYUINRRK-TAINRELKBN    =   "1"
                       CONTINUE
                   ELSE
                       GO  TO  900-PTNYUINRRK-KEY22-READ-SEC
                   END-IF
               END-IF
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK22
           END-IF
           .
       900-PTNYUINRRK-KEY22-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２３）
      *****************************************************************
       900-PTNYUINRRK-KEY23-READ-SEC    SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key23"              TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK23
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK23
           END-IF
           .
       900-PTNYUINRRK-KEY23-READ-EXT.
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
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
