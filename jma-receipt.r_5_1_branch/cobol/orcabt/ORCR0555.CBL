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
      *******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCR0555.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災）
      *  コンポーネント名  : レセプト電算ファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/02/07    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-藤原    13/08/20  ０９５９３で始まるコードがあ
      *                                     るレセプトは対象外とする
      *  04.07.02    NACL-門脇    13/10/07  療養期間記載修正
      *  04.07.03    NACL-藤原    14/03/30  平成２６年４月改正対応
      *                                     使用量、単価の小数部の記録方
      *                                     法の変更
      *                                     漢字モードの継続するスペース
      *                                     の記録方法の変更
      *  04.07.04    NACL-門脇    14/09/01  平成２６年１０月改正対応
      *  04.07.05    NACL-門脇    14/10/15  療養期間の記載修正
      *  04.07.06    NACL-藤原    15/02/12  告示等識別区分による記録順を修正
      *                                    （通則加算対象外）
      *  04.07.07    NACL-門脇    15/03/09  症状詳記９０の対応
      *  04.07.08    NACL-門脇    15/03/16  平成２７年４月改正対応
      *  04.07.09    NACL-藤原    15/04/16  注加算コードが１４１９の加算
      *                                     項目の記録方法の修正
      *  04.07.10    NACL-藤原    15/06/10  保険請求対象外傷病名コード対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    14/07/23  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *  04.08.03    NACL-藤原    16/01/15  請求情報への更新日設定
      *  04.08.04    NACL-藤原    16/04/01  平成２８年４月改正対応
      *  04.08.05    NACL-門脇    16/09/20  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *  04.08.06    NACL-藤原    17/03/02  未コード化特定器材コード廃止対応
      *  04.08.07    NACL-藤原    18/04/06  平成３０年４月改正対応
      *  04.08.08    NACL-藤原    18/05/17  平成３０年４月改正対応
      *                                     カタカナ（氏名）、患者状態の追加
      *  04.08.09    NACL-門脇    18/09/27  選択式コメント対応
      *
      *  05.00.01    NACL-藤原    17/07/04  ＨＡＯＲＩ対応
      *  05.00.02    NACL-伊藤    17/12/22  診療情報システム対応
      *  05.00.03    NACL-藤原    19/11/21  診療行為データ用テーブル
      *                                    （加算項目扱いの基本項目）を増やす
      *  05.00.04    NACL-藤原    20/03/25  記録条件仕様変更に伴う対応
      *                                     年月（年月日）の西暦記録 
      *                                     Warning 対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *
           SELECT  RECE43-FILE     ASSIGN  RECE43PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE43-KEY
                                   FILE    STATUS  IS  STS-RECE43.
           SELECT  RECE44-FILE    ASSIGN  RECE44PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE44-KEY
                                   FILE    STATUS  IS  STS-RECE44.
           SELECT  RECE45-FILE     ASSIGN  RECE45PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE45-KEY
                                   FILE    STATUS  IS  STS-RECE45.
           SELECT  RECE455-FILE    ASSIGN  RECE455PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE455-KEY
                                   FILE    STATUS  IS  STS-RECE455.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC. 
           COPY    "CPRCF041.INC".
      *
       FD  RECE43-FILE.
       01  RECE43-REC.
           COPY    "CPRCF043.INC".
           03  RECE43-SYUBYOFLG-X PIC X(01).
      *
       FD  RECE44-FILE.
       01  RECE44-REC.
           COPY    "CPRCF0441.INC" REPLACING  //RECE441//
                                   BY         //RECE44//.
      *
       FD  RECE45-FILE.
       01  RECE45-REC.
           COPY    "CPRCF045.INC".
      *
      *    関連コメントデータ
       FD  RECE455-FILE.
       01  RECE455-REC.
           COPY    "CPRCF0455.INC".
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE43//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE44//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE45//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE455//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE41              PIC X(02).
           03  STS-RECE43              PIC X(02).
           03  STS-RECE44              PIC X(02).
           03  STS-RECE45              PIC X(02).
           03  STS-RECE455             PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECECOM             PIC 9(01).
           03  FLG-OPECOM              PIC 9(01).
      *
           03  FLG-RECE41              PIC 9(01).
           03  FLG-RECE43              PIC 9(01).
           03  FLG-RECE44              PIC 9(01).
           03  FLG-RECE45              PIC 9(01).
           03  FLG-RECE455             PIC 9(01).
      *
           03  FLG-TEIGEN              PIC 9(01).
           03  FLG-YKZ20               PIC 9(01).
           03  FLG-TOTALKIN            PIC 9(01).
           03  FLG-KOKUJISKBKBN1       PIC 9(03).
           03  FLG-SRYKBN70-TEIGEN     PIC 9(01).
           03  FLG-KENSA-KASAN         PIC 9(01).
           03  FLG-KENSA-TEIGEN        PIC 9(01).
           03  FLG-SET                 PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
           03  FLG-COMMENT             PIC 9(01).
           03  FLG-KOUI                PIC 9(01).
           03  FLG-TOYAKUFLG           PIC 9(01).
           03  FLG-RECECOM-XX          PIC 9(01).
           03  FLG-RECECOM-AA          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-RECE04              PIC 9(06).
           03  CNT-COMMIT              PIC 9(03). 
      *
       01  INDEX-AREA.
           03  IDV                     PIC 9(03).
           03  IDW                     PIC 9(03).
           03  IDX                     PIC 9(04). 
           03  IDY                     PIC 9(03). 
           03  IDZ                     PIC 9(03).
           03  IDY1                    PIC 9(04).
           03  IDY2                    PIC 9(04).
           03  IDY3                    PIC 9(03).
           03  IDY4                    PIC 9(03).
           03  IDXX                    PIC 9(03).
           03  IDXY                    PIC 9(03).
           03  IDXZ                    PIC 9(03).
           03  IDW1                    PIC 9(03).
           03  IDW2                    PIC 9(04).
           03  IDW3                    PIC 9(04).
           03  IDW4                    PIC 9(04).
           03  IDW5                    PIC 9(04).
           03  IDZ1                    PIC 9(03).
           03  IDZ2                    PIC 9(03).
           03  IDZ3                    PIC 9(03).
           03  IDZ4                    PIC 9(03).
           03  IDZ5                    PIC 9(03).
           03  IDZ6                    PIC 9(03).
           03  IDYY                    PIC 9(03).
           03  IDZZ                    PIC 9(03).
           03  IDWW                    PIC 9(03).
           03  IDVV                    PIC 9(03).
           03  IDV1                    PIC 9(03).
           03  IDX4                    PIC 9(04). 
           03  IDX5                    PIC 9(04). 
           03  IDX6                    PIC 9(04). 
      *
           03  SYSKAN-IDX              PIC 9(03).
           03  IDX0                    PIC 9(03).
      *
       01  SUM-AREA                                VALUE   ZERO.
           03  SUM-TOTALKIN           PIC 9(10).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  100.
               05  TBL-SYS1001         PIC X(610).
     *
      *    一時領域
       01  WRK-AREA.
      *    パラメタエリア
           03  WRK-PARA.
               05  WRK-PARA1.
                   07  WRK-PARA-SRYYM  PIC X(06).
                   07  WRK-PARA-TERMID PIC X(16).
                   07  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-CREYMD     PIC X(08).
               05  WRK-PARA-CREHMS     PIC X(08).
               05  WRK-PARA-PRTKBN     PIC X(01).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-OPECOM-MCP-TABLE    PIC X(64).
           03  WRK-OPECOM-MCP-PATHNAME PIC X(64).
      *
           03  WRK-KIZAI-SRYCD         PIC X(09).
      *     右づめ編集用     
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-COM-SRYKBN          PIC X(02).
           03  WRK-REC                 PIC X(2500).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NUM             PIC ZZZZZZZZZ9.99999. 
               05  WRK-NUM-R           REDEFINES   WRK-NUM.
                   07  WRK-NUM-R-1     PIC X(10).
                   07  FILLER          PIC X(01).
                   07  WRK-NUM-R-2     PIC X(05).
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
               05  WRK-MOJI-CHK        PIC 9(01).
      *    傷病名レコード編集用
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEICD        PIC X(07).
               05  WRK-RECECD-G.
                   07  WRK-RECECD      PIC X(04)   OCCURS  20.
      *       
           03  WRK-RENNUM              PIC 9(04).
           03  WRK-COM-RENNUM          PIC 9(04).
      *
           03  WRK-RECE44-SRYKBN      PIC X(02).
           03  WRK-COM-FTNKBN          PIC X(01).
           03  WRK-TAIHI-FTNKBN        PIC X(01).
           03  WRK-SJ-KBN              PIC X(02).
           03  WRK-RECECOM-SJKBN       PIC X(02).
           03  WRK-SJ-CHK              PIC 9(01).
      *
           03  WRK-CHUKSNCD            PIC X(04).
      *
      *    事業場の所在地
           03  WRK-PREF-AREA.
               05  WRK-PREFKBN         PIC X(02).     
               05  WRK-CITYKBN         PIC X(02).
      *
      *    摘要レコード編集用
           03  WRK-ZAIWMAX             PIC 9(03).
           03  WRK-TBLMAX1             PIC 9(03).
           03  WRK-TBLMAX6             PIC 9(03).
           03  WRK-TBLMAX7             PIC 9(03).
           03  WRK-TBLMAX7-SRT         PIC 9(03).
           03  WRK-TBLMAX8             PIC 9(03).
           03  WRK-SURYO               PIC 9(08).
           03  WRK-TENSU               PIC 9(07).
           03  WRK-KAISU               PIC 9(03).
           03  WRK-TANKA               PIC 9(08)V9(02).
           03  WRK-SRYSURYO            PIC 9(05)V9(05).
           03  WRK-SRYCD1-MAX          PIC 9(03).           
           03  WRK-SRYCD71-MAX         PIC 9(03).           
           03  WRK-SRYCD72-MAX         PIC 9(03).           
           03  WRK-KOKUJISKBKBN1-KOUI-TEN
                                       PIC 9(09)V9(02). 
           03  WRK-RECE04-ZAIKAISU     PIC 9(04).    
           03  WRK-KENSASU             PIC 9(04).    
      *    時間外加算区分（基本項目）
           03  WRK-KIHON-TIMEKSNKBN    PIC 9(01).
      *    処置の加算項目の有無フラグ
           03  WRK-SRYKBN40-KOKUJISKBKBN1
                                       PIC 9(01).
      *    日付変換用
           03  WRK-SYMD                PIC X(08).
           03  WRK-RYOSTYMD.
               05  FILLER              PIC X(06).
               05  WRK-RYOSTYMD-DD     PIC 9(02).
           03  WRK-RYOEDYMD.
               05  FILLER              PIC X(06).
               05  WRK-RYOEDYMD-DD     PIC 9(02).
      *
           03  WRK-SINRYO-MM.
               05  WRK-SINRYO-MM2      PIC 9(02).
           03  WRK-KYUGYO-MM.
               05  WRK-KYUGYO-MM2      PIC 9(02).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-ACCTP-SYUTEN        PIC 9(07).
      *
           03  WRK-CO-COMMENT          PIC X(76).
           03  WRK-CO-SRYCD            PIC X(09).
      *
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *
           03  WRK-CNT             PIC 9(10).
           03  WRK-CNT-X           REDEFINES   WRK-CNT
                                   PIC X(10).
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT         PIC X(50)
                                           OCCURS  48.
           03  WRK-COMMENT-R           REDEFINES   WRK-COMMENT-G.
               05  WRK-COMMENT1        PIC X(100)
                                           OCCURS  24.
           03  WRK-COMM                PIC X(2400).
           03  WRK-COMM1               PIC X(2400).
           03  WRK-COMM-BACK           PIC X(2400). 
           03  WRK-COL                 PIC 9(03). 
           03  WRK-RECECOM             PIC 9(01).
           03  WRK-RECEHEN             PIC 9(01).
           03  WRK-SJKBN-X.
               05  WRK-SJKBN           PIC 9(02).
      *
      *    手術歴編集
           03  WRK-OPECOM-HEN          PIC X(50).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPRECEDAT1.INC"    REPLACING  //RECEDAT1PARA//
                                       BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG             PIC 9(08).
           03  WRK-HENYMDGR            REDEFINES   WRK-HENYMDG.
               05  FILLER              PIC 9(01).
               05  WRK-HENYMD          PIC 9(07).
      *
      *    関連コメントテーブル
       01  TBL-RECE455-COMMENT-AREA.
           03  TBL-RECE455-OCC         OCCURS  100.
               05  TBL-RECE455-SRYCD   PIC X(09).
               05  TBL-RECE455-NAME    PIC X(200).
           03  TBL-RECE455-MAX         PIC 9(03).
      *
      *    医薬品・特定器材用テーブル（１医薬品 ２特定器材 ３特定器材並び変え用）
       01  TBL-ZAIW-AREA.
           03  TBL-ZAIW-TABLE          OCCURS  3.
               05  TBL-ZAIW-REC        OCCURS 100.
                   07  TBL-ZAIW-KEY.
                       09  TBL-ZAIW-HOSPNUM         PIC 9(2).
                       09  TBL-ZAIW-SRYYM           PIC 9(06).
                       09  TBL-ZAIW-NYUGAIKBN       PIC X(01).
                       09  TBL-ZAIW-PTID            PIC 9(10).
                       09  TBL-ZAIW-RECEKA          PIC X(02).
                       09  TBL-ZAIW-HKNID           PIC 9(10).
                       09  TBL-ZAIW-TEKSTYMD        PIC X(08).
                       09  TBL-ZAIW-ZAINUM          PIC 9(08).
                       09  TBL-ZAIW-RENNUM          PIC 9(02).
                   07  TBL-ZAIW-SHORI-RENNUM        PIC 9(01).
                   07  TBL-ZAIW-SRYACTINF.
      *                診療コード
                       09  TBL-ZAIW-SRYCD           PIC  X(09).
      *                診療種別区分
                       09  TBL-ZAIW-SRYSYUKBN       PIC  X(03).
      *                数量
                       09  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(05).
      *                回数
                       09  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *                単位コード
                       09  TBL-ZAIW-TANICD          PIC  9(03).
      *                単位名称有効桁数
                       09  TBL-ZAIW-TANIMOJISU      PIC  9(01).
      *                単位名称
                       09  TBL-ZAIW-TANINAME        PIC  X(24).
      *                点数識別区分
                       09  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
      *                点数
                       09  TBL-ZAIW-TEN             PIC  9(09)V9(02).
      *                明細請求フラグ
                       09  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *                自動算定区分
                       09  TBL-ZAIW-AUTOKBN         PIC  X(01).
      *                名称入力番号
                       09  TBL-ZAIW-INPUTNUM        PIC  9(03).
      *                点数欄集計先識別
                       09  TBL-ZAIW-TENTTLKBN       PIC  9(03).
      *                データ区分
                       09  TBL-ZAIW-DATAKBN         PIC  9(01).
      *                包括対象検査
                       09  TBL-ZAIW-HOUKNSKBN       PIC  9(02).
      *                検査等実施判断
                       09  TBL-ZAIW-KNSJISKBN       PIC  9(01).
      *                検査等実施判断グループ
                       09  TBL-ZAIW-KNSJISGRPKBN    PIC  9(02).
      *                各種番号等
                       09  TBL-ZAIW-CDKBNINF.
                           11  TBL-ZAIW-CDKBN-KBN   PIC  X(01).
                           11  TBL-ZAIW-CDKBN-SYO   PIC  9(01).
                           11  TBL-ZAIW-CDKBN-BU    PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KBNNUM     PIC  9(03).
                           11  TBL-ZAIW-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KOUBAN     PIC  9(02).
                           11  TBL-ZAIW-KOUHYOJYUNNUM    PIC  9(09).
      *                告示等識別区分１
                       09  TBL-ZAIW-KOKUJISKBKBN1   PIC  9(01).
      *                告示等識別区分２
                       09  TBL-ZAIW-KOKUJISKBKBN2   PIC  9(01).
      *                通則加算対象外区分
                       09  TBL-ZAIW-TUSOKUGAIKBN    PIC  9(01).
      *                時間外加算区分
                       09  TBL-ZAIW-TIMEKSNKBN      PIC  9(01).
      *                きざみ値計算識別
                       09  TBL-ZAIW-KZMCOMPSIKIBETU PIC  9(01).
      *                注加算コード
                       09  TBL-ZAIW-CHUKSNINF.
      *                    コード
                           11  TBL-ZAIW-CHUKSNCD    PIC  X(04).
      *                    通番
                           11  TBL-ZAIW-CHUKSNTUBAN PIC  X(01).
      *                検体検査コメント
                       09  TBL-ZAIW-KENTAICOMMENT   PIC  9(01).
      *                入力コメント
                       09  TBL-ZAIW-INPUTCOMENT     PIC  X(140).
      *            名称情報
                   07  TBL-ZAIW-NAMEINF.
      *                漢字有効桁数
                       09  TBL-ZAIW-YUKOKETA        PIC  9(03).
      *                漢字名称
                       09  TBL-ZAIW-NAME            PIC  X(200).
      *            データ識別(１：フィルム ２：特定機材)
                   07  TBL-ZAIW-DATSKB              PIC  9(01).
      *            特定器材種別１
                   07  TBL-ZAIW-TOKUKIZAISBT1       PIC  9(01).
      *            酸素等区分  
                   07  TBL-ZAIW-SANSOKBN            PIC  9(01).
      *            レセ電用特定器材商品名
                   07  TBL-ZAIW-RECEDENNAME         PIC  X(300).
      *
      *    診療行為・コメント用テーブル
       01  TBL-KOUI-AREA.
           03  TBL-KOUI-REC                     OCCURS  100.
               05  TBL-KOUI-KEY.
                   07  TBL-KOUI-HOSPNUM         PIC 9(2).
                   07  TBL-KOUI-SRYYM           PIC  9(06).
                   07  TBL-KOUI-NYUGAIKBN       PIC  X(01).
                   07  TBL-KOUI-PTID            PIC  9(10).
                   07  TBL-KOUI-RECEKA          PIC  X(02).
                   07  TBL-KOUI-HKNID           PIC  9(10).
                   07  TBL-KOUI-TEKSTYMD        PIC  X(08). 
                   07  TBL-KOUI-ZAINUM          PIC  9(08).
                   07  TBL-KOUI-RENNUM          PIC  9(02).
               05  TBL-KOUI-SHORI-RENNUM        PIC  9(01).
               05  TBL-KOUI-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KOUI-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                   07  TBL-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KOUI-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KOUI-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KOUI-CDKBNINF.
                       09  TBL-KOUI-CDKBN-KBN   PIC  X(01).
                       09  TBL-KOUI-CDKBN-SYO   PIC  9(01).
                       09  TBL-KOUI-CDKBN-BU    PIC  9(02).
                       09  TBL-KOUI-CDKBN-KBNNUM     PIC  9(03).
                       09  TBL-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  TBL-KOUI-CDKBN-KOUBAN     PIC  9(02).
                       09  TBL-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                   07  TBL-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KOUI-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KOUI-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KOUI-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                   07  TBL-KOUI-CHUKSNINF.
      *                コード
                       09  TBL-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KOUI-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KOUI-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KOUI-INPUTCOMENT     PIC  X(140). 
      *        名称情報
               05  TBL-KOUI-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KOUI-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KOUI-NAME            PIC  X(200).
      *
      *    コメントデータ用テーブル
       01  TBL-COMMENT-AREA.
           03  TBL-COMMENT-REC                  OCCURS  100.
               05  TBL-COMMENT-SRYCD            PIC  X(09).
               05  TBL-COMMENT-NAME             PIC  X(200).
      *
      *    診療行為データ用テーブル（通則加算項目の加減算コード）
       01  WRK-KOUI-KASAN-TAIHI-AREA.
           03  WRK-KOUI-KASAN-TAIHI-TABLE       OCCURS  10.
               05  WRK-KOUI-KASAN-TAIHI-REC     PIC X(600).
      *        時間外加算区分
               05  WRK-KOUI-KASAN-TAIHI-TIMEKSNKBN
                                                PIC  9(01).
      *
      *    診療行為データ用テーブル（告示等識別区分による記録順）
       01  WRK-KOUI-TAIHI-AREA.
           03  WRK-KOUI-TAIHI-TABLE             OCCURS  100.
               05  WRK-KOUI-TAIHI-REC           PIC X(600).
      *
      *    診療行為データ用テーブル（告示等識別区分による記録順）
       01  WRK-KOUI-SRY-TAIHI-AREA.
           03  WRK-KOUI-SRY-TAIHI-TABLE             OCCURS  100.
               05  WRK-KOUI-SRY-TAIHI-REC           PIC X(600).
      *        診療コード
               05  WRK-KOUI-SRY-TAIHI-SRYCD         PIC  X(09).
      *        告示等識別区分１
               05  WRK-KOUI-SRY-TAIHI-KOKUJISKB1    PIC  9(01).
      *        告示等識別区分２
               05  WRK-KOUI-SRY-TAIHI-KOKUJISKB2    PIC  9(01).
      *        通則加算対象外区分
               05  WRK-KOUI-SRY-TAIHI-TUSOKUGAIKBN  PIC  9(01).
      *        時間外加算区分
               05  WRK-KOUI-SRY-TAIHI-TIMEKSNKBN    PIC  9(01).
      *        注加算コード
               05  WRK-KOUI-SRY-TAIHI-CHUKSNCD      PIC  X(04).
      *        点数識別区分
               05  WRK-KOUI-SRY-TAIHI-TENSIKIBETU   PIC  9(01).
      *
      *   
       01  WRK-KOUI-AREA.
           03  WRK-KOUI-TABLE                   OCCURS  6.
               05  WRK-KOUI-TBLMAX              PIC 9(03).
               05  WRK-KOUI-REC                     OCCURS  100.
                   07  WRK-KOUI-KEY.
                       09  WRK-KOUI-HOSPNUM        PIC 9(2).
                       09  WRK-KOUI-SRYYM          PIC 9(06).
                       09  WRK-KOUI-NYUGAIKBN      PIC X(01).
                       09  WRK-KOUI-PTID           PIC 9(10).
                       09  WRK-KOUI-RECEKA         PIC X(02).
                       09  WRK-KOUI-HKNID          PIC 9(10).
                       09  WRK-KOUI-TEKSTYMD       PIC X(08). 
                       09  WRK-KOUI-ZAINUM         PIC 9(08).
                       09  WRK-KOUI-RENNUM         PIC 9(02).
                   07  WRK-KOUI-SHORI-RENNUM       PIC 9(01).
      *
                   07  WRK-KOUI-SRYACTINF.
      *            診療コード
                       09  WRK-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                       09  WRK-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                       09  WRK-KOUI-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                       09  WRK-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                       09  WRK-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                       09  WRK-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                       09  WRK-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                       09  WRK-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                       09  WRK-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                       09  WRK-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                       09  WRK-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                       09  WRK-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                       09  WRK-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                       09  WRK-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                       09  WRK-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                       09  WRK-KOUI-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                       09  WRK-KOUI-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                       09  WRK-KOUI-CDKBNINF.
                           11  WRK-KOUI-CDKBN-KBN   PIC  X(01).
                           11  WRK-KOUI-CDKBN-SYO   PIC  9(01).
                           11  WRK-KOUI-CDKBN-BU    PIC  9(02).
                           11  WRK-KOUI-CDKBN-KBNNUM     PIC  9(03).
                           11  WRK-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  WRK-KOUI-CDKBN-KOUBAN     PIC  9(02).
                           11  WRK-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                       09  WRK-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                       09  WRK-KOUI-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                       09  WRK-KOUI-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                       09  WRK-KOUI-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                       09  WRK-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                       09  WRK-KOUI-CHUKSNINF.
      *                コード
                           11  WRK-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                           11  WRK-KOUI-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                       09  WRK-KOUI-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                       09  WRK-KOUI-INPUTCOMENT     PIC  X(140). 
      *        名称情報
                   07  WRK-KOUI-NAMEINF.
      *            漢字有効桁数
                       09  WRK-KOUI-YUKOKETA        PIC  9(03).
      *            漢字名称
                       09  WRK-KOUI-NAME            PIC  X(200).
      *
      *    並び替え用ワーク
       01  TBL-KOUI-SORT                        PIC X(600).
      *
      *    診療行為データ用テーブル（加算項目扱いの基本項目）
       01  TBL-KOUI-SRYKBN12-AREA.
           03  TBL-KOUI-SRYKBN12-MAX                 PIC 9(02).
      *
           03  TBL-KOUI-SRYKBN12-REC                 OCCURS  50.
               05  TBL-KOUI-SRYKBN12-KEY.
                   07  TBL-KOUI-SRYKBN12-HOSPNUM     PIC 9(2).
                   07  TBL-KOUI-SRYKBN12-SRYYM       PIC 9(06).
                   07  TBL-KOUI-SRYKBN12-NYUGAIKBN   PIC X(01).
                   07  TBL-KOUI-SRYKBN12-PTID        PIC 9(10).
                   07  TBL-KOUI-SRYKBN12-RECEKA      PIC X(02).
                   07  TBL-KOUI-SRYKBN12-HKNID       PIC 9(10).
                   07  TBL-KOUI-SRYKBN12-TEKSTYMD    PIC X(08). 
                   07  TBL-KOUI-SRYKBN12-ZAINUM      PIC 9(08).
                   07  TBL-KOUI-SRYKBN12-RENNUM      PIC 9(02).
               05  TBL-KOUI-SRYKBN12-SHORI-RENNUM    PIC 9(01).
               05  TBL-KOUI-SRYKBN12-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYKBN12-SRYCD PIC X(09).
      *            回数
                   07  TBL-KOUI-SRYKBN12-KAISU PIC 9(04).
      *            点数
                   07  TBL-KOUI-SRYKBN12-TEN   PIC 9(09)V9(02).
      *            注加算コード
                   07  TBL-KOUI-SRYKBN12-CHUKSNINF.
      *            コード
                       09  TBL-KOUI-SRYKBN12-CHUKSNCD
                                               PIC X(04).
      *            通番
                       09  TBL-KOUI-SRYKBN12-CHUKSNTUBAN
                                               PIC X(01).
      *    点数合計
           03  TBL-KOUI-SRYKBN12-TEN-G         PIC 9(09)V9(02).
      *
      *    診療行為データ用テーブル（加算項目扱いの基本項目と同じ注換算）
       01  TBL-KOUI-SRYKBN12-CHU-AREA.
           03  TBL-KOUI-SRYKBN12-CHU-MAX                 PIC 9(02).
      *
           03  TBL-KOUI-SRYKBN12-CHU-REC                 OCCURS  5.
               05  TBL-KOUI-SRYKBN12-CHU-KEY.
                   07  TBL-KOUI-SRYKBN12-CHU-HOSPNUM     PIC 9(2).
                   07  TBL-KOUI-SRYKBN12-CHU-SRYYM       PIC 9(06).
                   07  TBL-KOUI-SRYKBN12-CHU-NYUGAIKBN   PIC X(01).
                   07  TBL-KOUI-SRYKBN12-CHU-PTID        PIC 9(10).
                   07  TBL-KOUI-SRYKBN12-CHU-RECEKA      PIC X(02).
                   07  TBL-KOUI-SRYKBN12-CHU-HKNID       PIC 9(10).
                   07  TBL-KOUI-SRYKBN12-CHU-TEKSTYMD    PIC X(08).
                   07  TBL-KOUI-SRYKBN12-CHU-ZAINUM      PIC 9(08).
                   07  TBL-KOUI-SRYKBN12-CHU-RENNUM      PIC 9(02).
               05  TBL-KOUI-SRYKBN12-CHU-SHORI-NUM       PIC 9(01).
               05  TBL-KOUI-SRYKBN12-CHU-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYKBN12-CHU-SRYCD PIC X(09).
      *            回数
                   07  TBL-KOUI-SRYKBN12-CHU-KAISU PIC 9(04).
      *            点数
                   07  TBL-KOUI-SRYKBN12-CHU-TEN   PIC 9(09)V9(02).
      *            注加算コード
                   07  TBL-KOUI-SRYKBN12-CHU-CHUKSNINF.
      *            コード
                       09  TBL-KOUI-SRYKBN12-CHU-CHUKSNCD
                                               PIC X(04).
      *            通番
                       09  TBL-KOUI-SRYKBN12-CHU-CHUKSNTU
                                               PIC X(01).
      *    点数合計
           03  TBL-KOUI-SRYKBN12-CHU-TEN-G     PIC 9(09)V9(02).
      *
       01  WRK-RECE455-REC.
           COPY    "CPRCF0455.INC" REPLACING  //RECE455//
                                   BY         //WRK-RECE455//.
      *
       01  WRK-RECE45-REC.
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //WRK-RECE45//.
      *
       01  WRK-TAIHI-RECE44-REC.
           COPY    "CPRCF0441.INC"  REPLACING //RECE441//
                                   BY         //WRK-TAIHI4//.
      *
      *    画像診断用退避エリア
      *    撮影料退避
       01  FLG-SRYSYUKBN721        PIC 9(01).
       01  WRK-721-RECE45-REC.
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //WRK-721-RECE45//.
       01  WRK-700-RECE45-AREA.
           02  WRK-SRYSYUKBN700-MAX           PIC 9(01).
           02  FLG-SRYSYUKBN700               PIC 9(01)  OCCURS  5.
           02  WRK-700-RECE45-REC                        OCCURS  5.
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //WRK-700-RECE45//.
      *    撮影料退避（同時撮影）
       01  FLG-SRYSYUKBN721-DOUJI             PIC 9(01).
       01  WRK-721-DOUJI-RECE45-REC.
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //WRK-721-DOU45//.
       01  WRK-700-DOUJI-RECE45-AREA.
           02  WRK-SRYSYUKBN700-DOUJI-MAX     PIC 9(01).
           02  FLG-SRYSYUKBN700-DOUJI         PIC 9(01)  OCCURS  5.
           02  WRK-700-DOUJI-RECE45-REC                  OCCURS  5.
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //WRK-700-DOU45//.
      *    間接撮影の５０％逓減
       01  WRK-SRYKBN70-RECE45-REC.
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //WRK-SRY70-RE45//.
      *    生体検査・加算用退避エリア
       01  WRK-KASAN-RECE45-REC.
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //WRK-KASAN-RE45//.
      *    生体検査・逓減用退避エリア
       01  WRK-TEIGEN-RECE45-REC.
           COPY    "CPRCF045.INC"  REPLACING  //RECE45//
                                   BY         //WRK-TEIGE-RE45//.
      *
      *    検査（判断料）用退避エリア
       01  TBL-KENSA-RECE44-AREA.
         02  TBL-KENSA-RECE44-REC  OCCURS  100.
      *
           COPY    "CPRCF0441.INC"  REPLACING  //RECE441//
                                    BY          //TBL-KENSA4//.
      *
           03  TBL-KENSA-RECE45-REC        OCCURS  50.
               05  TBL-KENSA-RECE45-KEY.
                   07  TBL-KENSA-RECE45-HOSPNUM        PIC 9(2).
                   07  TBL-KENSA-RECE45-SRYYM          PIC 9(06).
                   07  TBL-KENSA-RECE45-NYUGAIKBN      PIC X(01).
                   07  TBL-KENSA-RECE45-PTID           PIC 9(10).
                   07  TBL-KENSA-RECE45-RECEKA         PIC X(02).
                   07  TBL-KENSA-RECE45-HKNID          PIC 9(10).
                   07  TBL-KENSA-RECE45-TEKSTYMD       PIC X(08). 
                   07  TBL-KENSA-RECE45-ZAINUM         PIC 9(08).
                   07  TBL-KENSA-RECE45-RENNUM         PIC 9(02).
              05  TBL-KENSA-RECE45-SHORI-RENNUM        PIC 9(01).
      *
              05  TBL-KENSA-RECE45-SRYACTINF.
      *            診療コード
                   07  TBL-KENSA-RECE45-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KENSA-RECE45-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KENSA-RECE45-SRYSURYO      PIC  9(05)V9(05).
      *            回数
                   07  TBL-KENSA-RECE45-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KENSA-RECE45-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KENSA-RECE45-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KENSA-RECE45-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KENSA-RECE45-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KENSA-RECE45-TEN            PIC 9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KENSA-RECE45-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KENSA-RECE45-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KENSA-RECE45-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KENSA-RECE45-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KENSA-RECE45-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KENSA-RECE45-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KENSA-RECE45-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KENSA-RECE45-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KENSA-RECE45-CDKBNINF.
                       09  TBL-KENSA-RECE45-CDKBN-KBN   PIC  X(01).
                       09  TBL-KENSA-RECE45-CDKBN-SYO   PIC  9(01).
                       09  TBL-KENSA-RECE45-CDKBN-BU    PIC  9(02).
                       09  TBL-KENSA-RECE45-CDKBN-KBNNUM     PIC 9(03).
                       09  TBL-KENSA-RECE45-CDKBN-EDA        PIC 9(02).
                       09  TBL-KENSA-RECE45-CDKBN-KOUBAN     PIC 9(02).
                       09  TBL-KENSA-RECE45-KOUHYOJYUNNUM    PIC 9(09).
      *            告示等識別区分１
                   07  TBL-KENSA-RECE45-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KENSA-RECE45-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KENSA-RECE45-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KENSA-RECE45-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KENSA-RECE45-KZMCOMPSKB      PIC  9(01).
      *            注加算コード
                   07  TBL-KENSA-RECE45-CHUKSNINF.
      *                コード
                       09  TBL-KENSA-RECE45-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KENSA-RECE45-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KENSA-RECE45-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KENSA-RECE45-INPUTCOMENT     PIC  X(140). 
      *        名称情報
               05  TBL-KENSA-RECE45-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KENSA-RECE45-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KENSA-RECE45-NAME            PIC  X(200).
      *
           03  TBL-KENSA-SRYCD-MAX
                                  PIC 9(02).
      *
         02  TBL-KENSA-RECE44-MAX PIC 9(03).
         02  TBL-KENSA-RECE45-MAX PIC 9(03).
      *
       01  WRK-CONS-AREA.
      *    検査日　日
           03  WRK-CONS-COMMENT        PIC X(09)   VALUE   "840000099".
      *    カンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
      *    薬剤料逓減
           03  WRK-CONS-TEIGEN        PIC X(09)   VALUE   "630010002".
      *    薬剤料逓減（８０／１００）（向精神薬多剤投与）
           03  WRK-CONS-TEIGEN2       PIC X(09)   VALUE   "630010005".
      *    薬剤料逓減（６０／１００）（紹介率が低い大病院３０日以上投薬）
           03  WRK-CONS-TEIGEN3       PIC X(09)   VALUE   "630010006".
      *    薬剤料減点（湿布薬薬剤料上限超）
           03  WRK-CONS-TEIGEN4       PIC X(09)   VALUE   "630010007".
      *
      *    再診の時間加算
           03  WRK-CONS-JIKANGAI-TABLE. 
      *    一般
      *        時間外（再診）
               05  FILLER             PIC X(09)    VALUE   "112001110".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112001210".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112001310".
      *        時間外特例（再診）
               05  FILLER             PIC X(09)    VALUE   "112001410".
      *        時間外（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006470".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006570".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006670".
      *        時間外特例（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112006770".
      *    老人
      *        時間外（再診）
               05  FILLER             PIC X(09)    VALUE   "112701010".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112701110".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112701210".
      *        時間外特例（再診）
               05  FILLER             PIC X(09)    VALUE   "112701310".
      *        時間外（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701710".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701810".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112701910".
      *        時間外特例（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112702010".
      *    乳幼児
      *        夜間（再診）
               05  FILLER             PIC X(09)    VALUE   "112014170".
      *        休日（再診）
               05  FILLER             PIC X(09)    VALUE   "112014270".
      *        深夜（再診）
               05  FILLER             PIC X(09)    VALUE   "112014370".
      *        夜間（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014470".
      *        休日（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014570".
      *        深夜（外来診療科）
               05  FILLER             PIC X(09)    VALUE   "112014670".
      *        再診（乳幼児時間外）加算
               05  FILLER             PIC X(09)    VALUE   "112014770".
      *        再診（乳幼児休日）加算
               05  FILLER             PIC X(09)    VALUE   "112014870".
      *        再診（乳幼児深夜）加算
               05  FILLER             PIC X(09)    VALUE   "112014970".
      *        再診（乳幼児時間外特例医療機関）加算
               05  FILLER             PIC X(09)    VALUE   "112015070".
      *        外来診療科（乳幼児時間外）加算
               05  FILLER             PIC X(09)    VALUE   "112015170".
      *        外来診療科（乳幼児休日）加算
               05  FILLER             PIC X(09)    VALUE   "112015270".
      *        外来診療科（乳幼児深夜）加算
               05  FILLER             PIC X(09)    VALUE   "112015370".
      *        外来診療科（乳幼児時間外特例医療機関）加算
               05  FILLER             PIC X(09)    VALUE   "112015470".
      *
      *        再診（夜間・早朝等）加算
               05  FILLER             PIC X(09)    VALUE   "112015570".
      *
      *    妊婦
      *        妊婦時間外加算（再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112021370".
      *        妊婦休日加算（再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112021470".
      *        妊婦深夜加算（再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112021570".
      *        妊婦時間外特例医療機関加算（再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112021670".
      *        妊婦夜間加算（産科又は産婦人科再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022170".
      *        妊婦休日加算（産科又は産婦人科再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022270".
      *        妊婦深夜加算（産科又は産婦人科再診）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022370".
      *        妊婦時間外加算（外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022470".
      *        妊婦休日加算（外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022570".
      *        妊婦深夜加算（外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022670".
      *        妊婦時間外特例医療機関加算（外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022770".
      *        妊婦夜間加算（産科又は産婦人科外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112022970".
      *        妊婦休日加算（産科又は産婦人科外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112023070".
      *        妊婦深夜加算（産科又は産婦人科外来診療料）（入院外）
               05  FILLER             PIC X(09)    VALUE   "112023170".
      *
      *        薬剤適正使用連携加算（再診）
               05  FILLER             PIC X(09)    VALUE   "112021970".
      *
           03  WRK-CONS-JIKANGAI-TABLE-R  REDEFINES   
                                      WRK-CONS-JIKANGAI-TABLE.
               05  WRK-CONS-JIKANGAI-SRYCD-G       OCCURS  46.
                   07  WRK-CONS-JIKANGAI-SRYCD
                                      PIC X(09).
           03  WRK-CONS-JIKANGAI-TEN-TABLE.
               05  WRK-CONS-JIKANGAI-TEN-G         OCCURS  46.
                   07  WRK-CONS-JIKANGAI-TEN
                                      PIC 9(09).
           03  WRK-CONS-JIKANGAI-MAX  PIC 9(02)    VALUE   46.
      *
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）
           03  WRK-CONS-SANSO1        PIC X(09)   VALUE   "739210000".
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）（離島）
           03  WRK-CONS-SANSO2        PIC X(09)   VALUE   "739250000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）
           03  WRK-CONS-SANSO3        PIC X(09)   VALUE   "739200000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）（離島）
           03  WRK-CONS-SANSO4        PIC X(09)   VALUE   "739240000".
      *    酸素ボンベ・大型
           03  WRK-CONS-SANSO5        PIC X(09)   VALUE   "739220000".
      *    酸素ボンベ・大型（離島）
           03  WRK-CONS-SANSO6        PIC X(09)   VALUE   "739260000".
      *    酸素ボンベ・小型
           03  WRK-CONS-SANSO7        PIC X(09)   VALUE   "739230000".
      *    酸素ボンベ・小型（離島）
           03  WRK-CONS-SANSO8        PIC X(09)   VALUE   "739270000".
      *
      *    間接撮影の５０％逓減
           03  WRK-CONS-SRYKBN70-TEIGEN
                                      PIC X(09)   VALUE   "170020570".
      *
      *    施設基準不適合（検査）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-KENSA
                                      PIC X(09)   VALUE   "160174470".
      *
      *    施設基準不適合（画像）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-GAZOU
                                      PIC X(09)   VALUE   "170025170".
      *
      *    施設基準不適合（放射線）（１００分の７０）
           03  WRK-CONS-SISETUKIJYUN-HOUSYASEN
                                      PIC X(09)   VALUE   "180025270".
      *
      *    酸素補正率１．３（１気圧）
           03  WRK-CONS-SANSO-HOSEI   PIC X(09)   VALUE   "770020070".
      *
      *    デジタル撮影とアナログ撮影の同時撮影
           03  WRK-CONS-DOUJI-SATUEI-AREA.
      *        単純撮影（デジタルと同時撮影）（アナログ撮影）
               05  FILLER              PIC X(09)   VALUE   "170032050".
      *        単純撮影（アナログと同時撮影）（デジタル撮影）
               05  FILLER              PIC X(09)   VALUE   "170032150".
      *        単純撮影（デジタルと同時撮影）（アナログ撮影）
               05  FILLER              PIC X(09)   VALUE   "170032450".
      *        単純撮影（アナログと同時撮影）（デジタル撮影）
               05  FILLER              PIC X(09)   VALUE   "170032550".
      *        造影剤使用撮影（アナログ撮影）
               05  FILLER              PIC X(09)   VALUE   "170032250".
      *        造影剤使用撮影（デジタル撮影）
               05  FILLER              PIC X(09)   VALUE   "170032350".
      *        造影剤使用撮影（アナログ撮影）
               05  FILLER              PIC X(09)   VALUE   "170032650".
      *        造影剤使用撮影（デジタル撮影）
               05  FILLER              PIC X(09)   VALUE   "170032750".
           03  WRK-CONS-DOUJI-SATIEI-R     REDEFINES
                                       WRK-CONS-DOUJI-SATUEI-AREA.
               05  WRK-CONS-DOUJI-SATUEI
                                       PIC X(09)   OCCURS  8.
      *
      *    出力対象のコメント
           03  WRK-CONS-COMMENT-G.
      *        複数診療科受診
               05  WRK-CONS-COMMENT1  PIC X(09)   VALUE   "099999901".
      *        他保険にて診察料算定済
               05  WRK-CONS-COMMENT2  PIC X(09)   VALUE   "099999902".
      *********（粉砕）
      *********05  WRK-CONS-COMMENT3  PIC X(09)   VALUE   "099209901".
      *       （残量廃棄）
               05  WRK-CONS-COMMENT4  PIC X(09)   VALUE   "099309901".
      *    レセプトコメント
           03  WRK-CONS-RECECOM-MAX
                                   PIC 9(02)   VALUE   48.
      *    レセプトコメント長さ
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
      *
      *    改行コード
       01  WRK-CONS-RETURN          PIC X(01)   VALUE   X"0A".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK2005.INC".
      *
           COPY    "CPSK2009.INC".
      *
      *    医療機関情報
           COPY   "CPSK1001.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    レセプトコメントマスタ
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    手術歴コメント
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    レセ器材再編集サブ
           COPY    "CPORCSRECESORT.INC"
                                   REPLACING  //-IN-KEY-TEISYUTUSAKI//
                                   BY         //-IN-KEY-TEISYUTU//.
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    拡張漢字チェックサブ
           COPY    "CPORCSKANACHK2.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *    PUSH
           COPY    "CPPUSHBATCH01.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
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
                                       RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA1
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-CREYMD
                                               WRK-PARA-CREHMS
                                               WRK-PARA-PRTKBN
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "レセ電データ作成"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCR0555"      TO  JOB-PGID
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 1001-SYSKANRI-HENSYU-SEC
      *
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    "RECE439"           TO  RECE43PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE43PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE43PARA-HOSPNUM
      *
           MOVE    "RECE44F"           TO  RECE44PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE44PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE44PARA-HOSPNUM
      *
           MOVE    "RECE45F"           TO  RECE45PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE45PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE45PARA-HOSPNUM
      *
           MOVE    "RECE456"           TO  RECE455PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE455PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE455PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE41PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE43PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE44PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE45PARA-BASENAME TO  SGETTEMP-BASENAMES  (5)
           MOVE    RECE455PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE41PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE43PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE44PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE45PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE455PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    I-O                 RECE41-FILE  
           IF      STS-RECE41          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE41PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE41          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE43-FILE  
           IF      STS-RECE43          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE43PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE43          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE44-FILE  
           IF      STS-RECE44          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE44PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE44          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE45-FILE  
           IF      STS-RECE45          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE45PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE45          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE455-FILE
           IF      STS-RECE455         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE455PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE455         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      * 
      *    ファイル読み込み
           PERFORM 900-RECE41-READ-SEC
      *
      *    時間外加算点数取得
           INITIALIZE                  WRK-CONS-JIKANGAI-TEN-TABLE
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-JIKANGAI-MAX
               INITIALIZE                  TNS-TENSU-REC
               MOVE    WRK-CONS-JIKANGAI-SRYCD (IDX)
                                       TO  TNS-SRYCD
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TEN         TO  WRK-CONS-JIKANGAI-TEN (IDX)
           END-PERFORM
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム管理マスタ取得      
      *****************************************************************
       1001-SYSKANRI-HENSYU-SEC     SECTION.
      *
           IF      WRK-PARA-SRYYM  NOT =   ZERO
      *        医療機関ＩＤ編集
               INITIALIZE                  SYS-1001-REC
               MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD (1:6)
               MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
               MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
               ELSE
                   MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           ELSE
               MOVE     ZERO               TO  SYSKAN-IDX
               MOVE     SPACE              TO  TBL-SYS1001-T
               INITIALIZE                      TBL-SYS1001-T
      *
               INITIALIZE                      SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
               MOVE    SYS-1001-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
                   PERFORM UNTIL     ( FLG-SYSKANRI    =    1  )
                            OR       ( SYSKAN-IDX      >=  100 )
                       MOVE    MCPDATA-REC         TO  SYS-1001-REC
                       ADD     1                   TO  SYSKAN-IDX
                       MOVE    SYS-1001-REC        TO  TBL-SYS1001
                                                           (SYSKAN-IDX)
      *
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               IF      SYSKAN-IDX          =   ZERO
                   MOVE    "医療機関情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *         
      *    レセプト編集区分情報
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                      SYS-2005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE
               INITIALIZE                      SYS-2005-REC
           END-IF
           IF      SYS-2005-SYUBYOHENFLG
                                       =   SPACE
               MOVE    ZERO                TO  SYS-2005-SYUBYOHENFLG
           END-IF
           IF      SYS-2005-RECECOMKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RECECOMKBN
           END-IF
      *    
           MOVE    "0"                 TO  SYS-2005-RECECOMKBN
           .
       1001-SYSKANRI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    療養期間編集処理
           PERFORM 2001-SRYYM-HENSYU-SEC
      *
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  FLG-TOYAKUFLG
      *
           MOVE    ZERO                TO  WRK-COM-RENNUM
                                           FLG-COMMENT
           MOVE    SPACE               TO  WRK-COM-SRYKBN
      *
           INITIALIZE                      WRK-RECE44-SRYKBN
                                           TBL-KOUI-SRYKBN12-AREA
                                           TBL-KOUI-SRYKBN12-CHU-AREA
                                           TBL-KENSA-RECE44-AREA
      *
      *    合計金額の退避
           MOVE    ZERO                TO  FLG-TOTALKIN
           COMPUTE SUM-TOTALKIN    =   RECE41-TOTALKIN
                                   +   RECE41-TOTALKIN2
      *         
      *    レセプト共通レコード
           PERFORM 2002-HENSYU-SEC
      *    労災レセプトレコード
           PERFORM 2003-HENSYU-SEC
      *    傷病名レコード
           PERFORM 2006-HENSYU-SEC
      *    労災医科診療行為レコード
           PERFORM 2007-HENSYU-SEC
      *    症状詳記（レセコメント）
           MOVE    ZERO        TO  FLG-TOYAKUFLG
           PERFORM 2009-COMMENT-HEN-99-SEC
      *
           ADD     1                   TO  CNT-COMMIT
           IF      CNT-COMMIT          =   300
               MOVE    ZERO                TO  CNT-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               PERFORM 900-DBSTART-SEC
           END-IF
      *
           PERFORM     900-RECE41-READ-SEC
           .
       200-MAIN-EXT.
           EXIT.             
      *
      *****************************************************************
      *    療養期間編集処理      
      *****************************************************************
       2001-SRYYM-HENSYU-SEC           SECTION.
      *     
      *    療養期間開始
           MOVE    RECE41-SRYYM        TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
      *     
      *    療養期間終了
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE41-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"               USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
           . 
       2001-SRYYM-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    レセプト共通レコード編集処理
      *****************************************************************
       2002-HENSYU-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "RE,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    レセプト番号
           PERFORM 5005-NUM-HENSYU-SEC
      *    予備１
           PERFORM 5004-MOJI-HENSYU-SEC
      *    予備２
           PERFORM 5004-MOJI-HENSYU-SEC
      *    氏名
           INITIALIZE                      ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    "1"                 TO  KANACHK2-SYORIKBN 
           MOVE    RECE41-NAME         TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"      USING   ORCSKANACHK2AREA
           IF      KANACHK2-RC4        =   ZERO
               MOVE    SPACE               TO  RECE41-NAME
               MOVE    KANACHK2-OUT-INPUT (1:KANACHK2-MAX)
                                           TO  RECE41-NAME
      *
               IF      RECE41-NAME (1:2)   =   "　"
                   IF      RECE41-KANANAME (1:2)   =   "　"
                       PERFORM VARYING IDX FROM    1   BY  2
                               UNTIL   IDX >       49
                           IF      RECE41-KANANAME (IDX:2) =   "　"
                               CONTINUE
                           ELSE     
                               MOVE    RECE41-KANANAME (IDX:)
                                                           TO  WRK-DATA
                               MOVE    49                  TO  IDX
                           END-IF
                       END-PERFORM 
                   ELSE
                       MOVE    RECE41-KANANAME     TO  WRK-DATA
                   END-IF
               ELSE
                   MOVE    RECE41-NAME         TO  WRK-DATA
               END-IF
           ELSE
               IF      RECE41-KANANAME (1:2)   =   "　"
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       49
                       IF      RECE41-KANANAME (IDX:2)   =   "　"
                           CONTINUE
                       ELSE     
                           MOVE    RECE41-KANANAME (IDX:) TO  WRK-DATA
                           MOVE    49                     TO  IDX
                       END-IF
                   END-PERFORM 
               ELSE
                   MOVE    RECE41-KANANAME     TO  WRK-DATA
               END-IF
           END-IF 
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    性別
           MOVE    RECE41-SEX          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    生年月日
      *****MOVE    RECE41-BIRTHDAY     TO  WRK-SYMD
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    RECE41-BIRTHDAY     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    予備３
           PERFORM 5005-NUM-HENSYU-SEC
      *    入院年月日
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    病棟区分
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備４
           PERFORM 5004-MOJI-HENSYU-SEC
      *    予備５
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    病床数
           IF    ( SYS-1001-BEDSU      >   ZERO )
           AND   ( SYS-1001-HOSPSBT    =   "1"  ) 
               IF      RECE41-BEDSUFLG     =   "1" 
                   MOVE    SYS-1001-BEDSU      TO  WRK-NUM
                   MOVE    4                   TO  WRK-NAGASA
               ELSE
                   IF      SYS-1001-BEDSUIPN   >=  200
                       MOVE    SYS-1001-BEDSUIPN   TO  WRK-NUM
                       MOVE    4                   TO  WRK-NAGASA
                   END-IF
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC         
      *    カルテ番号等
           MOVE    RECE41-PTNUM        TO  WRK-DATA     
           MOVE    20                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備６
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備７
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備８
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備９
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    電算処理受付番号
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    記録条件仕様年月情報
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    請求情報
      *    請求情報（作成日時）
           STRING  WRK-PARA-CREYMD     DELIMITED  BY SIZE
                   WRK-PARA-CREHMS     DELIMITED  BY SIZE
                                       INTO  WRK-DATA
           END-STRING
           MOVE    14                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療科名１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    人体の部位等１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    性別等１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    医学的処置１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    特定疾病１
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    診療科名２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    人体の部位等２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    性別等２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    医学的処置２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    特定疾病２
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    診療科名３
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    人体の部位等３
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    性別等３
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    医学的処置３
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    特定疾病３
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備
           PERFORM 5004-MOJI-HENSYU-SEC
      *    患者状態 
           IF      RECE41-NINPUFLG >   ZERO
               MOVE    "001"           TO  WRK-DATA
           END-IF
           MOVE    60                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           MOVE    1                   TO  FLG-TOTALKIN
           PERFORM 5001-FILE-WRITE-SEC
           .
       2002-HENSYU-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    労災レセプトレコード編集処理
      *****************************************************************
       2003-HENSYU-SEC             SECTION.
      *     
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "RR,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    回数
           MOVE    RECE41-RECEKAISU    TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    業務災害・通勤災害
           EVALUATE    RECE41-SAIGAIKBN
               WHEN    "1"
                   MOVE    "1"                 TO  WRK-DATA
               WHEN    "2"
                   MOVE    "3"                 TO  WRK-DATA
           END-EVALUATE
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    帳票種別
           EVALUATE    RECE41-HKNKBN
               WHEN    "1"
                   MOVE    "3"                 TO  WRK-DATA
               WHEN    "2"
                   MOVE    "5"                 TO  WRK-DATA
           END-EVALUATE
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    年金証書番号
           IF      RECE41-HKNKBN       =   "2"
               MOVE    RECE41-KOFUNUM      TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
            PERFORM 5004-MOJI-HENSYU-SEC
      *    労働保険番号
           IF      RECE41-HKNKBN       =   "1"
               IF      RECE41-KOFUNUM  NOT =   SPACE
                   MOVE    RECE41-KOFUNUM      TO  WRK-DATA
               ELSE
                   MOVE    "99999999999999"    TO  WRK-DATA
               END-IF
               MOVE    14                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    傷病年月日
           IF      RECE41-HKNKBN       =   "1"
      *********MOVE    RECE41-SHOBYOYMD    TO  WRK-SYMD
      *********PERFORM 5002-HIZUKE-HEN-SEC
      *********MOVE    WRK-HENYMD          TO  WRK-DATA
      *********MOVE    7                   TO  WRK-NAGASA
               MOVE    RECE41-SHOBYOYMD    TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    新継再別
           MOVE    RECE41-SINKEI       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    転帰事由
           MOVE    RECE41-TENKI        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    療養期間
           IF      RECE41-JNISSU       =   999
               IF    ( RECE41-KYUGYOFLG    =   SPACE )
               OR    ( RECE41-KYUGYO-PRTKBN
                                           =   "2"   )
                   MOVE    RECE41-RYOSTYMD     TO  WRK-RYOSTYMD
                                                   WRK-RYOEDYMD
               ELSE
                   MOVE    RECE41-RYOSTYMD     TO  WRK-RYOSTYMD
      *
                   IF    ( RECE41-KYUGYO-END
                                           NOT NUMERIC    )
                   OR    ( RECE41-KYUGYO-END   =   "0000" )
                       MOVE    RECE41-RYOSTYMD     TO  WRK-RYOSTYMD
                                                       WRK-RYOEDYMD
                   ELSE
                       MOVE    RECE41-SRYYM(5:2)   TO  WRK-SINRYO-MM
                       MOVE    RECE41-KYUGYO-END(1:2)
                                                   TO  WRK-KYUGYO-MM
                       IF    WRK-KYUGYO-MM2    <=  WRK-SINRYO-MM2
                           MOVE   RECE41-SRYYM (1:4)
                                                   TO  WRK-RYOEDYMD(1:4)
                           MOVE   RECE41-KYUGYO-END
                                                   TO  WRK-RYOEDYMD(5:4)
                       ELSE
                           INITIALIZE                  STS-AREA-DAY
                           INITIALIZE                  LNK-DAY6-AREA
                           MOVE    "61"            TO  LNK-DAY6-IRAI
                           MOVE    RECE41-SRYYM    TO  
                                                   LNK-DAY6-KIJUN (1:6)
                           MOVE    "01"            TO
                                                   LNK-DAY6-KIJUN (7:2)
                           MOVE    "2"             TO  LNK-DAY6-ZOGENPTN
                           MOVE    -12             TO  LNK-DAY6-ZOGEN
                           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                           LNK-DAY6-AREA
                           MOVE    LNK-DAY6-KEISAN(1:4)
                                                   TO  WRK-RYOEDYMD(1:4)
                           MOVE    RECE41-KYUGYO-END
                                                   TO  WRK-RYOEDYMD(5:4)
                       END-IF
                   END-IF
               END-IF
           ELSE
               IF      RECE41-RYOSTYMD     <   WRK-STYMD
                   MOVE    WRK-STYMD           TO  WRK-RYOSTYMD
                   MOVE    WRK-STYMD (7:2)     TO  IDX5
               ELSE      
                   MOVE    RECE41-RYOSTYMD     TO  WRK-RYOSTYMD
                   MOVE    RECE41-RYOSTYMD (7:2)
                                               TO  IDX5
               END-IF    
               IF      RECE41-RYOEDYMD     >   WRK-EDYMD
                   MOVE    WRK-EDYMD           TO  WRK-RYOEDYMD
                   MOVE    WRK-EDYMD (7:2)     TO  IDX6
               ELSE      
                   MOVE    RECE41-RYOEDYMD     TO  WRK-RYOEDYMD
                   MOVE    RECE41-RYOEDYMD (7:2)
                                               TO  IDX6
               END-IF    
               IF    RECE41-NYUINFLG  =  1
                 PERFORM VARYING IDX4    FROM    IDX5    BY  1
                         UNTIL   IDX4    >       IDX6
                     IF    ( RECE41-NYUINBITBL(IDX4) =  SPACE )
                     OR    ( RECE41-JNISSUTBL (IDX4) =  "1"   )
                         MOVE    IDX4          TO  WRK-RYOSTYMD-DD
                         MOVE    IDX6          TO  IDX4
                     END-IF
                 END-PERFORM
                 PERFORM VARYING IDX4    FROM    IDX6    BY  -1
                         UNTIL   IDX4    <       IDX5
                     IF    ( RECE41-NYUINBITBL(IDX4) =   SPACE )
                     OR    ( RECE41-JNISSUTBL (IDX4) =   "1"   )
                         MOVE    IDX4          TO  WRK-RYOEDYMD-DD
                         MOVE    IDX5          TO  IDX4
                     END-IF
                 END-PERFORM
               END-IF
           END-IF
      *
      *    療養期間−初日
      *****MOVE    WRK-RYOSTYMD        TO  WRK-SYMD
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    WRK-RYOSTYMD        TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    療養期間−末日
      *****MOVE    WRK-RYOEDYMD        TO  WRK-SYMD
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    WRK-RYOEDYMD        TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療実日数
           MOVE    RECE41-JNISSU       TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    労働者の氏名（カナ）
           MOVE    1                   TO  IDY
           PERFORM VARYING IDX FROM    1   BY  2
                   UNTIL   IDX >       49
                   OR      RECE41-KANANAME (IDX:1) =   SPACE
               IF      RECE41-KANANAME (IDX:2)
                                           >=  "ァ"  AND
                                           <=  "ヶ"
                   EVALUATE    RECE41-KANANAME (IDX:2)
                       WHEN    "ァ"
                           MOVE    "ア"        TO  WRK-DATA (IDY:2)
                       WHEN    "ィ"
                           MOVE    "イ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ゥ"
                           MOVE    "ウ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ェ"
                           MOVE    "エ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ォ"
                           MOVE    "オ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ッ"
                           MOVE    "ツ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ャ"
                           MOVE    "ヤ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ュ"
                           MOVE    "ユ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ョ"
                           MOVE    "ヨ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ヮ"
                           MOVE    "ワ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ヵ"
                           MOVE    "カ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ヶ"
                           MOVE    "ケ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ヰ"
                           MOVE    "イ"        TO  WRK-DATA (IDY:2)
                       WHEN    "ヱ"
                           MOVE    "エ"        TO  WRK-DATA (IDY:2)
                       WHEN    OTHER
                           MOVE    RECE41-KANANAME (IDX:2)
                                               TO  WRK-DATA (IDY:2)
                   END-EVALUATE
                   ADD     2               TO  IDY    
               ELSE    
                   EVALUATE    RECE41-KANANAME (IDX:2)
      *                A8 A1
                       WHEN    "─"
      *                A1 DD
                       WHEN    "−"
      *                A1 BE
                       WHEN    "‐"
      *                A1 BD
                       WHEN    "―"
      *                    A1 BC
                           MOVE    "ー"            TO  WRK-DATA (IDY:2)
                           ADD     2               TO  IDY    
                       WHEN    "　"
      *                    A1 BC
                       WHEN    "ー"
                           MOVE    RECE41-KANANAME (IDX:2)
                                                   TO  WRK-DATA (IDY:2)
                           ADD     2               TO  IDY    
                   END-EVALUATE
               END-IF
           END-PERFORM
           IF      IDY                 =   1
               MOVE    RECE41-KANANAME     TO  WRK-DATA
           END-IF
           MOVE    40                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    事業の名称
           IF      RECE41-HKNKBN       =   "1"
               MOVE    RECE41-JIGYOUNAME   TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    事業場の所在地
           INITIALIZE                      WRK-PREF-AREA
           IF      RECE41-HKNKBN       =   "1"
               IF      RECE41-PREFNAME NOT =   SPACE
                   EVALUATE    RECE41-PREFKBN
                       WHEN    "1"
                           MOVE    "都"            TO  WRK-PREFKBN
                       WHEN    "2"
      **********           MOVE    "道"            TO  WRK-PREFKBN
                           MOVE    SPACE           TO  WRK-PREFKBN
                       WHEN    "3"
                           MOVE    "府"            TO  WRK-PREFKBN
                       WHEN    "4"
                           MOVE    "県"            TO  WRK-PREFKBN
                   END-EVALUATE
               END-IF
               IF      RECE41-CITYNAME NOT =   SPACE
                   EVALUATE    RECE41-CITYKBN
                       WHEN    "1"
                           MOVE    "郡"            TO  WRK-CITYKBN
                       WHEN    "2"
                           MOVE    "市"            TO  WRK-CITYKBN
                       WHEN    "3"
                           MOVE    "区"            TO  WRK-CITYKBN
                   END-EVALUATE
               END-IF
               STRING  RECE41-PREFNAME     DELIMITED  BY  SPACE
                       WRK-PREFKBN         DELIMITED  BY  SPACE
                       RECE41-CITYNAME     DELIMITED  BY  SPACE
                       WRK-CITYKBN         DELIMITED  BY  SPACE
                                           INTO  WRK-DATA
               END-STRING
               MOVE    80                  TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    傷病の経過
           MOVE    ZERO                TO  FLG-RECECOM-XX
           MOVE    SPACE               TO  WRK-COMM-BACK
      *
           INITIALIZE                      RECECOM-REC
           MOVE    RECE41-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE41-PTID         TO  RECECOM-PTID
           MOVE    RECE41-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
           MOVE    "AA"                TO  RECECOM-SJKBN
           MOVE    RECE41-HKNCOMBI     TO  RECECOM-HKNCOMBI
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key16"             TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           PERFORM             UNTIL   FLG-RECECOM   =   1
               IF      RECECOM-HKNCOMBI    =   RECE41-HKNCOMBI
                   MOVE    RECECOM-COMMENT TO  WRK-COMM
                   MOVE    ZERO            TO  WRK-RECEHEN
                   PERFORM     UNTIL       WRK-RECEHEN  =   1      
                       MOVE    SPACE           TO  WRK-COMM1
                       MOVE    ZERO            TO  WRK-COL
      *
                       IF      WRK-COMM (1:1)  =   WRK-CONS-RETURN
                           MOVE    WRK-COMM (2:)   TO  WRK-COMM1
                       ELSE             
                           INSPECT WRK-COMM    TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                           IF      WRK-COL         >   ZERO
                               PERFORM VARYING IDX FROM    1   BY  1
                                       UNTIL   IDX >
                                           WRK-CONS-RECECOM-LENGTH-MAX
                                  IF       WRK-COMM(IDX:1) =
                                                       WRK-CONS-RETURN
                                       COMPUTE IDY1    =   IDX     -   1
                                       COMPUTE IDY2    =   IDX     +   1
                                       STRING  WRK-COMM-BACK
                                                   DELIMITED  BY  SPACE
                                               WRK-COMM (1:IDY1)
                                                   DELIMITED  BY  SPACE
                                                   INTO  WRK-DATA
                                       END-STRING
                                       MOVE    WRK-COMM (IDY2:)
                                                           TO  WRK-COMM1
                                       MOVE
                                           WRK-CONS-RECECOM-LENGTH-MAX
                                                           TO  IDX
                                   END-IF
                               END-PERFORM
                           ELSE
                               STRING  WRK-COMM-BACK
                                                   DELIMITED  BY  SPACE
                                       WRK-COMM    DELIMITED  BY  SPACE
                                                   INTO  WRK-DATA
                               END-STRING
                               MOVE    1           TO  WRK-RECEHEN
                           END-IF
      *
                           IF      WRK-DATA        NOT =   SPACE
                               IF      WRK-DATA (101:) NOT =   SPACE
      *                            ５０文字を超えていたら症状詳記に記載
                                   MOVE    1       TO  FLG-RECECOM-XX
                                   MOVE    1       TO  WRK-RECEHEN
                                   MOVE    1       TO  FLG-RECECOM
                               ELSE
                                   MOVE    WRK-DATA
                                                   TO  WRK-COMM-BACK
                               END-IF 
                               MOVE    100         TO  WRK-NAGASA
                           END-IF
                       END-IF  
                       MOVE    WRK-COMM1       TO  WRK-COMM
                   END-PERFORM
               END-IF    
               IF      FLG-RECECOM             =   ZERO
                   MOVE    "tbl_rececom"       TO  MCP-TABLE
                   MOVE    "key16"             TO  MCP-PATHNAME
                   PERFORM 900-RECECOM-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-RECECOM-XX      =   1
               MOVE    "症状詳記に記載"    TO  WRK-DATA
           END-IF
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    小計点数
           MOVE    RECE41-TOTALTEN     TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    小計点数金額換算（イ）
           MOVE    RECE41-TOTALKIN     TO  WRK-NUM
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    小計金額（ロ）
           MOVE    RECE41-TOTALKIN2    TO  WRK-NUM
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    食事療養合計回数
           PERFORM 5005-NUM-HENSYU-SEC
      *    食事療養合計金額（ハ）
           PERFORM 5005-NUM-HENSYU-SEC
      *    合計額（イ）＋（ロ）＋（ハ）
           MOVE    SUM-TOTALKIN        TO  WRK-NUM
           MOVE    9                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5005-NUM-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       2003-HENSYU-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    傷病名レコード編集処理
      *****************************************************************
       2006-HENSYU-SEC             SECTION.
      *     
           INITIALIZE                      RECE43-REC
           MOVE    RECE41-KEY          TO  RECE43-KEY1
      *
           START   RECE43-FILE
               KEY IS    >=   RECE43-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE43
               NOT INVALID
                   PERFORM 990-RECE43-NEXT-SEC
           END-START
      *     
           PERFORM UNTIL  ( FLG-RECE43   =   1           )
                   OR     ( RECE41-KEY
                                     NOT =   RECE43-KEY1 )
      *        傷病名編集処理
               PERFORM 20061-BYOMEI-HENSYU-SEC
      *             
               PERFORM 990-RECE43-NEXT-SEC
           END-PERFORM       
           .
       2006-HENSYU-EXT.
           EXIT. 
      *            
      *
      *****************************************************************
      *    傷病名編集処理
      *****************************************************************
       20061-BYOMEI-HENSYU-SEC         SECTION.
      *     
      *    廃止年月日が設定されているとき廃止年月日＜診療開始日なら
      *    未傷病名コードにする
      *    保険請求対象外のときは未傷病名コードにする
      *    保険請求対象外の傷病名コードのみのときはそのまま記録する
      *
           IF      RECE43-BYOMEIHENFLG =   "1"
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE43-BYOMEICD (IDX)   =   SPACE
                   IF      RECE43-BYOMEICD(IDX)   =   ALL "9"
                       CONTINUE
                   ELSE  
                       IF      RECE43-BYOMEICD(IDX)(1:3)
                                                   NOT =   "ZZZ"
                           MOVE    RECE43-BYOMEICD(IDX)
                                                   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      BYO-HKNSKYKBN   =   1
                               IF      RECE43-BYOMEICD (2)
                                                   NOT =   SPACE
                                   MOVE    "1" TO  RECE43-BYOMEIHENFLG
                               END-IF
                               MOVE    21      TO  IDX
                           ELSE
                               IF      BYO-HAISIYMD    NOT =   SPACE
                                   IF      BYO-HAISIYMD    <
                                                       RECE43-SRYYMD
                                       MOVE    "1"     TO
                                                   RECE43-BYOMEIHENFLG
                                       MOVE    21      TO  IDX
                                   END-IF
                               END-IF
                           END-IF    
                       END-IF    
                   END-IF       
               END-PERFORM
           END-IF    
      *
      *    疑いフラグより病名を編集
           IF      RECE43-UTAGAIFLG    NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    RECE43-UTAGAIFLG    TO  SMKBYO-UTAGAIFLG
               MOVE    RECE43-IN-BYOMEI    TO  SMKBYO-BYOMEI
               MOVE    RECE43-BYOMEIHENFLG
                                           TO  SMKBYO-BYOMEIHENFLG
               MOVE    RECE43-BYOMEICD-G   TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
               MOVE    SMKBYO-BYOMEI       TO  RECE43-BYOMEI
               MOVE    SMKBYO-BYOMEIHENFLG TO  RECE43-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  RECE43-BYOMEICD-G
           ELSE
               MOVE    RECE43-IN-BYOMEI    TO  RECE43-BYOMEI 
           END-IF
      *
      *    傷病名コード／修飾語コードの取得
           MOVE    SPACE               TO  WRK-BYOMEI-AREA
      *
           IF      RECE43-BYOMEIHENFLG =   "1"
      *        編集病名のとき未傷病名コードにする
               MOVE    "0000999"           TO  WRK-BYOMEICD
           ELSE
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE43-BYOMEICD(IDX)   =   SPACE
                 IF      RECE43-BYOMEICD(IDX) =   ALL "9"
                     CONTINUE
                 ELSE
                   IF      RECE43-BYOMEICD(IDX)(1:3)  NOT =   "ZZZ"
                       IF      WRK-BYOMEICD                =   SPACE
                           MOVE    RECE43-BYOMEICD(IDX)
                                                   TO  WRK-BYOMEICD
                       END-IF
                   ELSE
                       ADD     1           TO  IDY
                       MOVE    RECE43-BYOMEICD(IDX)(4:)
                                           TO  WRK-RECECD(IDY)
                   END-IF
                 END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "SY,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    傷病名コード
           MOVE    WRK-BYOMEICD        TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    診療開始日
      *****MOVE    RECE43-SRYYMD      TO  WRK-SYMD
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    RECE43-SRYYMD       TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    予備１
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    修飾語コード
           EVALUATE    WRK-BYOMEICD 
               WHEN    "0000998"
               WHEN    "0000999"
                   CONTINUE
               WHEN    OTHER
                   MOVE    WRK-RECECD-G        TO  WRK-DATA
                   MOVE    80                  TO  WRK-NAGASA
           END-EVALUATE
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    傷病名称
           IF      WRK-BYOMEICD        =   "0000999"   OR
                                           "0000998"
               MOVE    RECE43-BYOMEI       TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    主傷病
           IF    ( RECE43-SYUBYOFLG-X  =   "1"  )
           AND   ( SYS-2005-SYUBYOHENFLG
                                   NOT =   ZERO )
               MOVE    "01"                TO  WRK-DATA
           END-IF 
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    補足コメント
           MOVE    RECE43-HOSOKU-COMMENT
                                       TO  WRK-DATA
           MOVE    40                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20061-BYOMEI-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    摘要レコード編集処理
      *****************************************************************
       2007-HENSYU-SEC              SECTION.
      *                                        
      *    摘要情報
           INITIALIZE                      RECE44-REC
           MOVE    RECE41-KEY          TO  RECE44-KEY1
      *
           START   RECE44-FILE
               KEY IS    >=   RECE44-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE44
               NOT INVALID
                   PERFORM 990-RECE44-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE44           =   1            )
               OR     ( RECE41-KEY       NOT =   RECE44-KEY1 )
      *
               IF      FLG-COMMENT         =   ZERO
                   MOVE    1                   TO  FLG-COMMENT
      *            症状詳記（手術歴）
                   MOVE    ZERO                TO  FLG-TOYAKUFLG
                   PERFORM 2008-COMMENT-HEN-01-SEC
               END-IF
      *
      *        検査に判断料がある場合の検査の編集
               IF    ( RECE44-SRYKBN      >   "60"     )
               AND   ( TBL-KENSA-RECE44-MAX
                                           >   0        )
                   MOVE    RECE44-REC         TO  WRK-TAIHI-RECE44-REC
      *
                   PERFORM 20078-KENSA-HANDANRYO-HEN-SEC
      *
                   MOVE    WRK-TAIHI-RECE44-REC
                                               TO  RECE44-REC
      *
                   INITIALIZE                  TBL-KENSA-RECE44-AREA
               END-IF
      *
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
      *
      *        仮レセデータフラグ設定
               IF      RECE44-TOYAKUFLG =   SPACE
                   MOVE    ZERO              TO  FLG-TOYAKUFLG
               ELSE
                   MOVE    1                 TO  FLG-TOYAKUFLG
               END-IF 
      *
      *       摘要データ編集処理
               PERFORM 20074-ZAI-KOUI-SORT-SEC
               PERFORM 20072-ZAI-HENSYU-SEC
      * 
               PERFORM 990-RECE44-NEXT-SEC
           END-PERFORM
      *
      *    検査に判断料がある場合の検査の編集
           IF    ( TBL-KENSA-RECE44-MAX
                                       >   0        )
               PERFORM 20078-KENSA-HANDANRYO-HEN-SEC
      *
               INITIALIZE                  TBL-KENSA-RECE44-AREA
           END-IF
      *
           .
       2007-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    剤明細テーブル退避処理
      *****************************************************************
       20071-ZAI-TAIHI-SEC    SECTION.
      *
           INITIALIZE                      TBL-ZAIW-AREA
                                           TBL-KOUI-AREA
                                           TBL-KOUI-SRYKBN12-AREA
                                           TBL-KOUI-SRYKBN12-CHU-AREA
                                           TBL-COMMENT-AREA
           INITIALIZE                      WRK-RECE45-REC
                                           WRK-KASAN-RECE45-REC
                                           WRK-TEIGEN-RECE45-REC
                                           WRK-700-RECE45-AREA
                                           WRK-700-DOUJI-RECE45-AREA
                                           WRK-721-RECE45-REC
                                           WRK-721-DOUJI-RECE45-REC
                                           WRK-SRYKBN70-RECE45-REC
           INITIALIZE                      TBL-RECE455-COMMENT-AREA
      *
           MOVE    ZERO                TO  WRK-ZAIWMAX
           MOVE    ZERO                TO  WRK-TBLMAX1
                                           WRK-TBLMAX6
                                           WRK-TBLMAX7
                                           WRK-TBLMAX7-SRT
                                           WRK-TBLMAX8
                                           FLG-TEIGEN
                                           FLG-YKZ20
                                           FLG-KOKUJISKBKBN1
                                           FLG-SRYSYUKBN721
                                           FLG-SRYSYUKBN721-DOUJI 
                                           FLG-SRYKBN70-TEIGEN
                                           FLG-KENSA-KASAN
                                           FLG-KENSA-TEIGEN
                                           WRK-SRYCD1-MAX
                                           WRK-SRYCD71-MAX
                                           WRK-SRYCD72-MAX
                                           WRK-KOKUJISKBKBN1-KOUI-TEN
      *                                     
           MOVE    RECE44-SRYKBN      TO  WRK-SRYKBN
      *
      *    検査（判断料）の退避
           IF     ( RECE44-KNSJISKBN   =   2 )
               ADD     1               TO  TBL-KENSA-RECE44-MAX
               MOVE    RECE44-REC      TO  TBL-KENSA-RECE44-REC
                                           (TBL-KENSA-RECE44-MAX)
           END-IF
           MOVE    ZERO                 TO  TBL-KENSA-RECE45-MAX
      *
           INITIALIZE                      RECE45-REC
           MOVE    RECE44-KEY1         TO  RECE45-KEY1
           MOVE    RECE44-ZAINUM       TO  RECE45-ZAINUM
           START   RECE45-FILE
               KEY IS    >=   RECE45-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE45
               NOT INVALID
                   PERFORM 990-RECE45-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE45             =   1             )
               OR     ( RECE44-KEY1        NOT =   RECE45-KEY1   )
               OR     ( RECE44-ZAINUM      NOT =   RECE45-ZAINUM )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE45-SRYCD(IDX) =   SPACE )
                 IF    ( RECE45-RECEDENFLG(IDX) =   SPACE )
                 AND   ( RECE45-MEISKYFLG (IDX)
                                            NOT =   "2"   )
                   IF    ( RECE45-SRYCD (IDX) (1:3) = "059"   )
                       MOVE    "777770000"      TO  RECE45-SRYCD (IDX)
                   END-IF
      *
                   EVALUATE    RECE45-SRYCD (IDX) (1:1)
                       WHEN    "0"
                       WHEN    "8"
      *                    空白または全角スペースのみのフリーコメントは記載しない
                           MOVE    ZERO                TO  WRK-SJ-CHK
                           IF      RECE45-SRYCD (IDX)  =   "810000001"
                               IF      RECE45-NAME  (IDX)  =   SPACE
                                   CONTINUE
                               ELSE
                                   PERFORM VARYING IDY FROM    1   BY  2
                                           UNTIL   IDY >       200
                                           OR  RECE45-NAME(IDX) (IDY:2)
                                                           =   SPACE
                                           OR  WRK-SJ-CHK  =   1
                                       IF       RECE45-NAME(IDX) (IDY:2)
                                                       NOT =   "　"
                                           MOVE    1   TO  WRK-SJ-CHK
                                       END-IF
                                   END-PERFORM
                               END-IF
                               IF      WRK-SJ-CHK      =   1
      *                            後ろの全角空白を削除する
                                   INITIALIZE          ORCSKANACHKAREA
                                   MOVE    "1"     TO  KANACHK-SYORI
                                   MOVE    "1"     TO  KANACHK-SYORIKBN 
                                   MOVE    RECE45-NAME(IDX)
                                                   TO  KANACHK-MAE-INPUT
                                   CALL    "ORCSKANACHK"
                                               USING   ORCSKANACHKAREA
                                   IF    ( KANACHK-RC  =   ZERO )
                                   AND   ( KANACHK-MAX >   ZERO )
                                       MOVE    SPACE
                                                   TO  RECE45-NAME(IDX)
                                       MOVE    KANACHK-OUT-INPUT
                                                       (1:KANACHK-MAX)
                                                   TO  RECE45-NAME(IDX)
                                   END-IF
                               END-IF
                           ELSE
                               MOVE    1               TO  WRK-SJ-CHK
                           END-IF
                           IF      WRK-SJ-CHK          =   ZERO
                               CONTINUE
                           ELSE                      
      *                        検査（判断料）の場合の退避
                               IF      RECE44-KNSJISKBN   =   2
                                   PERFORM
                                       20013-KENSA-HANDANRYO-TAIHI-SEC
                               ELSE
                                   IF      RECE44-SRYKBN   =   "11"   OR
                                                               "12"   OR
                                                               "13"   OR
                                                               "14"   OR
                                                               "70"
                                     PERFORM
                                      200711-SRYKBN13-COM-TAIHI-SEC
                                   ELSE
                                      IF    ( RECE44-SRYKBN
                                                           =   "23"  )
                                      AND   ( RECE44-SIPPUFLG
                                                       NOT =   SPACE )
                                        IF      RECE45-SRYCD (IDX)
                                                           = "820000047"
                                          PERFORM
                                              200711-COMMENT-TAIHI-SEC
                                        ELSE
                                          PERFORM
                                           200711-SRYKBN13-COM-TAIHI-SEC
                                        END-IF
                                      ELSE
                                        PERFORM 200711-COMMENT-TAIHI-SEC
                                      END-IF 
                                   END-IF
                               END-IF
                           END-IF
                       WHEN    "1"
      *                    検査（判断料）の場合の退避
                           IF     ( RECE44-KNSJISKBN   =   2 )
                               PERFORM 20013-KENSA-HANDANRYO-TAIHI-SEC
                           ELSE
                               PERFORM 200711-KOUI-TAIHI-SEC
                           END-IF
                       WHEN    "6"
                           PERFORM 200711-YKZ-TAIHI-SEC
                       WHEN    "7"
                           PERFORM 200711-KIZAI-TAIHI-SEC
                           IF      RECE44-ZAIREQFLG       =   1
                               PERFORM 200711-KIZAI-ACCT-TAIHI-SEC
                           END-IF
                   END-EVALUATE
                 END-IF
               END-PERFORM
      *
               PERFORM 990-RECE45-NEXT-SEC
           END-PERFORM
      *
      *    画像診断のとき順番を変える
      *
           IF      FLG-SRYSYUKBN721    >   ZERO
               MOVE     FLG-SRYSYUKBN721   TO  IDX 
               MOVE    WRK-721-RECE45-REC  TO  RECE45-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                   MOVE    WRK-SRYKBN70-RECE45-REC
                                               TO  RECE45-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX 
                       MOVE     FLG-SRYSYUKBN700  (IDXY)  TO  IDX 
                       MOVE    WRK-700-RECE45-REC (IDXY)  TO  RECE45-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-PERFORM    
               END-IF
           END-IF
           IF      FLG-SRYSYUKBN721-DOUJI  >   ZERO
               MOVE    FLG-SRYSYUKBN721-DOUJI      TO  IDX 
               MOVE    WRK-721-DOUJI-RECE45-REC    TO  RECE45-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                   MOVE    WRK-SRYKBN70-RECE45-REC
                                               TO  RECE45-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-DOUJI-MAX  >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >
                                           WRK-SRYSYUKBN700-DOUJI-MAX
                       MOVE    FLG-SRYSYUKBN700-DOUJI (IDXY)
                                                          TO  IDX 
                       MOVE    WRK-700-DOUJI-RECE45-REC (IDXY)
                                                          TO  RECE45-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-PERFORM    
               END-IF
           END-IF
      *
           .
       20071-ZAI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）退避処理
      *****************************************************************
       200711-COMMENT-TAIHI-SEC       SECTION.
      *
      *    診療行為コード＝'０'のとき対象外出力しない
           IF      RECE45-SRYCD (IDX) (1:1) =   "0"
               IF    ( RECE45-SRYCD (IDX)(1:3)  =   "002"             )
               OR    ( RECE45-SRYCD (IDX)(1:3)  =   "008"             )
               OR    ( RECE45-SRYCD (IDX)       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE45-SRYCD (IDX)       =   WRK-CONS-COMMENT2 ) 
      *********OR    ( RECE45-SRYCD (IDX)       =   WRK-CONS-COMMENT3 ) 
      *********OR    ( RECE45-SRYCD (IDX)       =   WRK-CONS-COMMENT4 ) 
                   CONTINUE
               ELSE
                   GO  TO  200711-COMMENT-TAIHI-EXT
               END-IF
           END-IF    
      *     
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCR0555 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    RECE45-SRYCD (IDX)  TO
                                           TBL-KOUI-SRYCD (WRK-TBLMAX1)
               MOVE    RECE45-NAME  (IDX)  TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
      *
               IF      RECE45-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE45-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                           TBL-KOUI-SRYCD (WRK-TBLMAX1)
                   END-IF
      *            コメント
                   IF    ( RECE45-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE45-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE45-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE45-INPUTCOMENT (IDX)     TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
                   END-IF
               END-IF
      *        コメントパターン２０のコメントコード
               IF      RECE45-SRYCD (IDX)(1:2)
                                           =   "82"
                   MOVE    SPACE              TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
               END-IF
           END-IF
           .
       200711-COMMENT-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）退避処理
      *****************************************************************
       200711-SRYKBN13-COM-TAIHI-SEC       SECTION.
      *
      *    診療行為コード＝'０'のとき対象外出力しない
           IF      RECE45-SRYCD (IDX) (1:1) =   "0"
               IF    ( RECE45-SRYCD (IDX)(1:3)  =   "002"             )
               OR    ( RECE45-SRYCD (IDX)(1:3)  =   "008"             )
               OR    ( RECE45-SRYCD (IDX)       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE45-SRYCD (IDX)       =   WRK-CONS-COMMENT2 ) 
      *********OR    ( RECE45-SRYCD (IDX)       =   WRK-CONS-COMMENT3 ) 
      *********OR    ( RECE45-SRYCD (IDX)       =   WRK-CONS-COMMENT4 ) 
                   CONTINUE
               ELSE
                   GO  TO  200711-SRYKBN13-COM-TAIHI-EXT
               END-IF
           END-IF    
      *               
           IF      WRK-TBLMAX8         >=  100
               MOVE    "*** ORCR0555 COMMENT TABLE  OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX8
      *
               MOVE    RECE45-SRYCD (IDX)  TO
                                   TBL-COMMENT-SRYCD (WRK-TBLMAX8)
               MOVE    RECE45-NAME  (IDX)  TO
                                   TBL-COMMENT-NAME  (WRK-TBLMAX8)
      *
               IF      RECE45-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE45-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                       TBL-COMMENT-SRYCD (WRK-TBLMAX8)
                   END-IF
      *            コメント
                   IF    ( RECE45-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE45-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE45-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE45-INPUTCOMENT (IDX)     TO
                                       TBL-COMMENT-NAME  (WRK-TBLMAX8)
                   END-IF
               END-IF
      *        コメントパターン２０のコメントコード
               IF      RECE45-SRYCD (IDX)(1:2)
                                           =   "82"
                   MOVE    SPACE              TO
                                       TBL-COMMENT-NAME  (WRK-TBLMAX8)
               END-IF
           END-IF
          .
       200711-SRYKBN13-COM-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ退避処理
      *****************************************************************
       200711-KOUI-TAIHI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-ACCTP-SYUTEN
      *
      *    再診の時間外のとき
           IF      RECE44-SRYKBN      =   "12"
               PERFORM VARYING IDV1    FROM    1   BY  1
                       UNTIL   IDV1    >       WRK-CONS-JIKANGAI-MAX
                   IF      RECE45-SRYCD (IDX)
                                   =   WRK-CONS-JIKANGAI-SRYCD (IDV1)
                       MOVE    WRK-CONS-JIKANGAI-TEN (IDV1)
                                               TO  WRK-ACCTP-SYUTEN
                       MOVE    WRK-CONS-JIKANGAI-MAX
                                               TO  IDV1
                   END-IF
               END-PERFORM
               IF      WRK-ACCTP-SYUTEN        >   ZERO 
                   ADD     1               TO  TBL-KOUI-SRYKBN12-MAX
                   MOVE    RECE45-KEY      TO  TBL-KOUI-SRYKBN12-KEY
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    IDX             TO
                                       TBL-KOUI-SRYKBN12-SHORI-RENNUM
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE44-ZAIKAISU
                                           TO  TBL-KOUI-SRYKBN12-KAISU
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE45-SRYCD (IDX)
                                           TO  TBL-KOUI-SRYKBN12-SRYCD
                                               (TBL-KOUI-SRYKBN12-MAX)    
                   MOVE    WRK-ACCTP-SYUTEN
                                           TO  TBL-KOUI-SRYKBN12-TEN
                                               (TBL-KOUI-SRYKBN12-MAX)
                   ADD     WRK-ACCTP-SYUTEN
                                           TO  TBL-KOUI-SRYKBN12-TEN-G
      *    
                   MOVE    ZERO            TO  WRK-ACCTP-SYUTEN
                   GO  TO  200711-KOUI-TAIHI-EXT
               END-IF
           END-IF
      *
           IF    ( RECE44-SRYKBN      =   "11"   OR  "12"     )
           AND   ( RECE44-ZAIREQFLG   =   "1"                 )
           AND   ( RECE45-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
      *        基本項目のとき診療会計マスタ付加情報から点数を取得
               PERFORM VARYING IDV1    FROM    1   BY  1
                       UNTIL   IDV1    >       5
                       OR      RECE44-ACCTP-SRYCD (IDV1)
                                       =   SPACE
                   IF      RECE45-SRYCD (IDX)  =
                                           RECE44-ACCTP-SRYCD (IDV1)
                       MOVE    RECE44-ACCTP-SYUTEN (IDV1)
                                               TO  WRK-ACCTP-SYUTEN
                       MOVE    5               TO  IDV1
                   END-IF
               END-PERFORM
      *
      *        加算項目だが基本項目扱いのとき剤を別にする
               IF    ( RECE45-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
               AND   ( RECE45-KOKUJISKBKBN2 (IDX)
                                       =   "7" OR  "9"         )
                   ADD     1               TO  TBL-KOUI-SRYKBN12-MAX
                   MOVE    RECE45-KEY      TO  TBL-KOUI-SRYKBN12-KEY
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    IDX             TO
                                       TBL-KOUI-SRYKBN12-SHORI-RENNUM
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE44-ZAIKAISU
                                           TO  TBL-KOUI-SRYKBN12-KAISU
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE45-SRYCD (IDX)
                                           TO  TBL-KOUI-SRYKBN12-SRYCD
                                               (TBL-KOUI-SRYKBN12-MAX)    
                   MOVE    WRK-ACCTP-SYUTEN
                                           TO  TBL-KOUI-SRYKBN12-TEN
                                               (TBL-KOUI-SRYKBN12-MAX)
      *    
                   MOVE    ZERO            TO  WRK-ACCTP-SYUTEN
                   GO  TO  200711-KOUI-TAIHI-EXT
               END-IF
      *
      *        基本項目のとき付加情報の点数を設定
               IF      RECE44-SRYKBN      =   "11"
                   MOVE    WRK-ACCTP-SYUTEN  TO  RECE44-SYUTEN1
               END-IF
           END-IF
      *
           IF    ( RECE44-SRYKBN      =   "13"    OR  "14"     )
           AND   ( RECE45-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
      *        加算項目だが基本項目扱いのとき剤を別にする
               IF    ( RECE45-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
               AND   ( RECE45-DATAKBN (IDX)
                                       =   "2"                 )
                   ADD     1               TO  TBL-KOUI-SRYKBN12-MAX
                   MOVE    RECE45-KEY      TO  TBL-KOUI-SRYKBN12-KEY
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    IDX             TO
                                       TBL-KOUI-SRYKBN12-SHORI-RENNUM
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE44-ZAIKAISU
                                           TO  TBL-KOUI-SRYKBN12-KAISU
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE45-SRYCD (IDX)
                                           TO  TBL-KOUI-SRYKBN12-SRYCD
                                               (TBL-KOUI-SRYKBN12-MAX)    
                   MOVE    RECE45-CHUKSNCD (IDX)
                                           TO
                                           TBL-KOUI-SRYKBN12-CHUKSNCD
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE45-TEN (IDX)
                                           TO  TBL-KOUI-SRYKBN12-TEN
                                               (TBL-KOUI-SRYKBN12-MAX)
                   ADD     RECE45-TEN (IDX)
                                           TO  TBL-KOUI-SRYKBN12-TEN-G
                   GO  TO  200711-KOUI-TAIHI-EXT
               END-IF
           END-IF
      *
           IF    ( RECE44-SRYKBN       =   "13"    OR  "14" )
           AND   ( RECE45-KOKUJISKBKBN1 (IDX)
                                       =   "7" OR  "9"      )
           AND   ( RECE45-CHUKSNCD      (IDX)
                                       =   "1419"           )
      *        注加算コードが１４１９の加算項目のとき剤を別にする
               ADD     1               TO  TBL-KOUI-SRYKBN12-CHU-MAX
               MOVE    RECE45-KEY      TO  TBL-KOUI-SRYKBN12-CHU-KEY
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    IDX             TO
                                   TBL-KOUI-SRYKBN12-CHU-SHORI-NUM
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE44-ZAIKAISU
                                       TO  TBL-KOUI-SRYKBN12-CHU-KAISU
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE45-SRYCD (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-SRYCD
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE45-TEN (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-TEN
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE45-CHUKSNCD (IDX)
                                       TO
                                       TBL-KOUI-SRYKBN12-CHU-CHUKSNCD
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               ADD     RECE45-TEN (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-TEN-G
               GO  TO  200711-KOUI-TAIHI-EXT
           END-IF
      *
      *    画像診断の撮影料のとき順番を変える（同時撮影以外）
           IF     RECE45-SRYSYUKBN-X (IDX)
                                       =   "721"
             MOVE    RECE45-REC          TO  WRK-RECE45-REC
             MOVE    IDX                 TO  IDW1
      * 
             IF     RECE45-SRYCD (IDX)
                                   NOT =   WRK-CONS-DOUJI-SATUEI (1)
                                       AND WRK-CONS-DOUJI-SATUEI (2) 
                                       AND WRK-CONS-DOUJI-SATUEI (3) 
                                       AND WRK-CONS-DOUJI-SATUEI (4) 
                                       AND WRK-CONS-DOUJI-SATUEI (5) 
                                       AND WRK-CONS-DOUJI-SATUEI (6) 
                                       AND WRK-CONS-DOUJI-SATUEI (7) 
                                       AND WRK-CONS-DOUJI-SATUEI (8)
      *
               IF      FLG-SRYSYUKBN721    >   ZERO
                   MOVE     FLG-SRYSYUKBN721   TO  IDX 
                   MOVE    WRK-721-RECE45-REC  TO  RECE45-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO                TO  FLG-SRYSYUKBN721
                   INITIALIZE                  WRK-721-RECE45-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                       MOVE    WRK-SRYKBN70-RECE45-REC
                                               TO  RECE45-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       MOVE    ZERO            TO  FLG-SRYKBN70-TEIGEN
                       INITIALIZE              WRK-SRYKBN70-RECE45-REC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX 
                           MOVE     FLG-SRYSYUKBN700  (IDXY)
                                                           TO  IDX 
                           MOVE    WRK-700-RECE45-REC (IDXY)
                                                           TO RECE45-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM    
      *****************MOVE    ZERO                TO  FLG-SRYSYUKBN700
                       INITIALIZE                  WRK-700-RECE45-AREA
                   END-IF
               END-IF
               IF      FLG-SRYSYUKBN721-DOUJI  >   ZERO
                   MOVE    FLG-SRYSYUKBN721-DOUJI      TO  IDX 
                   MOVE    WRK-721-DOUJI-RECE45-REC    TO  RECE45-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO               TO  FLG-SRYSYUKBN721-DOUJI
                   INITIALIZE                   WRK-721-DOUJI-RECE45-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                                   TO  IDX 
                       MOVE    WRK-SRYKBN70-RECE45-REC
                                                   TO  RECE45-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-DOUJI-MAX  >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >
                                           WRK-SRYSYUKBN700-DOUJI-MAX
                           MOVE    FLG-SRYSYUKBN700-DOUJI (IDXY)
                                                          TO  IDX 
                           MOVE    WRK-700-DOUJI-RECE45-REC (IDXY)
                                                          TO  RECE45-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM    
                       INITIALIZE              WRK-700-DOUJI-RECE45-AREA
                   END-IF
               END-IF
             END-IF  
      *
             MOVE    WRK-RECE45-REC      TO  RECE45-REC
             MOVE    IDW1                TO  IDX
      *
             IF     RECE45-SRYCD (IDX)
                                   NOT =   WRK-CONS-DOUJI-SATUEI (1)
                                       AND WRK-CONS-DOUJI-SATUEI (2) 
                                       AND WRK-CONS-DOUJI-SATUEI (3) 
                                       AND WRK-CONS-DOUJI-SATUEI (4) 
                                       AND WRK-CONS-DOUJI-SATUEI (5) 
                                       AND WRK-CONS-DOUJI-SATUEI (6) 
                                       AND WRK-CONS-DOUJI-SATUEI (7) 
                                       AND WRK-CONS-DOUJI-SATUEI (8)
               MOVE    IDX                 TO  FLG-SRYSYUKBN721
               MOVE    RECE45-REC          TO WRK-721-RECE45-REC
             ELSE
               MOVE    IDX                 TO  FLG-SRYSYUKBN721-DOUJI
               MOVE    RECE45-REC          TO WRK-721-DOUJI-RECE45-REC
             END-IF
           ELSE
               IF    ( RECE45-SRYCD (IDX)  =  WRK-CONS-SRYKBN70-TEIGEN )
               AND   ( FLG-SRYSYUKBN721    >  ZERO                     )
                   MOVE    IDX         TO  FLG-SRYKBN70-TEIGEN
                   MOVE    RECE45-REC  TO  WRK-SRYKBN70-RECE45-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               ELSE
                   IF    ( RECE45-SRYSYUKBN-X (IDX)
                                               =   "700"  )
                   AND   ( RECE45-DATAKBN   (IDX)
                                               =   "2"    )
                   AND   (  (  FLG-SRYSYUKBN721-DOUJI
                                               >   ZERO ) 
                                   OR
                            ( FLG-SRYSYUKBN721 >   ZERO ) )
                       IF      FLG-SRYSYUKBN721-DOUJI  >   ZERO 
                           ADD     1           TO
                                           WRK-SRYSYUKBN700-DOUJI-MAX
                           MOVE    IDX         TO
                                           FLG-SRYSYUKBN700-DOUJI
                                            (WRK-SRYSYUKBN700-DOUJI-MAX) 
                           MOVE    RECE45-REC  TO
                                           WRK-700-DOUJI-RECE45-REC
                                            (WRK-SRYSYUKBN700-DOUJI-MAX) 
                       ELSE
                           ADD     1           TO  WRK-SRYSYUKBN700-MAX
                           MOVE    IDX         TO  FLG-SRYSYUKBN700
                                                  (WRK-SRYSYUKBN700-MAX) 
                           MOVE    RECE45-REC  TO WRK-700-RECE45-REC
                                                  (WRK-SRYSYUKBN700-MAX)
                       END-IF
                   ELSE
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-IF    
               END-IF     
           END-IF
           .
       200711-KOUI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）退避編集処理
      *****************************************************************
       2007111-KOUI-TAIHI-HENSYU-SEC SECTION.
      *
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCR0555 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    WRK-TBLMAX1         TO  WRK-SRYCD1-MAX
      *
               MOVE    RECE45-KEY          TO  TBL-KOUI-KEY
                                                        (WRK-TBLMAX1)
               MOVE    IDX                 TO  TBL-KOUI-SHORI-RENNUM
                                                        (WRK-TBLMAX1)
               MOVE    RECE45-SRYACTINF (IDX)  TO
                                     TBL-KOUI-SRYACTINF (WRK-TBLMAX1)
               MOVE    RECE45-NAMEINF (IDX)    TO
                                     TBL-KOUI-NAMEINF   (WRK-TBLMAX1)
           END-IF
           .
       2007111-KOUI-TAIHI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査（判断料）の場合の退避処理
      *****************************************************************
       20013-KENSA-HANDANRYO-TAIHI-SEC           SECTION.
      *
           ADD     1               TO  TBL-KENSA-RECE45-MAX
           MOVE    RECE45-KEY      TO  TBL-KENSA-RECE45-KEY
                           (TBL-KENSA-RECE44-MAX TBL-KENSA-RECE45-MAX)
           MOVE    IDX             TO  TBL-KENSA-RECE45-SHORI-RENNUM
                           (TBL-KENSA-RECE44-MAX TBL-KENSA-RECE45-MAX)
           MOVE    RECE45-SRYACTINF (IDX)
                                   TO  TBL-KENSA-RECE45-SRYACTINF
                           (TBL-KENSA-RECE44-MAX TBL-KENSA-RECE45-MAX)
           MOVE    RECE45-NAMEINF (IDX)
                                   TO  TBL-KENSA-RECE45-NAMEINF
                           (TBL-KENSA-RECE44-MAX TBL-KENSA-RECE45-MAX)
           IF      RECE45-SRYCD (IDX) (1:1)  =   "1"
               MOVE    TBL-KENSA-RECE45-MAX 
                                       TO  TBL-KENSA-SRYCD-MAX
                                                (TBL-KENSA-RECE44-MAX)
           ELSE
               IF      RECE45-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE45-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                       TBL-KENSA-RECE45-SRYCD
                           (TBL-KENSA-RECE44-MAX TBL-KENSA-RECE45-MAX)
                   END-IF
      *            コメント
                   IF    ( RECE45-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE45-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE45-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE45-INPUTCOMENT (IDX)     TO
                                       TBL-KENSA-RECE45-NAME
                           (TBL-KENSA-RECE44-MAX TBL-KENSA-RECE45-MAX)
                   END-IF
               END-IF
           END-IF
      *
           .
       20013-KENSA-HANDANRYO-TAIHI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    摘要データ（医薬品）退避処理
      *****************************************************************
       200711-YKZ-TAIHI-SEC          SECTION.
      *
           IF      RECE45-SRYCD (IDX)  =   WRK-CONS-TEIGEN
                                       OR  WRK-CONS-TEIGEN2
                                       OR  WRK-CONS-TEIGEN3
                                       OR  WRK-CONS-TEIGEN4
               CONTINUE
           ELSE
               IF    ( RECE44-YKZTEN       =   ZERO  )
               AND   ( RECE44-TOYAKUFLG    =   SPACE )
               AND   ( RECE44-YKZKISAIFLG  =   SPACE )
                   GO  TO  200711-YKZ-TAIHI-EXT
               END-IF
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX6
      *
           IF      WRK-TBLMAX6         =   1
               IF      RECE45-SRYCD (IDX)
                                       =   WRK-CONS-TEIGEN
                                       OR  WRK-CONS-TEIGEN2
                                       OR  WRK-CONS-TEIGEN3
                                       OR  WRK-CONS-TEIGEN4
                   MOVE    1               TO  FLG-TEIGEN
               END-IF
           END-IF
      *     
           IF      WRK-TBLMAX6         >   100
               MOVE    "*** ORCR0555 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               MOVE    RECE45-KEY          TO
                                   TBL-ZAIW-KEY        (1 WRK-TBLMAX6)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM
                                                       (1 WRK-TBLMAX6)
               MOVE    RECE45-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (1 WRK-TBLMAX6)
               MOVE    RECE45-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (1 WRK-TBLMAX6)
               MOVE    RECE45-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (1 WRK-TBLMAX6)
               MOVE    RECE45-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(1 WRK-TBLMAX6)
               MOVE    RECE45-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (1 WRK-TBLMAX6)
               MOVE    RECE45-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (1 WRK-TBLMAX6)
               MOVE    RECE45-NAME       (IDX)  TO
                                   TBL-ZAIW-NAME       (1 WRK-TBLMAX6)
               MOVE    RECE45-YUKOKETA   (IDX)  TO
                                   TBL-ZAIW-YUKOKETA   (1 WRK-TBLMAX6)
           END-IF
           .
       200711-YKZ-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（その他）退避処理
      *****************************************************************
       200711-KIZAI-TAIHI-SEC          SECTION.
      *
           IF      RECE44-SRYKBN    NOT =   "70"
               IF    ( RECE44-KIZAITEN  =   ZERO  )
               AND   ( RECE44-TOYAKUFLG =   SPACE )
                   GO  TO  200711-KIZAI-TAIHI-EXT
               END-IF
           END-IF
      *
      *    酸素補正率１．３（１気圧）のとき
           IF     RECE45-SRYCD (IDX)    =   WRK-CONS-SANSO-HOSEI
               GO  TO  200711-KIZAI-TAIHI-EXT
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX7
      *     
           IF      WRK-TBLMAX7         >   100
               MOVE    "*** ORCR0555 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               MOVE    RECE45-KEY          TO
                                   TBL-ZAIW-KEY        (2 WRK-TBLMAX7)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM(2 WRK-TBLMAX7)
               MOVE    RECE45-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
               MOVE    RECE45-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
               MOVE    RECE45-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (2 WRK-TBLMAX7)
               MOVE    RECE45-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(2 WRK-TBLMAX7)
               MOVE    RECE45-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (2 WRK-TBLMAX7)
               MOVE    RECE45-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (2 WRK-TBLMAX7)
               MOVE    RECE45-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF    (2 WRK-TBLMAX7)
               MOVE    RECE45-RECEDENNAME(IDX)  TO
                                   TBL-ZAIW-RECEDENNAME(2 WRK-TBLMAX7)
      *
               IF    ( RECE45-DATAKBN (IDX)
                                           =   "3"  )
               AND   ( RECE44-SRYKBN      =   "70" )
                   MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX 
               ELSE
                   IF       RECE45-SRYCD (IDX)
                                              =   "799990070"
                       MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX 
                   ELSE
                       MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX 
                   END-IF
               END-IF
           END-IF
      *
      *    酸素のとき補正率を編集
           IF    ( RECE45-SRYCD (IDX)  =   WRK-CONS-SANSO1
                                       OR  WRK-CONS-SANSO2
                                       OR  WRK-CONS-SANSO3
                                       OR  WRK-CONS-SANSO4
                                       OR  WRK-CONS-SANSO5
                                       OR  WRK-CONS-SANSO6
                                       OR  WRK-CONS-SANSO7
                                       OR  WRK-CONS-SANSO8 )
               ADD     1                   TO  WRK-TBLMAX7
      *     
               IF      WRK-TBLMAX7         >   100
                   MOVE    "*** ORCR0555 TABLE 100 OVER  ***"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               ELSE
                   MOVE    WRK-CONS-SANSO-HOSEI TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
                   MOVE    1                   TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
                   MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX
               END-IF
           END-IF
           .
       200711-KIZAI-TAIHI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    摘要データ（その他）特定器材退避処理
      *****************************************************************
       200711-KIZAI-ACCT-TAIHI-SEC          SECTION.
      *
           IF      RECE44-SRYKBN    NOT =   "70"
               IF    ( RECE44-KIZAITEN  =   ZERO  )
               AND   ( RECE44-TOYAKUFLG =   SPACE )
                   GO  TO  200711-KIZAI-ACCT-TAIHI-EXT
               END-IF
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX7-SRT
      *     
           IF      WRK-TBLMAX7-SRT     >   100
               MOVE    "*** ORCR0555 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               MOVE    RECE45-KEY          TO
                                   TBL-ZAIW-KEY      (3 WRK-TBLMAX7-SRT)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD    (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD   (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-TEN        (IDX) TO
                                   TBL-ZAIW-TEN      (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN  (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF  (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-TOKUKIZAISBT1 (IDX)  TO
                                   TBL-ZAIW-TOKUKIZAISBT1
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-SANSOKBN  (IDX)  TO
                                   TBL-ZAIW-SANSOKBN (3 WRK-TBLMAX7-SRT)
               MOVE    RECE45-RECEDENNAME  (IDX)   TO
                                   TBL-ZAIW-RECEDENNAME
                                                     (3 WRK-TBLMAX7-SRT)
           END-IF
           .
       200711-KIZAI-ACCT-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）並び替え処理
      *****************************************************************
       20074-ZAI-KOUI-SORT-SEC         SECTION.
      *
           INITIALIZE                  WRK-KOUI-AREA
                                       WRK-KOUI-TAIHI-AREA
                                       WRK-KOUI-KASAN-TAIHI-AREA
                                       WRK-KOUI-SRY-TAIHI-AREA
                                       WRK-SRYKBN40-KOKUJISKBKBN1
      *
           IF      RECE44-SRYKBN       =   "50"    OR  "54"    OR
                                           "60"    OR  "40"    OR
                                           "80"    OR  "33"    OR
                                           "70"    OR  "11"    OR
                                           "12"    OR  "13"    OR
                                           "14"
               CONTINUE
           ELSE
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           IF      WRK-TBLMAX1         =   1
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           MOVE    ZERO                TO  IDY
                                           IDWW
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-TBLMAX1
      *
      *        加減算コード（通則加算項目）があるか
               IF    ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                           =   "9"         )
               AND   ( TBL-KOUI-TENSIKIBETU   (IDX)
                                           =   "5" OR  "6" )
                   ADD     1                   TO  IDWW
                   MOVE    TBL-KOUI-REC  (IDX) TO
                                   WRK-KOUI-KASAN-TAIHI-REC (IDWW)
                   MOVE    TBL-KOUI-TIMEKSNKBN    (IDX)
                                               TO
                                   WRK-KOUI-KASAN-TAIHI-TIMEKSNKBN
                                                            (IDWW)
               END-IF
      *
      *        処置の加算項目の有無                
               IF    ( RECE44-SRYKBN      =   "40" )
               AND   ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                           =   "7"  )
                   MOVE    1                   TO
                                   WRK-SRYKBN40-KOKUJISKBKBN1
               END-IF
      *
      *        コメントを最初に編集（手術のとき）
               IF      RECE44-SRYKBN      =   "50"
                   EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                       WHEN    SPACE
                           CONTINUE
                       WHEN    "0"
                       WHEN    "8" 
                           ADD     1                           TO  IDY
                           MOVE    TBL-KOUI-REC          (IDX) TO
                                           WRK-KOUI-TAIHI-REC    (IDY)
                           INITIALIZE      TBL-KOUI-REC (IDX)
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    基本項目毎に並び替えをする
           MOVE    1                   TO  IDX
           PERFORM UNTIL   IDX >   WRK-TBLMAX1
             MOVE    ZERO                TO  IDYY
             INITIALIZE                  WRK-KOUI-SRY-TAIHI-AREA
             PERFORM UNTIL   IDX >   WRK-TBLMAX1
               MOVE    SPACE               TO  WRK-CHUKSNCD
               MOVE    ZERO                TO  FLG-KOUI
               PERFORM UNTIL   IDX >   WRK-TBLMAX1
                 IF    ( TBL-KOUI-SRYCD (IDX)  =   SPACE       )
                 OR    ( TBL-KOUI-SRYCD (IDX) (1:1)
                                               =   "0" OR  "8" )
                   COMPUTE IDX     =   IDX     +   1
                 ELSE
                   EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                             ALSO    TBL-KOUI-KOKUJISKBKBN1 (IDX)
                     WHEN    SPACE   ALSO    ANY
                     WHEN    "0"     ALSO    ANY
                     WHEN    "8"     ALSO    ANY
                         CONTINUE
                     WHEN    ANY     ALSO    "1"
                     WHEN    ANY     ALSO    "3"
                     WHEN    ANY     ALSO    "5"
                       IF      WRK-CHUKSNCD        =   SPACE
                           MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-CHUKSNCD
                           ADD     1                           TO  IDYY
                           MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-REC     (IDYY)
                           MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-SRYCD   (IDYY)
                           MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-TENSIKIBETU
                                                                  (IDYY)
                           MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-CHUKSNCD(IDYY)
                           MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-SRY-TAIHI-KOKUJISKB1
                                                                  (IDYY)
                           MOVE    TBL-KOUI-TUSOKUGAIKBN(IDX) TO
                                       WRK-KOUI-SRY-TAIHI-TUSOKUGAIKBN
                                                                  (IDYY)
                           MOVE    TBL-KOUI-TIMEKSNKBN(IDX) TO
                                       WRK-KOUI-SRY-TAIHI-TIMEKSNKBN
                                                                  (IDYY)
                           INITIALIZE      TBL-KOUI-REC (IDX)
                       ELSE
                           MOVE    1               TO  FLG-KOUI
                       END-IF    
                     WHEN    OTHER
                       ADD     1                           TO  IDYY
                       MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-REC     (IDYY)
                       MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-SRYCD   (IDYY)
                       MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-TENSIKIBETU
                                                                  (IDYY)
                       MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-SRY-TAIHI-CHUKSNCD(IDYY)
                       MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-SRY-TAIHI-KOKUJISKB1
                                                                  (IDYY)
                       MOVE    TBL-KOUI-TUSOKUGAIKBN(IDX) TO
                                       WRK-KOUI-SRY-TAIHI-TUSOKUGAIKBN
                                                                  (IDYY)
                       MOVE    TBL-KOUI-TIMEKSNKBN(IDX) TO
                                       WRK-KOUI-SRY-TAIHI-TIMEKSNKBN
                                                                  (IDYY)
                       INITIALIZE      TBL-KOUI-REC (IDX)
                   END-EVALUATE
      *    
                   COMPUTE IDX     =   IDX     +   1
                 END-IF
      *    
                 IF    ( IDX       <=  WRK-TBLMAX1 )
                 AND   ( IDYY      >   ZERO        )
                     IF    ( TBL-KOUI-SRYCD (IDX) (1:1)
                                   =   "1"                 )
                     AND   ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                   =   "1" OR  "3" OR  "5" )
                     AND   ( TBL-KOUI-KOKUJISKBKBN2 (IDX)
                                   =   "1" OR  "3" OR  "5" )
                         ADD     WRK-TBLMAX1       TO  IDX
                         IF      FLG-KOUI          =   ZERO
                             MOVE    9                 TO  FLG-KOUI
                         END-IF 
                     END-IF
                 END-IF
               END-PERFORM
      *
      *        出力対象となっていないデータがある 
               IF      FLG-KOUI            =   1
                   MOVE    1                   TO  IDX
               END-IF
             END-PERFORM
      *      摘要データ（診療行為）基本項目毎の並び替え処理
             PERFORM 200741-KIHON-HENSYU-SEC
      *      データが残っているとき 
             IF      FLG-KOUI            =   9
                 MOVE    1                   TO  IDX
             END-IF
           END-PERFORM
      *
      *    コメントを最後に編集
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       WRK-TBLMAX1
               EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                   WHEN    SPACE
                       CONTINUE
                   WHEN    "0"
                   WHEN    "8"
                       ADD     1                           TO  IDY
                       MOVE    TBL-KOUI-REC          (IDX) TO
                                               WRK-KOUI-TAIHI-REC (IDY)
                 END-EVALUATE
            END-PERFORM           
      *
           INITIALIZE                      TBL-KOUI-AREA
           MOVE    ZERO                TO  WRK-TBLMAX1
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       100
                   OR      WRK-KOUI-TAIHI-REC (IDX)  =   SPACE
               ADD     1           TO  WRK-TBLMAX1   
               MOVE    WRK-KOUI-TAIHI-REC (IDX)
                                   TO  TBL-KOUI-REC(WRK-TBLMAX1)
           END-PERFORM
           .
       20074-ZAI-KOUI-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）基本項目毎の並び替え処理
      *****************************************************************
       200741-KIHON-HENSYU-SEC          SECTION.
      *
           MOVE    1                   TO  IDZZ
           PERFORM UNTIL   IDZZ    >=  100
                   OR      WRK-KOUI-SRY-TAIHI-SRYCD (IDZZ)  =   SPACE
               EVALUATE    WRK-KOUI-SRY-TAIHI-SRYCD (IDZZ) (1:1)
                   WHEN    "0"
                   WHEN    "8"
                       IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (5) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (6) >  ZERO )
                           PERFORM 200741-KASAN-HENSYU-SEC
                       END-IF
                       ADD     1           TO  IDY   
                       MOVE    WRK-KOUI-SRY-TAIHI-REC (IDZZ)
                                           TO  WRK-KOUI-TAIHI-REC (IDY)
                       COMPUTE IDZZ    =   IDZZ    +   1
                   WHEN    OTHER
      *                基本項目・加算項目・通則加算項目に分ける
                       PERFORM UNTIL   IDZZ    >=      100
                               OR      WRK-KOUI-SRY-TAIHI-SRYCD (IDZZ)
                                                       =   SPACE
                               OR      WRK-KOUI-SRY-TAIHI-SRYCD (IDZZ)
                                                                  (1:1)
                                                   NOT =   "1"
                           MOVE    ZERO            TO  IDZ
                           EVALUATE    WRK-KOUI-SRY-TAIHI-KOKUJISKB1
                                                                (IDZZ)
                               WHEN    "1"
                               WHEN    "3" 
                               WHEN    "5"
                                   IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (5) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (6) >  ZERO )
                                       PERFORM 200741-KASAN-HENSYU-SEC
                                   END-IF
                                   MOVE    1               TO  IDZ  
                               WHEN    "7"
                                   IF  WRK-KOUI-SRY-TAIHI-TUSOKUGAIKBN
                                                               (IDZZ)
                                                           =   "1"
                                     MOVE    4               TO  IDZ  
                                   ELSE
                                     IF      WRK-KOUI-SRY-TAIHI-SRYCD
                                                                 (IDZZ)
                                                           =
                                         "150268970"                  OR
                                         WRK-CONS-SISETUKIJYUN-KENSA  OR
                                         WRK-CONS-SISETUKIJYUN-GAZOU  OR
                                         WRK-CONS-SISETUKIJYUN-HOUSYASEN
                                       MOVE    2               TO  IDZ  
                                     ELSE
                                      IF
                                        ( WRK-KOUI-SRY-TAIHI-TENSIKIBETU 
                                                               (IDZZ)
                                                         =   5   OR  6 )
                                      AND
                                        ( WRK-KOUI-SRY-TAIHI-CHUKSNCD
                                                                 (IDZZ)
                                                         =   "0000"    )
                                      AND   ( RECE44-SRYKBN
                                                     NOT =   "54"      ) 
                                       MOVE    3               TO  IDZ
                                      ELSE
                                       MOVE    2               TO  IDZ
                                      END-IF
                                     END-IF
                                   END-IF   
                               WHEN    "9" 
                                   IF   ( WRK-KOUI-SRY-TAIHI-TENSIKIBETU 
                                                               (IDZZ)
                                           =   "5" OR  "6" )
                                       MOVE    ZERO        TO  IDZ
                                   ELSE
                                       MOVE    5           TO  IDZ
                                   END-IF  
                               WHEN    "8" 
                                   MOVE    6           TO  IDZ
                           END-EVALUATE
                           IF      IDZ         >   ZERO
                               ADD     1           TO
                                               WRK-KOUI-TBLMAX (IDZ)
                               MOVE    WRK-KOUI-TBLMAX (IDZ)
                                                   TO  IDYY
                               MOVE    WRK-KOUI-SRY-TAIHI-REC (IDZZ)
                                                   TO
                                               WRK-KOUI-REC (IDZ IDYY)
                           END-IF
                           COMPUTE IDZZ    =   IDZZ    +   1
                       END-PERFORM
               END-EVALUATE
           END-PERFORM
      *               
           IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (5) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (6) >  ZERO )
               PERFORM 200741-KASAN-HENSYU-SEC
           END-IF
           .
       200741-KIHON-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       200741-KASAN-HENSYU-SEC          SECTION.
      *
      *    注加算項目の記録順
           IF      WRK-KOUI-TBLMAX (2)         >   ZERO
               COMPUTE IDYY    =   WRK-KOUI-TBLMAX (2) -   1
               PERFORM VARYING IDY1    FROM    1   BY  1
                       UNTIL   IDY1    >       IDYY
                   COMPUTE IDZ     =   IDY1    +   1
                   PERFORM VARYING IDY2    FROM   IDZ  BY  1
                           UNTIL   IDY2    >   WRK-KOUI-TBLMAX (2)
                       IF      WRK-KOUI-CHUKSNINF (2 IDY1)
                                       > WRK-KOUI-CHUKSNINF (2 IDY2)
                           MOVE    WRK-KOUI-REC (2 IDY1)  TO
                                                 TBL-KOUI-SORT
                           MOVE    WRK-KOUI-REC (2 IDY2)  TO
                                                 WRK-KOUI-REC (2 IDY1)
                           MOVE    TBL-KOUI-SORT         TO
                                                 WRK-KOUI-REC (2 IDY2)
                       END-IF
                   END-PERFORM                              
               END-PERFORM
           END-IF    
      *
      *    基本項目・注加算項目・通則加算項目の順に編集
           MOVE    ZERO            TO  WRK-KIHON-TIMEKSNKBN
      *
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       5
               MOVE    WRK-KOUI-TBLMAX (IDY1)  TO  IDYY
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       IDYY
      *            基本項目の時間外加算区分を退避
                   IF    ( IDY1            =   1 ) 
                   AND   ( IDY2            =   1 )
                       MOVE    WRK-KOUI-TIMEKSNKBN  (IDY1 IDY2)
                                           TO  WRK-KIHON-TIMEKSNKBN
                   END-IF
      * 
                   ADD     1               TO  IDY
                   MOVE    WRK-KOUI-REC (IDY1 IDY2)
                                           TO  WRK-KOUI-TAIHI-REC  (IDY)
                   MOVE    IDY             TO  WRK-SRYCD1-MAX
               END-PERFORM
           END-PERFORM
      *
      *    加減算コードの通則加算項目の編集
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       IDWW
      *        時間外加算区分による編集有無チェック
               IF    ( WRK-KIHON-TIMEKSNKBN    =   ZERO )
               AND   ( WRK-KOUI-KASAN-TAIHI-TIMEKSNKBN (IDY1)
                                           NOT =   ZERO )
      *            時間外区分算定外のとき加算項目があれば編集（処置）
                   IF      WRK-SRYKBN40-KOKUJISKBKBN1  =   1
                       ADD     1               TO  IDY
                       MOVE    WRK-KOUI-KASAN-TAIHI-REC (IDY1)
                                           TO  WRK-KOUI-TAIHI-REC  (IDY)
                       MOVE    IDY         TO  WRK-SRYCD1-MAX
                   END-IF
               ELSE 
                   ADD     1               TO  IDY
                   MOVE    WRK-KOUI-KASAN-TAIHI-REC (IDY1)
                                           TO  WRK-KOUI-TAIHI-REC  (IDY)
                   MOVE    IDY             TO  WRK-SRYCD1-MAX
               END-IF    
           END-PERFORM
      *
      *    労災特別加算項目の編集
           MOVE    6                       TO  IDY1
           MOVE    WRK-KOUI-TBLMAX (IDY1)  TO  IDYY
           PERFORM VARYING IDY2    FROM    1   BY  1
                   UNTIL   IDY2    >       IDYY
               ADD     1               TO  IDY
               MOVE    WRK-KOUI-REC (IDY1 IDY2)
                                       TO  WRK-KOUI-TAIHI-REC  (IDY)
               MOVE    IDY             TO  WRK-SRYCD1-MAX
           END-PERFORM
      *
           INITIALIZE                   WRK-KOUI-AREA
           .
       200741-KASAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       20072-ZAI-HENSYU-SEC          SECTION.
      *
      *    コメントデータの編集（診療区分７０）
           IF      RECE44-SRYKBN      =   "70"                 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX8
                   PERFORM 200724-COMMENT-HENSYU-SEC
               END-PERFORM  
           END-IF          
      *     
      *    診療行為データ・コメントデータの編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-TBLMAX1
      *
               INITIALIZE                  TBL-RECE455-COMMENT-AREA
      *
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
      *
               EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                   WHEN    "0"
                   WHEN    "8"
                       MOVE    "CO,"               TO  WRK-REC
                       MOVE    3                   TO  WRK-REC-LENGTH
      *                診療識別
                       IF    ( IDX                 =   1     )
                       AND   ( WRK-SRYKBN      NOT =   SPACE )
                           MOVE    WRK-SRYKBN          TO  WRK-DATA
                           MOVE    2                   TO  WRK-NAGASA
                           MOVE    SPACE               TO  WRK-SRYKBN
                       END-IF    
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                予備１
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード
                       MOVE    TBL-KOUI-SRYCD (IDX)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ
                       MOVE    TBL-KOUI-NAME(IDX)  TO  WRK-DATA
                       MOVE    76                  TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                       MOVE    1                   TO  WRK-END 
                       PERFORM 5004-MOJI-HENSYU-SEC
      *
                       IF      TBL-KOUI-NAME(IDX) (77:)
                                               NOT =   SPACE
                           PERFORM 5001-FILE-WRITE-SEC
      *
                           MOVE    TBL-KOUI-SRYCD (IDX)
                                                   TO  WRK-CO-SRYCD
                           MOVE    TBL-KOUI-NAME  (IDX) (77:)
                                                   TO  WRK-CO-COMMENT
                           PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
                       END-IF
                   WHEN    OTHER
      *                関連コメント退避処理
                       IF      RECE44-SETCOMFLG   =   "1"
                           INITIALIZE               WRK-RECE455-REC
                           MOVE    TBL-KOUI-KEY (IDX)
                                                TO  WRK-RECE455-KEY1
                           MOVE    TBL-KOUI-SHORI-RENNUM (IDX)
                                                TO
                                                WRK-RECE455-SHORI-RENNUM
                           MOVE    ZERO         TO  WRK-RECE455-SEQNO
                           PERFORM 200799-KANREN-COM-TAIHI-SEC
                       END-IF
      *  
                       MOVE    "RI,"               TO  WRK-REC
                       MOVE    3                   TO  WRK-REC-LENGTH
      *                診療識別
                       IF    ( IDX                 =   1     )
                       AND   ( WRK-SRYKBN      NOT =   SPACE )
                           MOVE    WRK-SRYKBN          TO  WRK-DATA
                           MOVE    2                   TO  WRK-NAGASA
                           MOVE    SPACE               TO  WRK-SRYKBN
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                診療行為コード
                       MOVE    TBL-KOUI-SRYCD (IDX)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                数量
                       IF      TBL-KOUI-SRYSURYO (IDX) >   ZERO
                               IF    ( TBL-KOUI-SRYCD (IDX) (1:1)
                                                       =   "1"        )
                               AND   ( TBL-KOUI-KZMCOMPSIKIBETU (IDX)
                                                   NOT =   "1"        )
                                   CONTINUE
                               ELSE
                                   MOVE    TBL-KOUI-SRYSURYO (IDX)
                                                       TO  WRK-SURYO
                                   MOVE    WRK-SURYO   TO  WRK-NUM
                                   MOVE    8           TO  WRK-NAGASA
                               END-IF
                       END-IF    
                       PERFORM 5005-NUM-HENSYU-SEC  
      *                点数
                       IF    ( IDX                 =   WRK-SRYCD1-MAX )
                         IF      RECE44-ZAI-KINGAKU    >   ZERO
                           IF      RECE44-ZAI-TENSU      >   ZERO
                               EVALUATE    RECE44-SRYKBN
                                   WHEN    "70"
                                       COMPUTE WRK-TENSU   =
                                               RECE44-ZAI-TENSU    -
                                               RECE44-SYUTEN2
                                   WHEN    "12"
                                   WHEN    "13"
                                   WHEN    "14"
                                       COMPUTE WRK-TENSU   =
                                           RECE44-ZAI-TENSU    -
                                           TBL-KOUI-SRYKBN12-TEN-G
                                                               -
                                           TBL-KOUI-SRYKBN12-CHU-TEN-G
                                   WHEN    OTHER  
                                       MOVE    RECE44-ZAI-TENSU
                                                       TO  WRK-TENSU
                               END-EVALUATE
                               MOVE    WRK-TENSU         TO  WRK-NUM
                               MOVE    7                 TO  WRK-NAGASA
                           END-IF
                         ELSE
                           IF      RECE44-SYUTEN1      >   ZERO
                               EVALUATE    RECE44-SRYKBN
                                   WHEN    "70"
                                       COMPUTE WRK-TENSU   =
                                               RECE44-SYUTEN1 -
                                               RECE44-SYUTEN2
                                   WHEN    "12"
                                   WHEN    "13"
                                   WHEN    "14"
                                       COMPUTE WRK-TENSU   =
                                           RECE44-SYUTEN1      -
                                           TBL-KOUI-SRYKBN12-TEN-G
                                                               -
                                           TBL-KOUI-SRYKBN12-CHU-TEN-G
                                   WHEN    OTHER  
                                       MOVE    RECE44-SYUTEN1
                                                       TO  WRK-TENSU
                               END-EVALUATE
                               MOVE    WRK-TENSU         TO  WRK-NUM
                               MOVE    7                 TO  WRK-NAGASA
                           END-IF
                         END-IF 
                       END-IF    
                       PERFORM 5005-NUM-HENSYU-SEC  
      *                金額
                       IF    ( IDX                 =   WRK-SRYCD1-MAX )
                       AND   ( RECE44-ZAI-KINGAKU  >   ZERO           )
                           MOVE    RECE44-ZAI-KINGAKU  TO  WRK-NUM
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF    
                       PERFORM 5005-NUM-HENSYU-SEC  
      *                回数
                       MOVE    RECE44-ZAIKAISU    TO  WRK-KAISU
                       MOVE    WRK-KAISU           TO  WRK-NUM
                       MOVE    3                   TO  WRK-NAGASA
                       PERFORM 5005-NUM-HENSYU-SEC
      *                コメントコード１
                       IF      TBL-RECE455-MAX >   ZERO
                           MOVE    TBL-RECE455-SRYCD (1)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ１
                       IF      TBL-RECE455-MAX >   ZERO
                           MOVE    TBL-RECE455-NAME  (1)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード２
                       IF      TBL-RECE455-MAX >   1
                           MOVE    TBL-RECE455-SRYCD (2)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ２
                       IF      TBL-RECE455-MAX >   1
                           MOVE    TBL-RECE455-NAME  (2)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード３
                       IF      TBL-RECE455-MAX >   2
                           MOVE    TBL-RECE455-SRYCD (3)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ３
                       IF      TBL-RECE455-MAX >   2
                           MOVE    TBL-RECE455-NAME  (3)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                日付情報
                       PERFORM VARYING IDV     FROM    1   BY  1
                               UNTIL   IDV     >       31
                           IF      RECE44-KAISU (1 IDV)  >   ZERO
                               MOVE    RECE44-KAISU (1 IDV)
                                                       TO  WRK-NUM
                               MOVE    3               TO  WRK-NAGASA
                           END-IF
                           IF      IDV                 =   31
                               MOVE    1                   TO  WRK-END 
                           END-IF     
                           PERFORM 5005-NUM-HENSYU-SEC
                       END-PERFORM  
               END-EVALUATE  
      *
               PERFORM 5001-FILE-WRITE-SEC
      *
      *        関連コメント編集処理
               PERFORM 200798-KANREN-COM-HEN-SEC
           END-PERFORM
      *
      *    加算項目扱いの基本項目を編集
           IF      TBL-KOUI-SRYKBN12-MAX   >   ZERO
               PERFORM 20079-SRYKBN12-JIKANGAIHEN-SEC
           END-IF
      *
      *    コメントデータの編集（診療区分１１〜１４）
           IF    ( RECE44-SRYKBN       =   "11"    OR  "12"    OR
                                           "13"    OR  "14" ) 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX8
                   PERFORM 200724-COMMENT-HENSYU-SEC
               END-PERFORM  
           END-IF          
      *
      *    医薬品データの編集                 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-TBLMAX6
               PERFORM 200722-YKZ-HENSYU-SEC
           END-PERFORM            
      *
      *    コメントデータの編集（診療区分２３）
           IF    ( RECE44-SRYKBN       =   "23"  )
           AND   ( RECE44-SIPPUFLG NOT =   SPACE )
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX8
                   PERFORM 200724-COMMENT-HENSYU-SEC
               END-PERFORM
           END-IF
      *
      *    特定器材データの編集
           IF      RECE44-ZAIREQFLG       =   SPACE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       2
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-TBLMAX7
                       IF      IDXX        =   TBL-ZAIW-DATSKB (2 IDX)
                           PERFORM 200723-KIZAI-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
           ELSE
               INITIALIZE                      ORCSRECESORTAREA
               MOVE    RECE41-HOSPNUM      TO  ORCSRECESORT-HOSPNUM
               MOVE    RECE41-NYUGAIKBN    TO  ORCSRECESORT-NYUGAIKBN
               MOVE    RECE41-PTID         TO  ORCSRECESORT-PTID
               MOVE    RECE41-SRYYM        TO  ORCSRECESORT-SRYYM
               MOVE    RECE44-SRYKBN       TO  ORCSRECESORT-SRYKBN
               MOVE    RECE44-ZAINUM       TO  ORCSRECESORT-ZAINUM
               MOVE    RECE44-KIZAITEN     TO  ORCSRECESORT-KIZAITEN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-TBLMAX7-SRT
                   MOVE    TBL-ZAIW-HOSPNUM       (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-HOSPNUM   (IDX)
                   MOVE    TBL-ZAIW-SRYYM         (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-SRYYM     (IDX)
                   MOVE    TBL-ZAIW-NYUGAIKBN     (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-NYUGAIKBN (IDX)
                   MOVE    TBL-ZAIW-PTID          (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-PTID      (IDX)
                   MOVE    TBL-ZAIW-RECEKA        (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-RECEKA    (IDX)
                   MOVE    TBL-ZAIW-HKNID         (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-KOHID     (IDX)
                   MOVE    TBL-ZAIW-TEKSTYMD      (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-TEKSTYMD  (IDX)
                   MOVE    TBL-ZAIW-ZAINUM        (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-ZAINUM    (IDX)
                   MOVE    TBL-ZAIW-RENNUM        (3 IDX)  TO
                                   ORCSRECESORT-IN-KEY-RENNUM    (IDX)
                   MOVE    TBL-ZAIW-SHORI-RENNUM  (3 IDX)  TO
                                   ORCSRECESORT-IN-SHORI-RENNUM  (IDX)
                   MOVE    TBL-ZAIW-SRYCD         (3 IDX)  TO
                                   ORCSRECESORT-IN-SRYCD         (IDX)
                   MOVE    TBL-ZAIW-DATAKBN       (3 IDX)  TO
                                   ORCSRECESORT-IN-DATAKBN       (IDX)
                   MOVE    TBL-ZAIW-SRYSURYO      (3 IDX)  TO
                                   ORCSRECESORT-IN-SRYSURYO      (IDX)
                   MOVE    TBL-ZAIW-SRYKAISU      (3 IDX)  TO
                                   ORCSRECESORT-IN-SRYKAISU      (IDX)
                   MOVE    TBL-ZAIW-TANICD        (3 IDX)  TO
                                   ORCSRECESORT-IN-TANICD        (IDX)
                   MOVE    TBL-ZAIW-TEN           (3 IDX)  TO
                                   ORCSRECESORT-IN-TEN           (IDX)
                   MOVE    TBL-ZAIW-TENSIKIBETU   (3 IDX)  TO
                                   ORCSRECESORT-IN-TENSIKIBETU   (IDX)
                   MOVE    TBL-ZAIW-NAME          (3 IDX)  TO
                                   ORCSRECESORT-IN-NAME          (IDX)
                   MOVE    TBL-ZAIW-TOKUKIZAISBT1 (3 IDX)  TO
                                   ORCSRECESORT-IN-TOKUKIZAISBT1 (IDX)
                   MOVE    TBL-ZAIW-SANSOKBN      (3 IDX)  TO
                                   ORCSRECESORT-IN-SANSOKBN      (IDX)
                   MOVE    TBL-ZAIW-RECEDENNAME   (3 IDX)  TO
                                   ORCSRECESORT-IN-RECEDENNAME   (IDX)
               END-PERFORM
               CALL    "ORCSRECESORT"      USING   ORCSRECESORTAREA
                                                   SPA-AREA
               IF      ORCSRECESORT-ERRCD  =   0
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       8
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >       50
                               OR      ORCSRECESORT-SRYCD (IDXX IDX)
                                                       =   SPACE
                           PERFORM 200723-KIZAI-SORT-HENSYU-SEC
                       END-PERFORM
                   END-PERFORM
               ELSE
                   DISPLAY "ORCSRECESORT-ERRCD=" ORCSRECESORT-ERRCD
      *
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       2
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       WRK-TBLMAX7
                           IF      IDXX    =   TBL-ZAIW-DATSKB (2 IDX)
                               PERFORM 200723-KIZAI-HENSYU-SEC
                           END-IF
                       END-PERFORM
                   END-PERFORM
               END-IF    
           END-IF
           .
       20072-ZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（コメント）編集処理（診療区分１３・１４）
      *****************************************************************
       200724-COMMENT-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      IDX                 =   1
           AND     WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    予備１
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    TBL-COMMENT-SRYCD (IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    TBL-COMMENT-NAME(IDX)
                                       TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
           IF      TBL-COMMENT-NAME(IDX) (77:)
                                   NOT =   SPACE
               PERFORM 5001-FILE-WRITE-SEC
      *
               MOVE    TBL-COMMENT-SRYCD (IDX)
                                           TO  WRK-CO-SRYCD
               MOVE    TBL-COMMENT-NAME  (IDX) (77:)
                                           TO  WRK-CO-COMMENT
               PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
            END-IF
      *
            PERFORM 5001-FILE-WRITE-SEC
           .
       200724-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（医薬品）編集処理
      *****************************************************************
       200722-YKZ-HENSYU-SEC         SECTION.
      *
      *    関連コメント退避処理
           INITIALIZE              TBL-RECE455-COMMENT-AREA
           IF      RECE44-SETCOMFLG   =   "1"
               INITIALIZE               WRK-RECE455-REC
               MOVE    TBL-ZAIW-KEY (1 IDX)
                                           TO  WRK-RECE455-KEY1
               MOVE    TBL-ZAIW-SHORI-RENNUM (1 IDX)
                                           TO  WRK-RECE455-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE455-SEQNO
               PERFORM 200799-KANREN-COM-TAIHI-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "IY,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      IDX                 =   1
           AND     WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    予備１
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    医薬品コード
           MOVE    TBL-ZAIW-SRYCD (1 IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    使用量
           IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =   WRK-CONS-TEIGEN
                                       OR  WRK-CONS-TEIGEN2
                                       OR  WRK-CONS-TEIGEN3
                                       OR  WRK-CONS-TEIGEN4
               CONTINUE
           ELSE
               IF      TBL-ZAIW-SRYSURYO (1 IDX) >   ZERO
                   MOVE    TBL-ZAIW-SRYSURYO (1 IDX) TO  WRK-SRYSURYO
                   MOVE    WRK-SRYSURYO              TO  WRK-NUM
                   MOVE    11                        TO  WRK-NAGASA
                   MOVE    5                         TO  WRK-SYOSUU     
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    点数 
           IF      IDX                 =   WRK-TBLMAX6
               IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =   WRK-CONS-TEIGEN
                                       OR  WRK-CONS-TEIGEN2
                                       OR  WRK-CONS-TEIGEN3
                                       OR  WRK-CONS-TEIGEN4
                   MOVE    RECE44-ZAITEN      TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               ELSE
                   IF      RECE44-YKZTEN          >   ZERO
                   OR      RECE44-TOYAKUFLG   NOT =   SPACE
                   OR      RECE44-YKZKISAIFLG     =   "1"
                       MOVE    RECE44-YKZTEN      TO  WRK-NUM
                       MOVE    7                   TO  WRK-NAGASA
                   END-IF
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
           MOVE    RECE44-ZAIKAISU    TO  WRK-KAISU
           MOVE    WRK-KAISU           TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    コメントコード１
           IF      TBL-RECE455-MAX >   ZERO
               MOVE    TBL-RECE455-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE455-MAX >   ZERO
               MOVE    TBL-RECE455-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE455-MAX >   1
               MOVE    TBL-RECE455-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE455-MAX >   1
               MOVE    TBL-RECE455-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE455-MAX >   2
               MOVE    TBL-RECE455-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE455-MAX >   2
               MOVE    TBL-RECE455-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE44-KAISU (1 IDV)   >   ZERO
                   MOVE    RECE44-KAISU (1 IDV)   TO  WRK-NUM
                   MOVE    3                       TO  WRK-NAGASA
               END-IF
               IF      IDV                 =   31
                   MOVE    1                   TO  WRK-END 
               END-IF     
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    関連コメント編集処理
           PERFORM 200798-KANREN-COM-HEN-SEC
           .
       200722-YKZ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（特定機材）編集処理
      *****************************************************************
       200723-KIZAI-HENSYU-SEC         SECTION.
      *
      *    関連コメント退避処理
           INITIALIZE              TBL-RECE455-COMMENT-AREA
           IF      RECE44-SETCOMFLG    =   "1"
               INITIALIZE               WRK-RECE455-REC
               MOVE    TBL-ZAIW-KEY (2 IDX)
                                           TO  WRK-RECE455-KEY1
               MOVE    TBL-ZAIW-SHORI-RENNUM (2 IDX)
                                           TO  WRK-RECE455-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE455-SEQNO
               PERFORM 200799-KANREN-COM-TAIHI-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "TO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    予備１
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    特定器材コード
           MOVE    TBL-ZAIW-SRYCD (2 IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    使用量
           IF    ( TBL-ZAIW-SRYSURYO (2 IDX)   >   ZERO )
           AND   ( TBL-ZAIW-TENSIKIBETU (2 IDX)
                                           NOT =   "5"  )    
               MOVE    TBL-ZAIW-SRYSURYO (2 IDX)   TO  WRK-SRYSURYO
               MOVE    WRK-SRYSURYO                TO  WRK-NUM
               MOVE    9                           TO  WRK-NAGASA
               MOVE    3                           TO  WRK-SYOSUU
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    点数 
           IF    ( IDXX                =   1               )
           AND   ( IDX                 =   WRK-SRYCD71-MAX )
           AND   ( RECE44-SRYKBN      =   "70"            )
               MOVE    RECE44-SYUTEN2     TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF
           IF    ( IDXX                =   2               )
           AND   ( IDX                 =   WRK-SRYCD72-MAX )
               MOVE    RECE44-KIZAITEN    TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
           MOVE    RECE44-ZAIKAISU    TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    単位コード
           IF      TBL-ZAIW-TANICD (2 IDX)     >   ZERO
               MOVE    TBL-ZAIW-TANICD (2 IDX) TO  WRK-DATA
               MOVE    3                       TO  WRK-NAGASA
           END-IF        
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    単価
           IF    ( TBL-ZAIW-TENSIKIBETU (2 IDX)    =   2               )
           OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   "777770000"     )
           OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   WRK-CONS-SANSO1
                                                   OR  WRK-CONS-SANSO2
                                                   OR  WRK-CONS-SANSO3
                                                   OR  WRK-CONS-SANSO4
                                                   OR  WRK-CONS-SANSO5
                                                   OR  WRK-CONS-SANSO6
                                                   OR  WRK-CONS-SANSO7
                                                   OR  WRK-CONS-SANSO8 )
          
               IF      TBL-ZAIW-TEN (2 IDX)  >   ZERO
                   MOVE    TBL-ZAIW-TEN (2 IDX)  TO  WRK-TANKA
                   MOVE    WRK-TANKA             TO  WRK-NUM
                   MOVE    10                    TO  WRK-NAGASA
                   MOVE    2                     TO  WRK-SYOSUU
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    特定器材名称
           IF      TBL-ZAIW-SRYCD (2 IDX)        =   "777770000"
               MOVE    TBL-ZAIW-NAME (2 IDX)     TO  WRK-DATA
           END-IF
           MOVE    40                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    商品名及び規格名及びサイズ
           IF      TBL-ZAIW-RECEDENNAME (2 IDX)
                                       NOT =   SPACE
               MOVE    TBL-ZAIW-RECEDENNAME (2 IDX)
                                                 TO  WRK-DATA
           END-IF
           MOVE    300                 TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    コメントコード１
           IF      TBL-RECE455-MAX >   ZERO
               MOVE    TBL-RECE455-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE455-MAX >   ZERO
               MOVE    TBL-RECE455-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE455-MAX >   1
               MOVE    TBL-RECE455-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE455-MAX >   1
               MOVE    TBL-RECE455-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE455-MAX >   2
               MOVE    TBL-RECE455-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE455-MAX >   2
               MOVE    TBL-RECE455-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE44-KAISU (1 IDV)  >    ZERO
                   MOVE    RECE44-KAISU (1 IDV)   TO  WRK-NUM
                   MOVE    3                       TO  WRK-NAGASA
               END-IF
               IF      IDV                 =   31
                   MOVE    1                   TO  WRK-END 
               END-IF     
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    関連コメント編集処理
           PERFORM 200798-KANREN-COM-HEN-SEC
           .
       200723-KIZAI-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    摘要データ（再編集特定機材）編集処理
      *****************************************************************
       200723-KIZAI-SORT-HENSYU-SEC    SECTION.
      *
      *    関連コメント退避処理
           INITIALIZE              TBL-RECE455-COMMENT-AREA
           IF      RECE44-SETCOMFLG    =   "1"
               INITIALIZE               WRK-RECE455-REC
               MOVE    ORCSRECESORT-KEY-HOSPNUM  (IDXX IDX)
                                           TO  WRK-RECE455-HOSPNUM
               MOVE    ORCSRECESORT-KEY-SRYYM    (IDXX IDX)
                                           TO  WRK-RECE455-SRYYM
               MOVE    ORCSRECESORT-KEY-NYUGAIKBN(IDXX IDX)
                                           TO  WRK-RECE455-NYUGAIKBN
               MOVE    ORCSRECESORT-KEY-PTID     (IDXX IDX)
                                           TO  WRK-RECE455-PTID
               MOVE    ORCSRECESORT-KEY-RECEKA   (IDXX IDX)
                                           TO  WRK-RECE455-RECEKA
               MOVE    ORCSRECESORT-KEY-KOHID    (IDXX IDX)
                                           TO  WRK-RECE455-HKNID
               MOVE    ORCSRECESORT-KEY-TEKSTYMD (IDXX IDX)
                                           TO  WRK-RECE455-TEKSTYMD
               MOVE    ORCSRECESORT-KEY-ZAINUM   (IDXX IDX)
                                           TO  WRK-RECE455-ZAINUM
               MOVE    ORCSRECESORT-KEY-RENNUM   (IDXX IDX)
                                           TO  WRK-RECE455-RENNUM
               MOVE    ORCSRECESORT-SHORI-RENNUM (IDXX IDX)
                                           TO  WRK-RECE455-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE455-SEQNO
               PERFORM 200799-KANREN-COM-TAIHI-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
           MOVE    "TO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      WRK-SRYKBN      NOT =   SPACE     
               MOVE    WRK-SRYKBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    予備１
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    特定器材コード
           MOVE    ORCSRECESORT-SRYCD (IDXX IDX)
                                       TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    使用量
           IF    ( ORCSRECESORT-SRYSURYO (IDXX IDX)
                                       >   ZERO         )
           AND   ( ORCSRECESORT-TENSIKIBETU (IDXX IDX)
                                           NOT =   "5"  )    
               MOVE    ORCSRECESORT-SRYSURYO (IDXX IDX)
                                           TO  WRK-SRYSURYO
               MOVE    WRK-SRYSURYO        TO  WRK-NUM
               MOVE    9                   TO  WRK-NAGASA
               MOVE    3                   TO  WRK-SYOSUU
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    点数 
           IF      ORCSRECESORT-TBLMAX (IDXX)
                                       =   IDX
               MOVE    ORCSRECESORT-ZAITEN (IDXX)
                                           TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
           MOVE    RECE44-ZAIKAISU    TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    単位コード
           IF      ORCSRECESORT-TANICD (IDXX IDX)
                                       >   ZERO
               MOVE    ORCSRECESORT-TANICD (IDXX IDX)
                                               TO  WRK-DATA
               MOVE    3                       TO  WRK-NAGASA
           END-IF        
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    単価
           IF    ( ORCSRECESORT-TENSIKIBETU (IDXX IDX)
                                       =   2               )
           OR    ( ORCSRECESORT-SRYCD       (IDXX IDX)
                                       =   "777770000"     )
           OR    ( ORCSRECESORT-SRYCD       (IDXX IDX)
                                       =   WRK-CONS-SANSO1
                                       OR  WRK-CONS-SANSO2
                                       OR  WRK-CONS-SANSO3
                                       OR  WRK-CONS-SANSO4
                                       OR  WRK-CONS-SANSO5
                                       OR  WRK-CONS-SANSO6
                                       OR  WRK-CONS-SANSO7
                                       OR  WRK-CONS-SANSO8 )
               IF      ORCSRECESORT-TEN (IDXX IDX)  >   ZERO
                   MOVE    ORCSRECESORT-TEN (IDXX IDX)
                                                 TO  WRK-TANKA
                   MOVE    WRK-TANKA             TO  WRK-NUM
                   MOVE    10                    TO  WRK-NAGASA
                   MOVE    2                     TO  WRK-SYOSUU
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    特定器材名称
           IF      ORCSRECESORT-SRYCD (IDXX IDX)
                                       =   "777770000"
               MOVE    ORCSRECESORT-NAME (IDXX IDX)
                                           TO  WRK-DATA
           END-IF
           MOVE    40                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    商品名及び規格名及びサイズ
           IF      ORCSRECESORT-RECEDENNAME (IDXX IDX)
                                   NOT =   SPACE
               MOVE    ORCSRECESORT-RECEDENNAME (IDXX IDX)
                                               TO  WRK-DATA
           END-IF
           MOVE    300                 TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    コメントコード１
           IF      TBL-RECE455-MAX     >   ZERO
               MOVE    TBL-RECE455-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE455-MAX     >   ZERO
               MOVE    TBL-RECE455-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE455-MAX     >   1
               MOVE    TBL-RECE455-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE455-MAX     >   1
               MOVE    TBL-RECE455-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE455-MAX     >   2
               MOVE    TBL-RECE455-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE455-MAX     >   2
               MOVE    TBL-RECE455-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE44-KAISU (1 IDV)   >   ZERO
                   MOVE    RECE44-KAISU (1 IDV)    TO  WRK-NUM
                   MOVE    3                        TO  WRK-NAGASA
               END-IF
               IF      IDV                 =   31
                   MOVE    1                   TO  WRK-END 
               END-IF     
               PERFORM 5005-NUM-HENSYU-SEC
           END-PERFORM  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    関連コメント編集処理
           PERFORM 200798-KANREN-COM-HEN-SEC
           .
       200723-KIZAI-SORT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査に判断料がある場合の検査の編集処理
      *****************************************************************
       20078-KENSA-HANDANRYO-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDZ3 FROM    1   BY  1
                   UNTIL   IDZ3 >   TBL-KENSA-RECE44-MAX
               IF      TBL-KENSA4-ZAIKAISU (IDZ3)
                                           >   0
                   MOVE    TBL-KENSA-RECE44-REC (IDZ3)
                                               TO  RECE44-REC
                   MOVE    "60"                TO  WRK-SRYKBN
      *
      *            仮レセデータフラグ設定
                   IF      RECE44-TOYAKUFLG =   SPACE
                       MOVE    ZERO              TO  FLG-TOYAKUFLG
                   ELSE
                       MOVE    1                 TO  FLG-TOYAKUFLG
                   END-IF 
      *
      *            検査に判断料がある場合の検査の編集処理
                   PERFORM VARYING IDZ4    FROM    1   BY  1
                           UNTIL   IDZ4    >       50
                           OR      TBL-KENSA-RECE45-REC  (IDZ3 IDZ4)
                                                   =   SPACE
                       MOVE    TBL-KENSA-SRYCD-MAX (IDZ3) 
                                           TO  IDZ5
                       INITIALIZE      WRK-RECE45-REC
                       MOVE    TBL-KENSA-RECE45-KEY        (IDZ3 IDZ4)
                                           TO  WRK-RECE45-KEY
                       MOVE    TBL-KENSA-RECE45-SRYACTINF  (IDZ3 IDZ4)
                                           TO  WRK-RECE45-SRYACTINF (1)
                       MOVE    TBL-KENSA-RECE45-NAMEINF    (IDZ3 IDZ4)
                                           TO  WRK-RECE45-NAMEINF   (1)
                       PERFORM 200781-KENSA-HENSYU-SEC
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20078-KENSA-HANDANRYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査に判断料がある場合の検査の編集処理
      *****************************************************************
       200781-KENSA-HENSYU-SEC      SECTION.
      *
           INITIALIZE                  TBL-RECE455-COMMENT-AREA
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           EVALUATE    WRK-RECE45-SRYCD (1) (1:1)
               WHEN    "0"
               WHEN    "8"
                   MOVE    "CO,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF      WRK-SRYKBN      NOT =   SPACE
                       MOVE    WRK-SRYKBN          TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    SPACE               TO  WRK-SRYKBN
                   END-IF    
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            予備１
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード
                   MOVE    WRK-RECE45-SRYCD (1)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ
                   MOVE    WRK-RECE45-NAME (1) TO  WRK-DATA
                   MOVE    76                  TO  WRK-NAGASA
                   MOVE    1                   TO  WRK-MOJI-CHK
                   MOVE    1                   TO  WRK-END 
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *
                   IF      WRK-RECE45-NAME (1) (77:)
                                           NOT =   SPACE
                       PERFORM 5001-FILE-WRITE-SEC
      *
                       MOVE    WRK-RECE45-SRYCD (1)
                                                   TO  WRK-CO-SRYCD
                       MOVE    WRK-RECE45-NAME  (1) (77:)
                                                   TO  WRK-CO-COMMENT
                       PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
                   END-IF
               WHEN    OTHER     
      *            関連コメント退避処理
                   IF      RECE44-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE455-REC
                       MOVE    TBL-KENSA-RECE45-KEY (IDZ3 IDZ4)
                                            TO  WRK-RECE455-KEY1
                       MOVE    TBL-KENSA-RECE45-SHORI-RENNUM
                                                    (IDZ3 IDZ4)
                                            TO  WRK-RECE455-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE455-SEQNO
                       PERFORM 200799-KANREN-COM-TAIHI-SEC
                   END-IF
      *
                   MOVE    "RI,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF      WRK-SRYKBN      NOT =   SPACE
                       MOVE    WRK-SRYKBN          TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    SPACE               TO  WRK-SRYKBN
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            診療行為コード
                   MOVE    WRK-RECE45-SRYCD (1)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            数量
                   IF      WRK-RECE45-SRYSURYO (1)
                                           >   ZERO
                       IF    ( WRK-RECE45-SRYCD (1) (1:1)
                                               =   "1"        )
                       AND   ( WRK-RECE45-KZMCOMPSIKIBETU (1)
                                           NOT =   "1"        )
                           CONTINUE
                       ELSE
                           MOVE    WRK-RECE45-SRYSURYO (1)
                                               TO  WRK-SURYO
                           MOVE    WRK-SURYO   TO  WRK-NUM
                           MOVE    8           TO  WRK-NAGASA
                       END-IF
                   END-IF
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            点数
                   IF    ( IDZ4            =   IDZ5 )
                   AND   ( RECE44-SYUTEN1  >   ZERO )
                       MOVE    RECE44-SYUTEN1      TO  WRK-TENSU
                       MOVE    WRK-TENSU           TO  WRK-NUM
                       MOVE    7                   TO  WRK-NAGASA
                   END-IF    
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            金額
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            回数
                   MOVE    RECE44-ZAIKAISU     TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC
      *            コメントコード１
                   IF      TBL-RECE455-MAX >   ZERO
                       MOVE    TBL-RECE455-SRYCD (1)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE455-MAX >   ZERO
                       MOVE    TBL-RECE455-NAME  (1)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE455-MAX >   1
                       MOVE    TBL-RECE455-SRYCD (2)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE455-MAX >   1
                       MOVE    TBL-RECE455-NAME  (2)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE455-MAX >   2
                       MOVE    TBL-RECE455-SRYCD (3)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE455-MAX >   2
                       MOVE    TBL-RECE455-NAME  (3)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE44-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE44-KAISU (1 IDV)
                                                       TO  WRK-NUM
                           MOVE    3                   TO  WRK-NAGASA
                       END-IF
                       IF      IDV                 =   31
                           MOVE    1                   TO  WRK-END 
                       END-IF     
                       PERFORM 5005-NUM-HENSYU-SEC
                   END-PERFORM  
           END-EVALUATE  
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
      *    関連コメント編集処理
           PERFORM 200798-KANREN-COM-HEN-SEC
           .
       200781-KENSA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    加算項目扱いの基本項目の編集処理
      *****************************************************************
       20079-SRYKBN12-JIKANGAIHEN-SEC   SECTION.
      *
      *    診療行為データの編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       TBL-KOUI-SRYKBN12-MAX
               IF      TBL-KOUI-SRYKBN12-KAISU (IDX)   >   ZERO
      *            関連コメント退避処理
                   INITIALIZE              TBL-RECE455-COMMENT-AREA
                   IF      RECE44-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE455-REC
                       MOVE    TBL-KOUI-SRYKBN12-KEY (IDX)
                                                TO  WRK-RECE455-KEY1
                       MOVE    TBL-KOUI-SRYKBN12-SHORI-RENNUM (IDX)
                                                TO
                                                WRK-RECE455-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE455-SEQNO
                       PERFORM 200799-KANREN-COM-TAIHI-SEC
                   END-IF
      *
                   MOVE    SPACE               TO  WRK-REC
                   MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
                   MOVE    "RI,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF    ( IDX                 =   1     )
                   AND   ( WRK-SRYKBN      NOT =   SPACE )
                       MOVE    WRK-SRYKBN          TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    SPACE               TO  WRK-SRYKBN
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            診療行為コード
                   MOVE    TBL-KOUI-SRYKBN12-SRYCD (IDX)
                                                   TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            数量
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            点数
                   MOVE    TBL-KOUI-SRYKBN12-TEN (IDX)
                                               TO  WRK-TENSU
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            金額
      ********     MOVE    TBL-KOUI-SRYKBN12-TEN (IDX)
      *??                                      TO  WRK-TENSU
      *??          MOVE    WRK-TENSU           TO  WRK-NUM
      ********     MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            回数
                   MOVE    TBL-KOUI-SRYKBN12-KAISU (IDX)
                                               TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC
      *            コメントコード１
                   IF      TBL-RECE455-MAX >   ZERO
                       MOVE    TBL-RECE455-SRYCD (1)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE455-MAX >   ZERO
                       MOVE    TBL-RECE455-NAME  (1)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE455-MAX >   1
                       MOVE    TBL-RECE455-SRYCD (2)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE455-MAX >   1
                       MOVE    TBL-RECE455-NAME  (2)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE455-MAX >   2
                       MOVE    TBL-RECE455-SRYCD (3)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE455-MAX >   2
                       MOVE    TBL-RECE455-NAME  (3)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE44-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE44-KAISU (1 IDV)
                                                       TO  WRK-NUM
                           MOVE    3                   TO  WRK-NAGASA
                       END-IF
                       IF      IDV                 =   31
                           MOVE    1                   TO  WRK-END 
                       END-IF     
                       PERFORM 5005-NUM-HENSYU-SEC
                   END-PERFORM  
      *
                   PERFORM 5001-FILE-WRITE-SEC
      *
      *            関連コメント編集処理
                   PERFORM 200798-KANREN-COM-HEN-SEC
      *
      *            注加算コードが１４１９の加算項目を編集
                   IF    ( TBL-KOUI-SRYKBN12-CHUKSNCD (IDX)
                                                   =   "1419" )
                   AND   ( TBL-KOUI-SRYKBN12-CHU-MAX
                                                   >   ZERO   )
                       PERFORM 2007251-SRYKBN12-CHUKSN-HEN-SEC
                       MOVE    ZERO        TO  TBL-KOUI-SRYKBN12-CHU-MAX
                   END-IF
               END-IF
           END-PERFORM
           .
       20079-SRYKBN12-JIKANGAIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    注加算コードが１４１９の加算項目を編集処理
      *****************************************************************
       2007251-SRYKBN12-CHUKSN-HEN-SEC   SECTION.
      *
      *    診療行為データの編集
           PERFORM VARYING IDY4    FROM    1   BY  1
                   UNTIL   IDY4    >       TBL-KOUI-SRYKBN12-CHU-MAX
               IF      TBL-KOUI-SRYKBN12-CHU-KAISU (IDY4)
                                       >   ZERO
      *            関連コメント退避処理
                   INITIALIZE              TBL-RECE455-COMMENT-AREA
                   IF      RECE44-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE455-REC
                       MOVE    TBL-KOUI-SRYKBN12-CHU-KEY (IDY4)
                                                TO  WRK-RECE455-KEY1
                       MOVE    TBL-KOUI-SRYKBN12-CHU-SHORI-NUM    (IDY4)
                                                TO
                                                WRK-RECE455-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE455-SEQNO
                           PERFORM 200799-KANREN-COM-TAIHI-SEC
                   END-IF
      *
                   MOVE    SPACE               TO  WRK-REC
                   MOVE    ZERO                TO  WRK-REC-LENGTH
      *          
                   MOVE    "RI,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF    ( IDY4                 =   1     )
                   AND   ( WRK-SRYKBN      NOT =   SPACE )
                       MOVE    WRK-SRYKBN          TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    SPACE               TO  WRK-SRYKBN
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            診療行為コード
                   MOVE    TBL-KOUI-SRYKBN12-CHU-SRYCD (IDY4)
                                                   TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            数量
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            点数
                   MOVE    TBL-KOUI-SRYKBN12-CHU-TEN (IDY4)
                                               TO  WRK-TENSU
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            金額
      ********     MOVE    TBL-KOUI-SRYKBN12-CHU-TEN (IDY4)
      *??                                      TO  WRK-TENSU
      *??          MOVE    WRK-TENSU           TO  WRK-NUM
      ********     MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            回数
                   MOVE    TBL-KOUI-SRYKBN12-CHU-KAISU (IDY4)
                                               TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC
      *            コメントコード１
                   IF      TBL-RECE455-MAX >   ZERO
                       MOVE    TBL-RECE455-SRYCD (1)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE455-MAX >   ZERO
                       MOVE    TBL-RECE455-NAME  (1)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE455-MAX >   1
                       MOVE    TBL-RECE455-SRYCD (2)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE455-MAX >   1
                       MOVE    TBL-RECE455-NAME  (2)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE455-MAX >   2
                       MOVE    TBL-RECE455-SRYCD (3)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE455-MAX >   2
                       MOVE    TBL-RECE455-NAME  (3)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE44-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE44-KAISU (1 IDV)
                                                       TO  WRK-NUM
                           MOVE    3                   TO  WRK-NAGASA
                       END-IF
                       IF      IDV                 =   31
                           MOVE    1                   TO  WRK-END 
                       END-IF     
                       PERFORM 5005-NUM-HENSYU-SEC
                   END-PERFORM  
      *
                   PERFORM 5001-FILE-WRITE-SEC
      *
      *            関連コメント編集処理
               PERFORM 200798-KANREN-COM-HEN-SEC
               END-IF
           END-PERFORM
           .
       2007251-SRYKBN12-CHUKSN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連コメント退避処理
      *****************************************************************
       200799-KANREN-COM-TAIHI-SEC     SECTION.
      *
           INITIALIZE                  TBL-RECE455-COMMENT-AREA
      *
           MOVE    WRK-RECE455-REC     TO  RECE455-REC
           START   RECE455-FILE
               KEY IS    >=   RECE455-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE455
               NOT INVALID
                   PERFORM 900-RECE455-NEXT-SEC
           END-START
           PERFORM UNTIL  ( FLG-RECE455    =   1                    )
                   OR     ( WRK-RECE455-KEY1 
                                       NOT =   RECE455-KEY1         )
                   OR     ( WRK-RECE455-SHORI-RENNUM
                                       NOT =   RECE455-SHORI-RENNUM )
               PERFORM 2007991-KANREN-COM-TAIHIHEN-SEC
      *
               PERFORM 900-RECE455-NEXT-SEC
           END-PERFORM
      *
           .
       200799-KANREN-COM-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連コメントチェック退避処理
      *****************************************************************
       2007991-KANREN-COM-TAIHIHEN-SEC    SECTION.
      *
      *    診療行為コード＝'０'のとき対象外は出力しない
           IF      RECE455-SRYCD (1:1) =   "0"
               IF    ( RECE455-SRYCD (1:3) =   "002"             )
               OR    ( RECE455-SRYCD (1:3) =   "008"             )
               OR    ( RECE455-SRYCD       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE455-SRYCD       =   WRK-CONS-COMMENT2 ) 
      *****    OR    ( RECE455-SRYCD       =   WRK-CONS-COMMENT3 ) 
               OR    ( RECE455-SRYCD       =   WRK-CONS-COMMENT4 )
                   CONTINUE
               ELSE
                   GO  TO  2007991-KANREN-COM-TAIHIHEN-EXT
               END-IF     
           END-IF
      *     
           ADD     1                TO  TBL-RECE455-MAX
           MOVE    RECE455-SRYCD    TO  TBL-RECE455-SRYCD
                                               (TBL-RECE455-MAX)         
           MOVE    RECE455-NAME     TO  TBL-RECE455-NAME
                                               (TBL-RECE455-MAX)         
      *
           IF      RECE455-SRYCD    =   "810000001"
               CONTINUE
           ELSE
      *        部位／薬剤／ユーザーコメント
               IF      RECE455-SRYCD (1:1) =   "0"
                   MOVE    "810000001"     TO  
                                           TBL-RECE455-SRYCD
                                               (TBL-RECE455-MAX)   
               END-IF
      *        コメント
               IF    ( RECE455-SRYCD (1:1) =   "8"   )
               OR    ( RECE455-SRYCD (1:3)     =   "008"  AND
                       RECE455-SRYCD (4:1) NOT =   "2"   )
                   MOVE    RECE455-INPUTCOMENT TO
                                           TBL-RECE455-NAME
                                               (TBL-RECE455-MAX)
               END-IF
           END-IF
      *    コメントパターン２０のコメントコード
           IF      RECE455-SRYCD (1:2) =   "82"
               MOVE    SPACE              TO
                                           TBL-RECE455-NAME
                                               (TBL-RECE455-MAX)
           END-IF
      *
           .
       2007991-KANREN-COM-TAIHIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連コメント編集処理
      *****************************************************************
       200798-KANREN-COM-HEN-SEC    SECTION.
      *
           GO  TO  200798-KANREN-COM-HEN-EXT
      *
      *    関連コメントが３対を超える場合
           IF      TBL-RECE455-MAX     <=  3
               GO  TO  200798-KANREN-COM-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDVV    FROM    4   BY  1
                   UNTIL   IDVV    >       TBL-RECE455-MAX
      *
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
      *
               MOVE    "CO,"               TO  WRK-REC
               MOVE    3                   TO  WRK-REC-LENGTH
      *        診療識別
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        予備１
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        コメントコード
               MOVE    TBL-RECE455-SRYCD (IDVV)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        文字データ
               MOVE    TBL-RECE455-NAME (IDVV)
                                           TO  WRK-DATA
               MOVE    76                  TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END 
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               IF      TBL-RECE455-NAME (IDVV) (77:)
                                       NOT =   SPACE
                   PERFORM 5001-FILE-WRITE-SEC
      *
                   MOVE    TBL-RECE455-SRYCD (IDVV)
                                               TO  WRK-CO-SRYCD
                   MOVE    TBL-RECE455-NAME (IDVV) (77:)
                                               TO  WRK-CO-COMMENT
                   PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
               END-IF
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-PERFORM
      *
           INITIALIZE                  TBL-RECE455-COMMENT-AREA
      *
           .
       200798-KANREN-COM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（手術歴）編集処理
      *****************************************************************
       2008-COMMENT-HEN-01-SEC      SECTION.
      *
           MOVE    "01"                TO  WRK-COM-SRYKBN
           MOVE    SPACE               TO  WRK-OPECOM-HEN
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE41-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE41-PTID         TO  OPECOM-PTID
           MOVE    RECE41-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE41-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key4"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM       =   RECE41-HKNNUM )
               AND   ( OPECOM-HKNID        =   RECE41-HKNID  )
                   MOVE    OPECOM-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC                 
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 200811-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    コメント編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE41-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE41-PTID         TO  OPECOM-PTID
           MOVE    RECE41-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE41-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key5"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM       =   RECE41-HKNNUM )
               AND   ( OPECOM-HKNID        =   RECE41-HKNID  )
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 200811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 200811-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2008-COMMENT-HEN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（手術歴）コメントの改行コード削除処理
      *****************************************************************
       200811-OPECOM-HENSYU-SEC    SECTION.
      *
           ADD     1                   TO  WRK-COM-RENNUM
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
           MOVE    "2"                 TO  RECEDEN-RECKBN
           MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
           INSPECT WRK-OPECOM-HEN      REPLACING   ALL "  "
                                                   BY  "　"
           MOVE    WRK-OPECOM-HEN      TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
      *    症状詳記（コメント）編集処理
           PERFORM 20099-COMMENT-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
           .
       200811-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（レセコメント）
      *****************************************************************
       2009-COMMENT-HEN-99-SEC      SECTION.
      *
           MOVE    "99"                TO  WRK-COM-SRYKBN
           MOVE   SPACE                TO  WRK-SJ-KBN
                                           WRK-RECECOM-SJKBN
      *     
      *    レセコメント編集処理
      *
      *    適用欄コメント     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 2009-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（０１〜０６）     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key8"              TO  WRK-MCP-PATHNAME
           PERFORM 2009-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（その他）    
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key9"              TO  WRK-MCP-PATHNAME
           PERFORM 2009-RECECOM-HENSYU-SEC
      *
      *    傷病の経過（５０文字を超えるとき）
           IF      FLG-RECECOM-XX      =   "1"
      *        表題を設定
                MOVE    1                  TO  FLG-RECECOM-AA
      *
               MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
               MOVE    "key16"             TO  WRK-MCP-PATHNAME
               PERFORM 2009-RECECOM-HENSYU-SEC
           END-IF
      *     
      *    症状詳記コメント（５０、５１、５２、９０）
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key23"             TO  WRK-MCP-PATHNAME
           PERFORM 2009-RECECOM-HENSYU-SEC
           .
       2009-COMMENT-HEN-99-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント読み込み処理
      *****************************************************************
       2009-RECECOM-HENSYU-SEC    SECTION.
      *
           INITIALIZE                      RECECOM-REC
           MOVE    RECE41-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE41-PTID         TO  RECECOM-PTID
           MOVE    RECE41-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
           IF      WRK-MCP-PATHNAME    =   "key16"
      *        傷病の経過
               MOVE    "AA"                TO  RECECOM-SJKBN
           END-IF
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           PERFORM             UNTIL   FLG-RECECOM   =   1
               IF      RECECOM-HKNCOMBI    =   RECE41-HKNCOMBI
                   IF      RECECOM-SJKBN   =   "99"
                       PERFORM 20091-RECECOM-TEKIYOU-HEN-SEC
                   ELSE      
                       PERFORM 20092-RECECOM-SJ-HENSYU-SEC                 
                   END-IF                 
               END-IF
      *                 
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2009-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄コメント（レセコメント）の改行コード削除処理
      *****************************************************************
       20091-RECECOM-TEKIYOU-HEN-SEC    SECTION.
      *                                          
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN
      *
           PERFORM     UNTIL       WRK-RECEHEN  =   1      
      *
               MOVE    SPACE           TO  WRK-COMMENT-G
                                           WRK-COMM1
               MOVE    ZERO            TO  WRK-COL
      *
               IF      WRK-COMM (1:1)  =   WRK-CONS-RETURN
                   MOVE    WRK-COMM (2:)   TO  WRK-COMM1
               ELSE             
                   INSPECT WRK-COMM    TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >
                                           WRK-CONS-RECECOM-LENGTH-MAX
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1    =   IDX     -   1
                               COMPUTE IDY2    =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                               TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                               TO  WRK-COMM1
                               MOVE    WRK-CONS-RECECOM-LENGTH-MAX
                                               TO  IDX
                           END-IF
                       END-PERFORM
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   IF      WRK-COMMENT-G       NOT =   SPACE
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       WRK-CONS-RECECOM-MAX
                             OR      WRK-COMMENT(IDW)  =   SPACE
      *
                       ADD     1                   TO  WRK-COM-RENNUM
      *          
      *                キー編集処理
                       PERFORM 50011-KEY-HENSYU-SEC
                       MOVE    "2"                 TO  RECEDEN-RECKBN
                       MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
                       MOVE    WRK-COMMENT (IDW)   TO  RECEDEN-RECEDATA
                       MOVE    "99"                TO  RECEDEN-SJKBN
      *
                       PERFORM 50012-DBINSERT-SEC
      *
      *                適用欄コメント編集処理
                       PERFORM 20099-COMMENT-HENSYU-SEC
                     END-PERFORM
                   END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       20091-RECECOM-TEKIYOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記コメント（レセコメント）の改行コード削除処理
      *****************************************************************
       20092-RECECOM-SJ-HENSYU-SEC    SECTION.
      *                                          
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN
           IF      RECECOM-SJKBN   =   "07"    OR  "08"    OR  "09"
               MOVE    "07"            TO  RECECOM-SJKBN
           END-IF 
           IF      RECECOM-SJKBN   =   "AA"
               MOVE    "07"            TO  RECECOM-SJKBN
           END-IF 
           IF      RECECOM-SJKBN   NOT =   WRK-RECECOM-SJKBN
               MOVE   RECECOM-SJKBN        TO  WRK-RECECOM-SJKBN
                                               WRK-SJ-KBN
           END-IF
      *
           PERFORM     UNTIL       WRK-RECEHEN  =   1      
      *
               MOVE    SPACE           TO  WRK-COMMENT-G
                                           WRK-COMM1
               MOVE    ZERO            TO  WRK-COL
      *
               IF      WRK-COMM (1:1)  =   WRK-CONS-RETURN
                   MOVE    WRK-COMM (2:)   TO  WRK-COMM1
               ELSE             
                   INSPECT WRK-COMM    TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >
                                           WRK-CONS-RECECOM-LENGTH-MAX
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1     =   IDX     -   1
                               COMPUTE IDY2     =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                                TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                                TO  WRK-COMM1
                               MOVE    WRK-CONS-RECECOM-LENGTH-MAX
                                                TO  IDX
                           END-IF
                       END-PERFORM                                                                                  
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   IF      WRK-COMMENT-G       NOT =   SPACE
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       24
                             OR      WRK-COMMENT1(IDW)   =   SPACE
      *
                       ADD     1                   TO  WRK-COM-RENNUM
      *          
      *                キー編集処理
                       PERFORM 50011-KEY-HENSYU-SEC
                       MOVE    "2"                 TO  RECEDEN-RECKBN
                       MOVE    WRK-COM-RENNUM      TO  RECEDEN-RENNUM
                       MOVE    WRK-COMMENT1 (IDW)  TO  RECEDEN-RECEDATA
                       MOVE    RECECOM-SJKBN       TO  WRK-SJKBN-X
                       MOVE    WRK-SJKBN           TO  RECEDEN-SJKBN
      *
                       PERFORM 50012-DBINSERT-SEC
      *
                     END-PERFORM
      *
      *              症状詳記レコード編集処理
                     PERFORM 200921-SJ-HENSYU-SEC
                   END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       20092-RECECOM-SJ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記レコード編集処理
      *****************************************************************
       200921-SJ-HENSYU-SEC       SECTION.
      *
      *    全角スペースのみのレコードは記載しない
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "1"                 TO  KANACHK-SYORIKBN 
           MOVE    WRK-COMMENT-G       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF       KANACHK-RC2        =   2
               GO  TO  200921-SJ-HENSYU-EXT
           END-IF
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-MAX         =   ZERO )
               GO  TO  200921-SJ-HENSYU-EXT
           END-IF
      *    後ろの全角空白を削除する
           IF      KANACHK-RC          =   ZERO
               MOVE    SPACE               TO  WRK-COMMENT-G
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-COMMENT-G
           END-IF
      *
      *    傷病の経過なら先頭に表題を記録する
           IF      FLG-RECECOM-AA      =   1
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
      *
               MOVE    "SJ,"               TO  WRK-REC
               MOVE    3                   TO  WRK-REC-LENGTH
      *        症状詳記区分
               IF      WRK-SJ-KBN      NOT =   SPACE     
                   MOVE    WRK-SJ-KBN          TO  WRK-DATA
                   MOVE    2                   TO  WRK-NAGASA
               END-IF
               MOVE    SPACE               TO  WRK-SJ-KBN
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        症状詳記データ
               MOVE    "【傷病の経過】"    TO  WRK-DATA
               MOVE    2400                TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END 
               PERFORM 5004-MOJI-HENSYU-SEC  
      *
                PERFORM 5001-FILE-WRITE-SEC
      *
                MOVE    0                  TO  FLG-RECECOM-AA
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "SJ,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    症状詳記区分
           IF      WRK-SJ-KBN      NOT =   SPACE     
               MOVE    WRK-SJ-KBN          TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
           END-IF
           MOVE    SPACE               TO  WRK-SJ-KBN
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    症状詳記データ
           MOVE    WRK-COMMENT-G       TO  WRK-DATA
           MOVE    2400                TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
            PERFORM 5001-FILE-WRITE-SEC
           .
       200921-SJ-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    症状詳記（コメント）編集処理
      *****************************************************************
       20099-COMMENT-HENSYU-SEC       SECTION.
      *
           IF      SYS-2005-RECECOMKBN =   "1"
               GO  TO  20099-COMMENT-HENSYU-EXT
           END-IF
      *
      *    全角スペースのみのレコードは記載しない
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "1"                 TO  KANACHK-SYORIKBN 
           MOVE    RECEDEN-RECEDATA    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF      KANACHK-RC2         =   2
               GO  TO  20099-COMMENT-HENSYU-EXT
           END-IF
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-MAX         =   ZERO )
               GO  TO  20099-COMMENT-HENSYU-EXT
           END-IF
      *    後ろの全角空白を削除する
           IF      KANACHK-RC          =   ZERO
               MOVE    SPACE               TO  RECEDEN-RECEDATA
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  RECEDEN-RECEDATA
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           IF      WRK-COM-SRYKBN      NOT =   SPACE     
               MOVE    WRK-COM-SRYKBN      TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               MOVE    SPACE               TO  WRK-COM-SRYKBN
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    予備１
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    "810000001"         TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    RECEDEN-RECEDATA    TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC  
      *
            PERFORM 5001-FILE-WRITE-SEC
           .
       20099-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE41-FILE
                   RECE43-FILE
                   RECE44-FILE
                   RECE45-FILE
                   RECE455-FILE
      *
      *    ＰＵＳＨ通知処理
           PERFORM 3001-PUSH-SEC
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE41  CNT-IN  :" CNT-IN  
           DISPLAY "RECE04  CNT-IN  :" CNT-RECE04  
           DISPLAY "DB      CNT-OUT :" CNT-OUT  
           DISPLAY "*** ORCR0555 END ***"
      *
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＳＨ通知処理
      *****************************************************************
       3001-PUSH-SEC        SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO  )
           AND   ( JOB-KARTE-UID
                               NOT =   SPACE )
               INITIALIZE                  PUSHBA01-REC
               MOVE    JOB-KARTE-UID   TO  PUSHBA01API-KARTE-UID
               MOVE    JOB-ORCA-UID    TO  PUSHBA01API-ORCA-UID
               MOVE    "000"           TO  PUSHBA01API-RESULT
               MOVE    "receipt_make"  TO  PUSHBA01-EVENT
               IF      WRK-PARA-SRYYM  =   ZERO
                   MOVE    "9999-12"       TO  PUSHBA01-PERFORM-MONTH
               ELSE
                   MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                           TO  PUSHBA01-PERFORM-MONTH
               END-IF
               MOVE    JOB-UPDCNT1     TO  WRK-CNT
               MOVE    WRK-CNT-X       TO  PUSHBA01-OUTPUT-COUNT
      *??
             display "push=" PUSHBA01-REC "#"
      *??
               MOVE    PUSHBA01-REC    TO  MCPDATA-REC
               MOVE    "PUSHEVENT"     TO  MCP-FUNC
               MOVE    "push_batch01"  TO  MCP-TABLE
               MOVE    "push_batch01"  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
           .
       3001-PUSH-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYMD (1:4)      TO  WRK-HEN-YY
               MOVE    WRK-SYMD (5:2)      TO  WRK-HEN-MM
               MOVE    WRK-SYMD (7:2)      TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *            
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCR0555 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR0555 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
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
           ADD     1                   TO  WRK-RENNUM
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
      *
      *    仮レセ（院外処方含む）かどうかの区分設定
           IF      FLG-TOYAKUFLG       =   ZERO
               MOVE    "1"                 TO  RECEDEN-RECKBN
           ELSE
               MOVE    "9"                 TO  RECEDEN-RECKBN
           END-IF
           MOVE    WRK-RENNUM          TO  RECEDEN-RENNUM
           IF      FLG-TOTALKIN        =   1
               MOVE    SUM-TOTALKIN        TO  RECEDEN-TOTALTEN
           END-IF
           MOVE    WRK-REC             TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
           MOVE    ZERO                TO  FLG-TOTALKIN
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー編集処理
      *****************************************************************
       50011-KEY-HENSYU-SEC        SECTION.
      *          
           INITIALIZE                      RECEDEN-REC
           MOVE    RECE41-HOSPNUM      TO  RECEDEN-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECEDEN-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECEDEN-PTID
           MOVE    ZERO                TO  RECEDEN-RECEKA
           MOVE    3                   TO  RECEDEN-TEISYUTUSAKI
           MOVE    RECE41-RECESYUBETU  TO  RECEDEN-RECESYUBETU
           MOVE    ZERO                TO  RECEDEN-HKNJANUM
           MOVE    ZERO                TO  RECEDEN-HOJOKBN-KEY
           MOVE    RECE41-HKNID        TO  RECEDEN-KOHID-KEY
           MOVE    RECE41-TEKSTYMD     TO  RECEDEN-TEKSTYMD
           MOVE    RECE41-TEKSTYMD     TO  RECEDEN-TEKSTYMD
           MOVE    SPACE               TO  RECEDEN-RECEIPT-KBN
           .
       50011-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       50012-DBINSERT-SEC          SECTION.
      *
      *    症状詳記用のデータは記録しない
           IF      RECEDEN-RECKBN      =   "2"
               GO  TO  50012-DBINSERT-EXT
           END-IF
      *
      *
           MOVE    WRK-PARA-CREYMD     TO  RECEDEN-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECEDEN-CREHMS
      *
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           IF      WRK-PARA-PRTKBN     =   "B"
               MOVE    "tbl_receden_tmp"   TO  MCP-TABLE
           ELSE
               MOVE    "tbl_receden"       TO  MCP-TABLE
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBINSERT-SEC
      *
           ADD     1                   TO  CNT-OUT
           .
       50012-DBINSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    ZERO                TO  WRK-SYMD
           END-IF    
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD            TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-YMD        TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    右詰め編集処理
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
               IF      WRK-MOJI-CHK    =   1
      *            後ろの全角空白を削除する
                   INITIALIZE                      ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    "1"                 TO  KANACHK-SYORIKBN
                   MOVE    WRK-NAGASA          TO  KANACHK-MAX-CNT
                   MOVE    WRK-DATA            TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC          =   ZERO )
                   AND   ( KANACHK-MAX         >   ZERO )
                       MOVE    SPACE               TO  WRK-DATA
                       MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                                   TO  WRK-DATA
                   END-IF
               END-IF
      *
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW2)          DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW2                TO  WRK-REC-LENGTH        
                       MOVE    1                   TO  IDW2
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
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.99999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        小数ありのとき、小数部がゼロの場合
               IF      WRK-SYOSUU          >   ZERO
                   IF      WRK-NUM-R-2         =   "00000"
                      MOVE    ZERO                TO  WRK-SYOSUU
                   ELSE
                       PERFORM VARYING IDW2    FROM    WRK-SYOSUU  BY -1
                               UNTIL   IDW2    <       1
                           IF      WRK-NUM-R-2 (IDW2:1)    NOT =   "0"
                               MOVE    IDW2            TO  WRK-SYOSUU
                               MOVE    1               TO  IDW2
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
      *
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW2    FROM    WRK-ST    BY  1
                       UNTIL   IDW2    >       10
                   IF      WRK-NUM (IDW2:1)  NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW2       
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW2:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ                  
                       MOVE    10                TO  IDW2
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
      *    コメントレコード記録処理（７６バイトを超えるとき）
      *****************************************************************
       5010-CO-RECORD-NEXT-HENSYU-SEC  SECTION.
      *
      *    全角スペースのみのレコードは記載しない
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "1"                 TO  KANACHK-SYORIKBN 
           MOVE    WRK-CO-COMMENT      TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF       KANACHK-RC2        =   2
               GO  TO  5010-CO-RECORD-NEXT-HENSYU-EXT
           END-IF
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-MAX         =   ZERO )
               GO  TO  5010-CO-RECORD-NEXT-HENSYU-EXT
           END-IF
      *    後ろの全角空白を削除する
           IF      KANACHK-RC          =   ZERO
               MOVE    SPACE               TO  WRK-CO-COMMENT
               MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                           TO  WRK-CO-COMMENT
           END-IF
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           MOVE    "CO,"               TO  WRK-REC
           MOVE    3                   TO  WRK-REC-LENGTH
      *    診療識別
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    負担区分
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード
           MOVE    WRK-CO-SRYCD        TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ
           MOVE    WRK-CO-COMMENT      TO  WRK-DATA
           MOVE    76                  TO  WRK-NAGASA
      *****MOVE    1                   TO  WRK-MOJI-CHK
           MOVE    1                   TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           INITIALIZE                      WRK-CO-SRYCD
                                           WRK-CO-COMMENT
           .
       5010-CO-RECORD-NEXT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC            SECTION.
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
      *    システム管理マスタ読込
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
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-READ-SEC               SECTION.
      *
           MOVE    RECE41-SRYYM        TO  TNS-YUKOSTYMD (1:6)
           MOVE    "01"                TO  TNS-YUKOSTYMD (7:2)
           MOVE    TNS-YUKOSTYMD       TO  TNS-YUKOEDYMD
           MOVE    WRK-PARA-HOSPNUM    TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントマスタ　ＲＥＡＤ
      *****************************************************************
       900-RECECOM-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECECOM
               MOVE    MCPDATA-REC     TO  RECECOM-REC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
           .
       900-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-SEC           SECTION.
      *
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-OPECOM
               INITIALIZE                      OPECOM-REC
           END-IF
      *
           .
       900-OPECOM-READ-EXT.
           EXIT.
      *
     *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  OPECOM-REC
               MOVE    ZERO                TO  FLG-OPECOM
           ELSE
               INITIALIZE                      OPECOM-REC
               MOVE    1                   TO  FLG-OPECOM
           END-IF
      *
           .
       900-OPECOM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE41-READ-SEC             SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                NOT AT  END
                   IF      STS-RECE41          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE41PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE41      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
      *            公務災害、アフターケア、種別不明のとき対象外
      *            指定医療機関ではないとき対象外
      *            ０９５９３で始まるコードがあるとき対象外
                   IF  (   RECE41-SRYYM        <   201307 )
                   OR  (   RECE41-PTNUM        =   SPACE  )
                   OR  (   RECE41-TSTPTNUMKBN  =   "1"    )
                   OR  (   RECE41-RECESYUBETU  =   ZERO
                                               OR  7021
                                               OR  7051   )
                   OR  (   RECE41-ERR-ERRKBN   >   ZERO   )
                   OR  (   RECE41-RSIIRYOKBN   =   "0"    )
                   OR  ((  RECE41-HKNKBN    = "1" OR "2" ) AND
                        (  RECE41-TOTALKIN  =  ZERO      ) AND
                        (  RECE41-TOTALKIN2 =  ZERO      ))
                   OR  (   RECE41-ORIGINALCDKBN
                                               =   1      )
                       GO  TO  900-RECE41-READ-SEC
                   ELSE
      *                医療機関情報取得
                       IF      WRK-PARA-SRYYM  =   ZERO
                           MOVE  RECE41-SRYYM      TO
                                                   WRK-SYS-SRYYMD (1:6)
                           MOVE  "01"              TO
                                                   WRK-SYS-SRYYMD (7:2)
                           PERFORM 900-SYSKANRI-HENSYU-SEC
                       END-IF
       *
                       ADD     1               TO  CNT-IN
                       DISPLAY "RECE41  PTNUM=" RECE41-PTNUM 
                               " SRYYM="        RECE41-SRYYM 
                               " RECESYUBETU="  RECE41-RECESYUBETU
                   END-IF
           END-READ
           .
       910-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
      *    医療機関情報（基本情報）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1001(SYSKAN-IDX) TO  SYS-1001-REC
               IF    ( SYS-1001-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING  "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE43-NEXT-SEC           SECTION.
      *
           READ    RECE43-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE43
               NOT AT  END
                   IF      STS-RECE43          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE43PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE43          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE43
           END-READ
      *
           .
       990-RECE43-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE44-NEXT-SEC           SECTION.
      *
           READ    RECE44-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE44
               NOT AT  END
                   IF      STS-RECE44          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE44PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE44          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE44
                   ADD     1         TO  CNT-RECE04
           END-READ
      *
      *
           .
       990-RECE44-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE45-NEXT-SEC           SECTION.
      *
           READ    RECE45-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE45
               NOT AT  END
                   IF      STS-RECE45          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE45PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE45          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE45
           END-READ
      *
           .
       990-RECE45-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    関連コメントデータ読み込み
      ****************************************************************
       900-RECE455-NEXT-SEC           SECTION.
      *
           READ    RECE455-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE455
               NOT AT  END
                   IF      STS-RECE455         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE455PARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE455         TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE455
           END-READ
      *
           .
       900-RECE455-NEXT-EXT.
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
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       900-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ更新処理エラー　TABLE="
                                           DELIMITED  BY  SIZE
                       MCP-TABLE           DELIMITED  BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0555 insert err TABLE="
                                           DELIMITED   BY  SIZE
                      MCP-TABLE            DELIMITED   BY  SPACE
                      " PATHNAME="         DELIMITED   BY  SIZE
                      MCP-PATHNAME         DELIMITED   BY  SPACE
                      " RC="               DELIMITED   BY  SIZE
                      WRK-MCP-RC           DELIMITED   BY  SIZE
                      LOW-VALUE            DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-DBINSERT-EXT.
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
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0555 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
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
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
