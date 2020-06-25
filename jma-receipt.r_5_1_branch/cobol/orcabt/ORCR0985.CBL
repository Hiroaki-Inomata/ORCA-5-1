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
       PROGRAM-ID.             ORCR0985.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災）
      *  コンポーネント名  : レセプト電算ファイル作成（入院）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/02/07    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-藤原    13/08/20  ０９５９３で始まるコードがあ
      *                                     るレセプトは対象外とする
      *  04.07.02    NACL-藤原    14/03/30  平成２６年４月改正対応
      *                                     使用量、単価の小数部の記録方
      *                                     法の変更
      *                                     漢字モードの継続するスペース
      *                                     の記録方法の変更
      *  04.07.03    NACL-藤原    15/01/24  同日再入院時の入退院履歴の記録
      *                                     を修正
      *                                    （入院日と退院日が同じとき）
      *  04.07.04    NACL-藤原    15/02/12  告示等識別区分による記録順を修正
      *                                    （通則加算対象外）
      *  04.07.05    NACL-藤原    15/04/16  注加算コードが１４１９の加算
      *                                     項目の記録方法の修正
      *  04.07.06    NACL-藤原    15/06/10  保険請求対象外傷病名コード対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    14/07/23  ３２文字以上の文字列修正
      *                                     （コンパイル時のワーニング修正）
      *  04.08.03    NACL-藤原    16/01/15  請求情報への更新日設定
      *  04.08.04    NACL-藤原    16/04/01  平成２８年４月改正対応
      *  04.08.05    NACL-門脇    16/08/23  持参薬対応
      *  04.08.06    NACL-門脇    16/09/06  入院室料加算差額請求マスタと
      *                                     特別労災付添看護料差額請求
      *                                     マスタの対応
      *  04.08.07    NACL-門脇    16/09/20  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *  04.08.08    NACL-藤原    17/03/02  未コード化特定器材コード廃止対応
      *  04.08.09    NACL-門脇    17/08/28  平成２９年１０月改正
      *                                     （入院時生活療養の見直し）
      *  04.08.10    NACL-藤原    18/05/17  平成３０年４月改正対応
      *                                     カタカナ（氏名）、患者状態の追加
      *  04.08.11    NACL-門脇    18/09/27  選択式コメント対応
      *
      *  05.00.01    NACL-藤原    17/07/04  ＨＡＯＲＩ対応
      *  05.00.02    NACL-伊藤    17/12/22  診療情報システム対応
      *  05.00.03    NACL-藤原    19/03/25  元号改正対応
      *                                     Warning 対応
      *  05.00.04    NACL-藤原    19/11/21  診療行為データ用テーブル
      *                                    （加算項目扱いの基本項目）を増やす
      *  05.00.05    NACL-藤原    20/03/25  記録条件仕様変更に伴う対応
      *                                     年月（年月日）の西暦記録 
      *                                     Warning 対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *
           SELECT  RECE83-FILE     ASSIGN  RECE83PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE83-KEY
                                   FILE    STATUS  IS  STS-RECE83.
           SELECT  RECE84-FILE    ASSIGN  RECE84PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE84-KEY
                                   FILE    STATUS  IS  STS-RECE84.
           SELECT  RECE85-FILE     ASSIGN  RECE85PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE85-KEY
                                   FILE    STATUS  IS  STS-RECE85.
           SELECT  RECE855-FILE    ASSIGN  RECE855PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE855-KEY
                                   FILE    STATUS  IS  STS-RECE855.
      *    入退院履歴情報
           SELECT  RECE88-FILE     ASSIGN  RECE88PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE88-KEY
                                   FILE    STATUS  IS  STS-RECE88.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC. 
           COPY    "CPRCF082.INC".
      *
       FD  RECE83-FILE.
       01  RECE83-REC.
           COPY    "CPRCF083.INC".
           03  RECE83-SYUBYOFLG-X  PIC X(01).
      *
       FD  RECE84-FILE.
       01  RECE84-REC.
           COPY    "CPRCF0841.INC" REPLACING  //RECE841//
                                   BY         //RECE84//.
      *
       FD  RECE85-FILE.
       01  RECE85-REC.
           COPY    "CPRCF085.INC".
      *
      *    関連コメントデータ
       FD  RECE855-FILE.
       01  RECE855-REC.
           COPY    "CPRCF0855.INC".
      *
       FD  RECE88-FILE.
       01  RECE88-REC.
           COPY    "CPRCF088.INC".
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
                                   BY         //RECE82//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE83//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE84//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE85//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE855//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE88//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82              PIC X(02).
           03  STS-RECE83              PIC X(02).
           03  STS-RECE84              PIC X(02).
           03  STS-RECE85              PIC X(02).
           03  STS-RECE855             PIC X(02).
           03  STS-RECE88              PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECECOM             PIC 9(01).
           03  FLG-OPECOM              PIC 9(01).
      *
           03  FLG-RECE82              PIC 9(01).
           03  FLG-RECE83              PIC 9(01).
           03  FLG-RECE84              PIC 9(01).
           03  FLG-RECE85              PIC 9(01).
           03  FLG-RECE855             PIC 9(01).
           03  FLG-RECE88              PIC 9(01).
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
           03  FLG-NYUTAI-RIREKI       PIC 9(01).
           03  FLG-NYUIN-SRYCD         PIC 9(01).
           03  FLG-TAISYO              PIC 9(01).
           03  FLG-RECECOM-XX          PIC 9(01).
           03  FLG-RECECOM-AA          PIC 9(01).
           03  FLG-NYUINYMD            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-RECE84              PIC 9(06).
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
           03  IDXZ2                   PIC 9(03).
           03  IDW1                    PIC 9(03).
           03  IDW2                    PIC 9(04).
           03  IDW3                    PIC 9(04).
           03  IDW4                    PIC 9(04).
           03  IDW5                    PIC 9(04).
           03  IDX0                    PIC 9(03).
           03  IDZ1                    PIC 9(03).
           03  IDZ2                    PIC 9(03).
           03  IDZ3                    PIC 9(03).
           03  IDZ4                    PIC 9(03).
           03  IDZ5                    PIC 9(03).
           03  IDZ6                    PIC 9(03).
           03  IDZ7                    PIC 9(03).
           03  IDYY                    PIC 9(03).
           03  IDZZ                    PIC 9(03).
           03  IDWW                    PIC 9(03).
           03  IDVV                    PIC 9(03).
           03  IDV1                    PIC 9(03).
      *
           03  SYSKAN-IDX              PIC 9(03).
      *    
       01  KEY-AREA                          VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-SRYYM         PIC 9(06).
               05  KEY-N-PTID          PIC 9(10).
           03  KEY-OLD.
               05  KEY-O-SRYYM         PIC 9(06).
               05  KEY-O-PTID          PIC 9(10).
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
           03  WRK-RECE84-SRYKBN       PIC X(02).
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
           03  WRK-RECE84-ZAIKAISU     PIC 9(04).    
           03  WRK-KENSASU             PIC 9(04).    
      *    時間外加算区分（基本項目）
           03  WRK-KIHON-TIMEKSNKBN    PIC 9(01).
      *    処置の加算項目の有無フラグ
           03  WRK-SRYKBN40-KOKUJISKBKBN1
                                       PIC 9(01).
      *    日付変換用
           03  WRK-SYMD.
               05  WRK-SYMD-YM         PIC X(06).
               05  WRK-SYMD-DD         PIC 9(02).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-ACCTP-SYUTEN        PIC 9(07).
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
           03  WRK-CO-COMMENT          PIC X(76).
           03  WRK-CO-SRYCD            PIC X(09).
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
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN         PIC X(01).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
      *    昭和用コメント
           03  WRK-NYUTAI-COMMENT.
               05 WRK-NYUTAI-COM       PIC X(08).
               05 WRK-NYUTAI-YMD       PIC X(22).
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
       01  TBL-RECE855-COMMENT-AREA.
           03  TBL-RECE855-OCC         OCCURS  100.
               05  TBL-RECE855-SRYCD   PIC X(09).
               05  TBL-RECE855-NAME    PIC X(200).
           03  TBL-RECE855-MAX         PIC 9(03).
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
       01  WRK-RECE855-REC.
           COPY    "CPRCF0855.INC" REPLACING  //RECE855//
                                   BY         //WRK-RECE855//.
      *
       01  WRK-RECE85-REC.
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //WRK-RECE85//.
      *
       01  WRK-TAIHI-RECE84-REC.
           COPY    "CPRCF0841.INC"  REPLACING //RECE841//
                                   BY         //WRK-TAIHI4//.
      *
      *    画像診断用退避エリア
      *    撮影料退避
       01  FLG-SRYSYUKBN721        PIC 9(01).
       01  WRK-721-RECE85-REC.
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //WRK-721-RECE85//.
       01  WRK-700-RECE85-AREA.
           02  WRK-SRYSYUKBN700-MAX           PIC 9(01).
           02  FLG-SRYSYUKBN700               PIC 9(01)  OCCURS  5.
           02  WRK-700-RECE85-REC                        OCCURS  5.
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //WRK-700-RECE85//.
      *    撮影料退避（同時撮影）
       01  FLG-SRYSYUKBN721-DOUJI             PIC 9(01).
       01  WRK-721-DOUJI-RECE85-REC.
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //WRK-721-DOU85//.
       01  WRK-700-DOUJI-RECE85-AREA.
           02  WRK-SRYSYUKBN700-DOUJI-MAX     PIC 9(01).
           02  FLG-SRYSYUKBN700-DOUJI         PIC 9(01)  OCCURS  5.
           02  WRK-700-DOUJI-RECE85-REC                  OCCURS  5.
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //WRK-700-DOU85//.
      *    間接撮影の５０％逓減
       01  WRK-SRYKBN70-RECE85-REC.
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //WRK-SRY70-RE85//.
      *    生体検査・加算用退避エリア
       01  WRK-KASAN-RECE85-REC.
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //WRK-KASAN-RE85//.
      *    生体検査・逓減用退避エリア
       01  WRK-TEIGEN-RECE85-REC.
           COPY    "CPRCF085.INC"  REPLACING  //RECE85//
                                   BY         //WRK-TEIGE-RE85//.
      *
      *    検査（判断料）用退避エリア
       01  TBL-KENSA-RECE84-AREA.
         02  TBL-KENSA-RECE84-REC  OCCURS  100.
      *
           COPY    "CPRCF0841.INC"  REPLACING  //RECE841//
                                    BY          //TBL-KENSA4//.
      *
           03  TBL-KENSA-RECE85-REC        OCCURS  50.
               05  TBL-KENSA-RECE85-KEY.
                   07  TBL-KENSA-RECE85-HOSPNUM        PIC 9(2).
                   07  TBL-KENSA-RECE85-SRYYM          PIC 9(06).
                   07  TBL-KENSA-RECE85-NYUGAIKBN      PIC X(01).
                   07  TBL-KENSA-RECE85-PTID           PIC 9(10).
                   07  TBL-KENSA-RECE85-RECEKA         PIC X(02).
                   07  TBL-KENSA-RECE85-HKNID          PIC 9(10).
                   07  TBL-KENSA-RECE85-TEKSTYMD       PIC X(08). 
                   07  TBL-KENSA-RECE85-ZAINUM         PIC 9(08).
                   07  TBL-KENSA-RECE85-RENNUM         PIC 9(02).
              05  TBL-KENSA-RECE85-SHORI-RENNUM        PIC 9(01).
      *
              05  TBL-KENSA-RECE85-SRYACTINF.
      *            診療コード
                   07  TBL-KENSA-RECE85-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KENSA-RECE85-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KENSA-RECE85-SRYSURYO      PIC  9(05)V9(05).
      *            回数
                   07  TBL-KENSA-RECE85-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KENSA-RECE85-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KENSA-RECE85-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KENSA-RECE85-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KENSA-RECE85-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KENSA-RECE85-TEN            PIC 9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KENSA-RECE85-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KENSA-RECE85-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KENSA-RECE85-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KENSA-RECE85-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KENSA-RECE85-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KENSA-RECE85-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KENSA-RECE85-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KENSA-RECE85-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KENSA-RECE85-CDKBNINF.
                       09  TBL-KENSA-RECE85-CDKBN-KBN   PIC  X(01).
                       09  TBL-KENSA-RECE85-CDKBN-SYO   PIC  9(01).
                       09  TBL-KENSA-RECE85-CDKBN-BU    PIC  9(02).
                       09  TBL-KENSA-RECE85-CDKBN-KBNNUM     PIC 9(03).
                       09  TBL-KENSA-RECE85-CDKBN-EDA        PIC 9(02).
                       09  TBL-KENSA-RECE85-CDKBN-KOUBAN     PIC 9(02).
                       09  TBL-KENSA-RECE85-KOUHYOJYUNNUM    PIC 9(09).
      *            告示等識別区分１
                   07  TBL-KENSA-RECE85-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KENSA-RECE85-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KENSA-RECE85-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KENSA-RECE85-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KENSA-RECE85-KZMCOMPSKB      PIC  9(01).
      *            注加算コード
                   07  TBL-KENSA-RECE85-CHUKSNINF.
      *                コード
                       09  TBL-KENSA-RECE85-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KENSA-RECE85-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KENSA-RECE85-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KENSA-RECE85-INPUTCOMENT     PIC  X(140). 
      *        名称情報
               05  TBL-KENSA-RECE85-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KENSA-RECE85-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KENSA-RECE85-NAME            PIC  X(200).
      *
           03  TBL-KENSA-SRYCD-MAX
                                  PIC 9(02).
      *
         02  TBL-KENSA-RECE84-MAX PIC 9(03).
         02  TBL-KENSA-RECE85-MAX PIC 9(03).
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
      *
      *    入院年月日（平成）
           03  WRK-CONS-NYUIN-SRYCD   PIC X(09)   VALUE   "840000088".
      *    入院年月日（令和）
           03  WRK-CONS-NYUIN-REIWA-SRYCD
                                      PIC X(09)   VALUE   "840000588".
      *    再入院年月日（平成）
           03  WRK-CONS-SAINYUIN-SRYCD
                                      PIC X(09)   VALUE   "840000014".
      *    再入院年月日（令和）
           03  WRK-CONS-SAINYUIN-REIWA-SRYCD
                                      PIC X(09)   VALUE   "840000514".
      *    退院年月日（平成）
           03  WRK-CONS-TAIIN-SRYCD   PIC X(09)   VALUE   "840000013".
      *    退院年月日（令和）
           03  WRK-CONS-TAIIN-REIWA-SRYCD
                                      PIC X(09)   VALUE   "840000513".
      *    コメント（昭和）
           03  WRK-CONS-COMMENT-SRYCD PIC X(09)   VALUE   "810000001".
      *    平成開始日
           03  WRK-CONS-YMD-HEISEI     PIC X(08)   VALUE   "19890108".
      *    令和開始日
           03  WRK-CONS-YMD-REIWA      PIC X(08)   VALUE   "20190501".
      *
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
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    レセプトコメント
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理
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
      *    入院会計作成用パラメタ
           COPY    "CPORCSRNYURECEDEN.INC".
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
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
                                       KEY-AREA
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
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "レセ電データ作成（労災）"
                                       TO  JOB-SHELLMSG
           MOVE    "ORCR0985"          TO  JOB-PGID
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 1001-SYSKANRI-HENSYU-SEC
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
      *
           MOVE    "RECE839"           TO  RECE83PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE83PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE83PARA-HOSPNUM
      *
           MOVE    "RECE84F"           TO  RECE84PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE84PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE84PARA-HOSPNUM
      *
           MOVE    "RECE85F"           TO  RECE85PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE85PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE85PARA-HOSPNUM
      *
           MOVE    "RECE856"           TO  RECE855PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE855PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE855PARA-HOSPNUM
      *
           MOVE    "RECE880"           TO  RECE88PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE88PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE88PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE82PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE83PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE84PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE85PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    RECE855PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           MOVE    RECE88PARA-BASENAME TO  SGETTEMP-BASENAMES  (7)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE82PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE83PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE84PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE85PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE855PARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE88PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    I-O                 RECE82-FILE  
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE83-FILE  
           IF      STS-RECE83          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE83PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE83          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE84-FILE  
           IF      STS-RECE84          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE84PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE84          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    I-O                 RECE85-FILE  
           IF      STS-RECE85          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE85PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE85          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE855-FILE
           IF      STS-RECE855         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE855PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE855         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE88-FILE
           IF      STS-RECE88          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE88PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE88          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      * 
           PERFORM 900-RECE82-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム管理マスタ取得      
      *****************************************************************
       1001-SYSKANRI-HENSYU-SEC    SECTION.
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
      *    入院会計取得処理
           PERFORM 2001-NYURECEDEN-GET-SEC
      *
           MOVE    ZERO            TO  WRK-RENNUM
           MOVE    ZERO            TO  FLG-TOYAKUFLG
      *
           MOVE    ZERO            TO  WRK-COM-RENNUM
                                       FLG-COMMENT
           MOVE    SPACE           TO  WRK-COM-SRYKBN
      *
           INITIALIZE                  WRK-RECE84-SRYKBN
                                       TBL-KOUI-SRYKBN12-AREA
                                       TBL-KOUI-SRYKBN12-CHU-AREA
                                       TBL-KENSA-RECE84-AREA
      *
      *    合計金額の退避
           MOVE    ZERO            TO  FLG-TOTALKIN
           COMPUTE SUM-TOTALKIN    =   RECE82-TOTALKIN
                                   +   RECE82-KINGAKU2-SYO
                                   +   RECE82-SHOKUJITOTALKIN
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
           ADD     1               TO  CNT-COMMIT
           IF      CNT-COMMIT      =   300
               MOVE    ZERO            TO  CNT-COMMIT
               PERFORM 900-DBCOMMIT-SEC
               PERFORM 900-DBSTART-SEC
           END-IF
      *
           PERFORM 900-RECE82-READ-SEC
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
           MOVE    RECE82-SRYYM        TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
      *     
      *    療養期間終了
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE82-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"               USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
           . 
       2001-SRYYM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計取得処理
      *****************************************************************
       2001-NYURECEDEN-GET-SEC         SECTION.
      *
           INITIALIZE                      ORCSRNYURECEDEN-AREA
           MOVE    RECE82-HOSPNUM      TO  LNK-RNYURECEDEN-HOSPNUM
           MOVE    RECE82-PTID         TO  LNK-RNYURECEDEN-PTID
           MOVE    RECE82-SRYYM        TO  LNK-RNYURECEDEN-SANTEIYM
           MOVE    RECE82-HKNCOMBI     TO  LNK-RNYURECEDEN-HKNCOMBI
           MOVE    "4"                 TO  LNK-RNYURECEDEN-HKNKBN
           CALL    "ORCSRNYURECEDEN"   USING   ORCSRNYURECEDEN-AREA
                                               SPA-AREA
           IF      LNK-RNYURECEDEN-RC  NOT =   ZERO
               DISPLAY "LNK-RNYURECEDEN-RC=" LNK-RNYURECEDEN-RC 
           END-IF
           .
       2001-NYURECEDEN-GET-EXT.
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
           MOVE    RECE82-NAME         TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"      USING   ORCSKANACHK2AREA
           IF      KANACHK2-RC4        =   ZERO
               MOVE    SPACE               TO  RECE82-NAME
               MOVE    KANACHK2-OUT-INPUT (1:KANACHK2-MAX)
                                           TO  RECE82-NAME
      *
               IF      RECE82-NAME (1:2)   =   "　"
                   IF      RECE82-KANANAME (1:2)   =   "　"
                       PERFORM VARYING IDX FROM    1   BY  2
                               UNTIL   IDX >       49
                           IF      RECE82-KANANAME (IDX:2) =   "　"
                               CONTINUE
                           ELSE     
                               MOVE    RECE82-KANANAME (IDX:)
                                                           TO  WRK-DATA
                               MOVE    49                  TO  IDX
                           END-IF
                       END-PERFORM 
                   ELSE
                       MOVE    RECE82-KANANAME     TO  WRK-DATA
                   END-IF
               ELSE
                   MOVE    RECE82-NAME         TO  WRK-DATA
               END-IF
           ELSE
               IF      RECE82-KANANAME (1:2)   =   "　"
                   PERFORM VARYING IDX FROM    1   BY  2
                           UNTIL   IDX >       49
                       IF      RECE82-KANANAME (IDX:2)   =   "　"
                           CONTINUE
                       ELSE     
                           MOVE    RECE82-KANANAME (IDX:) TO  WRK-DATA
                           MOVE    49                     TO  IDX
                       END-IF
                   END-PERFORM 
               ELSE
                   MOVE    RECE82-KANANAME     TO  WRK-DATA
               END-IF
           END-IF 
           MOVE    40                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    性別
           MOVE    RECE82-SEX          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    生年月日
      *****MOVE    RECE82-BIRTHDAY     TO  WRK-SYMD
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    RECE82-BIRTHDAY     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    予備３
           PERFORM 5005-NUM-HENSYU-SEC
      *    入院年月日
      *****MOVE    RECE82-NYUINYMD     TO  WRK-SYMD
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    RECE82-NYUINYMD     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    病棟区分
           MOVE    1               TO  IDZ                  
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      RECE82-BYOTOKBN (IDX)   =   SPACE
               MOVE    RECE82-BYOTOKBN (IDX)
                                           TO  WRK-DATA (IDZ:2)
               ADD     2               TO  IDZ
           END-PERFORM                            
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    予備４
           PERFORM 5004-MOJI-HENSYU-SEC
      *    予備５
           PERFORM 5004-MOJI-HENSYU-SEC         
      *    病床数
           PERFORM 5005-NUM-HENSYU-SEC         
      *    カルテ番号等
           MOVE    RECE82-PTNUM        TO  WRK-DATA     
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
           IF      RECE82-NINPUFLG >   ZERO
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
           MOVE    RECE82-RECEKAISU    TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    業務災害・通勤災害
           EVALUATE    RECE82-SAIGAIKBN
               WHEN    "1"
                   MOVE    "1"                 TO  WRK-DATA
               WHEN    "2"
                   MOVE    "3"                 TO  WRK-DATA
           END-EVALUATE
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    帳票種別
           EVALUATE    RECE82-HKNKBN
               WHEN    "1"
                   MOVE    "2"                 TO  WRK-DATA
               WHEN    "2"
                   MOVE    "4"                 TO  WRK-DATA
           END-EVALUATE
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    年金証書番号
           IF      RECE82-HKNKBN       =   "2"
               MOVE    RECE82-KOFUNUM      TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
            PERFORM 5004-MOJI-HENSYU-SEC
      *    労働保険番号
           IF      RECE82-HKNKBN       =   "1"
               IF      RECE82-KOFUNUM  NOT =   SPACE
                   MOVE    RECE82-KOFUNUM      TO  WRK-DATA
               ELSE
                   MOVE    "99999999999999"    TO  WRK-DATA
               END-IF
               MOVE    14                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    傷病年月日
           IF      RECE82-HKNKBN       =   "1"
      *********MOVE    RECE82-SHOBYOYMD    TO  WRK-SYMD
      *********PERFORM 5002-HIZUKE-HEN-SEC
      *********MOVE    WRK-HENYMD          TO  WRK-DATA
      *********MOVE    7                   TO  WRK-NAGASA
               MOVE    RECE82-SHOBYOYMD    TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    新継再別
           MOVE    RECE82-SINKEI       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    転帰事由
           MOVE    RECE82-TENKI        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    療養期間−初日
           MOVE    SPACE               TO  WRK-SYMD 
           IF      RECE82-RYOSTYMD NOT =   SPACE
               IF      RECE82-RYOSTYMD     <   WRK-STYMD
                   MOVE    WRK-STYMD           TO  WRK-SYMD
               ELSE      
                   MOVE    RECE82-RYOSTYMD     TO  WRK-SYMD
               END-IF    
           ELSE
               MOVE    WRK-STYMD (1:6)     TO  WRK-SYMD-YM
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF      RECE82-DAY (IDX)    =   RECE82-HKNCOMBI
                       MOVE    IDX                 TO  WRK-SYMD-DD
                       MOVE    31                  TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    WRK-SYMD            TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    療養期間−末日
           MOVE    SPACE               TO  WRK-SYMD 
           IF      RECE82-RYOEDYMD NOT =  SPACE
               IF      RECE82-RYOEDYMD    >   WRK-EDYMD
                   MOVE    WRK-EDYMD          TO  WRK-SYMD
               ELSE      
                   MOVE    RECE82-RYOEDYMD    TO  WRK-SYMD
               END-IF    
           ELSE
               MOVE    WRK-EDYMD (1:6)     TO  WRK-SYMD-YM
             PERFORM VARYING IDX FROM    31  BY  -1
                     UNTIL   IDX <       1
                   IF      RECE82-DAY (IDX)    =   RECE82-HKNCOMBI
                       MOVE    IDX                 TO  WRK-SYMD-DD
                       MOVE    1                   TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    WRK-SYMD            TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療実日数
           MOVE    RECE82-JNISSU       TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    労働者の氏名（カナ）
           MOVE    1                   TO  IDY
           PERFORM VARYING IDX FROM    1   BY  2
                   UNTIL   IDX >       49
                   OR      RECE82-KANANAME (IDX:1) =   SPACE
               IF      RECE82-KANANAME (IDX:2)
                                           >=  "ァ"  AND
                                           <=  "ヶ"
                   EVALUATE    RECE82-KANANAME (IDX:2)
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
                           MOVE    RECE82-KANANAME (IDX:2)
                                               TO  WRK-DATA (IDY:2)
                   END-EVALUATE
                   ADD     2               TO  IDY    
               ELSE    
                   EVALUATE    RECE82-KANANAME (IDX:2)
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
                           MOVE    RECE82-KANANAME (IDX:2)
                                                   TO  WRK-DATA (IDY:2)
                           ADD     2               TO  IDY    
                   END-EVALUATE
               END-IF
           END-PERFORM
           IF      IDY                 =   1
               MOVE    RECE82-KANANAME     TO  WRK-DATA
           END-IF
           MOVE    40                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-MOJI-CHK
           PERFORM 5004-MOJI-HENSYU-SEC
      *    事業の名称
           IF      RECE82-HKNKBN       =   "1"
               MOVE    RECE82-JIGYOUNAME   TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    事業場の所在地
           INITIALIZE                      WRK-PREF-AREA
           IF      RECE82-HKNKBN       =   "1"
               IF      RECE82-PREFNAME NOT =   SPACE
                   EVALUATE    RECE82-PREFKBN
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
               IF      RECE82-CITYNAME NOT =   SPACE
                   EVALUATE    RECE82-CITYKBN
                       WHEN    "1"
                           MOVE    "郡"            TO  WRK-CITYKBN
                       WHEN    "2"
                           MOVE    "市"            TO  WRK-CITYKBN
                       WHEN    "3"
                           MOVE    "区"            TO  WRK-CITYKBN
                   END-EVALUATE
               END-IF
               STRING  RECE82-PREFNAME     DELIMITED  BY  SPACE
                       WRK-PREFKBN         DELIMITED  BY  SPACE
                       RECE82-CITYNAME     DELIMITED  BY  SPACE
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
           MOVE    RECE82-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE82-PTID         TO  RECECOM-PTID
           MOVE    RECE82-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
           MOVE    "AA"                TO  RECECOM-SJKBN
           MOVE    RECE82-HKNCOMBI     TO  RECECOM-HKNCOMBI
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
               IF      RECECOM-HKNCOMBI    =   RECE82-HKNCOMBI
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
           MOVE    RECE82-TOTALTEN     TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    小計点数金額換算（イ）
           MOVE    RECE82-TOTALKIN     TO  WRK-NUM
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    小計金額（ロ）
           MOVE    RECE82-KINGAKU2-SYO TO  WRK-NUM
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    食事療養合計回数
           MOVE    RECE82-SHOKUJITOTALDAY
                                       TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    食事療養合計金額（ハ）
           MOVE    RECE82-SHOKUJITOTALKIN
                                       TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
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
           INITIALIZE                      RECE83-REC
           MOVE    RECE82-KEY          TO  RECE83-KEY1
      *
           START   RECE83-FILE
               KEY IS    >=   RECE83-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE83
               NOT INVALID
                   PERFORM 990-RECE83-NEXT-SEC
           END-START
      *     
           PERFORM UNTIL  ( FLG-RECE83 =   1           )
                   OR     ( RECE82-KEY
                                   NOT =   RECE83-KEY1 )
      *        傷病名編集処理
               PERFORM 20061-BYOMEI-HENSYU-SEC
      *             
               PERFORM 990-RECE83-NEXT-SEC
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
           IF      RECE83-BYOMEIHENFLG =   "1"
               CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE83-BYOMEICD (IDX)   =   SPACE
                   IF      RECE83-BYOMEICD(IDX)   =   ALL "9"
                       CONTINUE
                   ELSE  
                       IF      RECE83-BYOMEICD(IDX)(1:3)
                                                   NOT =   "ZZZ"
                           MOVE    RECE83-BYOMEICD(IDX)
                                                   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      BYO-HKNSKYKBN   =   1
                               IF      RECE83-BYOMEICD (2)
                                                   NOT =   SPACE
                                   MOVE    "1" TO  RECE83-BYOMEIHENFLG
                               END-IF
                               MOVE    21      TO  IDX
                           ELSE
                               IF      BYO-HAISIYMD    NOT =   SPACE
                                   IF      BYO-HAISIYMD    <
                                                       RECE83-SRYYMD
                                       MOVE    "1"     TO
                                                   RECE83-BYOMEIHENFLG
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
           IF      RECE83-UTAGAIFLG    NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    RECE83-UTAGAIFLG    TO  SMKBYO-UTAGAIFLG
               MOVE    RECE83-IN-BYOMEI    TO  SMKBYO-BYOMEI
               MOVE    RECE83-BYOMEIHENFLG
                                           TO  SMKBYO-BYOMEIHENFLG
               MOVE    RECE83-BYOMEICD-G   TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
               MOVE    SMKBYO-BYOMEI       TO  RECE83-BYOMEI
               MOVE    SMKBYO-BYOMEIHENFLG TO  RECE83-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  RECE83-BYOMEICD-G
           ELSE
               MOVE    RECE83-IN-BYOMEI    TO  RECE83-BYOMEI 
           END-IF
      *
      *    傷病名コード／修飾語コードの取得
           MOVE    SPACE               TO  WRK-BYOMEI-AREA
      *
           IF      RECE83-BYOMEIHENFLG =   "1"
      *        編集病名のとき未傷病名コードにする
               MOVE    "0000999"           TO  WRK-BYOMEICD
           ELSE
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE83-BYOMEICD(IDX)   =   SPACE
                 IF      RECE83-BYOMEICD(IDX) =   ALL "9"
                     CONTINUE
                 ELSE
                   IF      RECE83-BYOMEICD(IDX)(1:3)  NOT =   "ZZZ"
                       IF      WRK-BYOMEICD                =   SPACE
                           MOVE    RECE83-BYOMEICD(IDX)
                                                   TO  WRK-BYOMEICD
                       END-IF
                   ELSE
                       ADD     1           TO  IDY
                       MOVE    RECE83-BYOMEICD(IDX)(4:)
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
      *****MOVE    RECE83-SRYYMD      TO  WRK-SYMD
      *****PERFORM 5002-HIZUKE-HEN-SEC
      *****MOVE    WRK-HENYMD          TO  WRK-DATA
      *****MOVE    7                   TO  WRK-NAGASA
           MOVE    RECE83-SRYYMD       TO  WRK-DATA
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
               MOVE    RECE83-BYOMEI       TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
           END-IF    
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    主傷病
           IF    ( RECE83-SYUBYOFLG-X  =   "1"  )
           AND   ( SYS-2005-SYUBYOHENFLG
                                   NOT =   ZERO )
               MOVE    "01"                TO  WRK-DATA
           END-IF 
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    補足コメント
           MOVE    RECE83-HOSOKU-COMMENT
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
      *    症状詳記（手術歴）
           MOVE    ZERO                TO  FLG-TOYAKUFLG
           PERFORM 2008-COMMENT-HEN-01-SEC
      *                                        
      *    摘要情報
           INITIALIZE                      RECE84-REC
           MOVE    RECE82-KEY          TO  RECE84-KEY1
      *
           START   RECE84-FILE
               KEY IS    >=   RECE84-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE84
               NOT INVALID
                   PERFORM 990-RECE84-NEXT-SEC
           END-START
           PERFORM     UNTIL  ( FLG-RECE84           =   1           )
                       OR     ( RECE82-KEY       NOT =   RECE84-KEY1 )
                       OR     ( RECE84-SRYKBN        >=  "90"        )
      *
      *        検査に判断料がある場合の検査、又は薬剤料の減点がある場合の
      *        注射（その他）の編集
               IF    ( RECE84-SRYKBN       >   "60"    OR  "33" )
               AND   ( TBL-KENSA-RECE84-MAX
                                           >   0                )
                   MOVE    RECE84-REC          TO  WRK-TAIHI-RECE84-REC
      *
                   PERFORM 20078-KENSA-HANDANRYO-HEN-SEC
      *
                   MOVE    WRK-TAIHI-RECE84-REC
                                               TO  RECE84-REC
      *
                   INITIALIZE                  TBL-KENSA-RECE84-AREA
               END-IF
      *
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
      *
      *        仮レセデータフラグ設定
               IF      RECE84-TOYAKUFLG =   SPACE
                   MOVE    ZERO              TO  FLG-TOYAKUFLG
               ELSE
                   MOVE    1                 TO  FLG-TOYAKUFLG
               END-IF 
      *
      *       摘要データ編集処理
               PERFORM 20074-ZAI-KOUI-SORT-SEC
               PERFORM 20072-ZAI-HENSYU-SEC
      * 
               PERFORM 990-RECE84-NEXT-SEC
           END-PERFORM
      *
      *    検査に判断料がある場合の検査、又は薬剤料の減点がある場合の
      *    注射（その他）の編集
           IF    ( TBL-KENSA-RECE84-MAX
                                       >   0        )
               PERFORM 20078-KENSA-HANDANRYO-HEN-SEC
      *
               INITIALIZE                  TBL-KENSA-RECE84-AREA
           END-IF
      *
           MOVE    ZERO                TO  FLG-TOYAKUFLG
      *
      *    入退院履歴編集処理
           PERFORM 20079-NYUTAI-RIREKI-HEN-SEC
      *
      *    入院会計編集処理
           PERFORM 20077-NYUIN-HENSYU-SEC
      *
           .
       2007-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    入退院履歴編集処理
      *****************************************************************
       20079-NYUTAI-RIREKI-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUTAI-RIREKI
                                           FLG-NYUIN-SRYCD
                                           FLG-NYUINYMD
           MOVE    ZERO                TO  WRK-NYUINCHUKBN
      *          
           INITIALIZE                      RECE88-REC
           MOVE    RECE82-KEY          TO  RECE88-KEY1
      *
           START   RECE88-FILE
               KEY IS    >=   RECE88-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE88
               NOT INVALID
                   PERFORM 900-RECE88-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE88       =  1           )
                   OR    ( RECE82-KEY   NOT =  RECE88-KEY1 )
      *
      *        再入院年月日か入院年月日
               IF    ( WRK-NYUINCHUKBN =   "1"             )
                   CONTINUE
               ELSE
                 IF    ( RECE88-NYUINYMD =   RECE82-NYUINYMD )
                 AND   ( FLG-NYUINYMD    =   ZERO            )
                     MOVE    1               TO  FLG-NYUINYMD
                 ELSE
                   MOVE    SPACE               TO  WRK-REC
                   MOVE    ZERO                TO  WRK-REC-LENGTH
      *
                   MOVE    "CO,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF      FLG-NYUTAI-RIREKI   =   ZERO
                       MOVE    "90"                TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    1                   TO  FLG-NYUTAI-RIREKI
                   END-IF    
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            予備１
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード
                   IF    ( RECE88-SHONUM       =   ZERO )
                   AND   ( FLG-NYUIN-SRYCD     =   ZERO )
      *                継続しない入退院があるとき初回は入院日で記録する
                       IF      RECE88-NYUINYMD <   WRK-CONS-YMD-HEISEI
                           MOVE    "入　院　"      TO  WRK-NYUTAI-COM
                           MOVE    WRK-CONS-COMMENT-SRYCD
                                                   TO  WRK-DATA
                       ELSE
                           IF      RECE88-NYUINYMD
                                               <   WRK-CONS-YMD-REIWA
                               MOVE    WRK-CONS-NYUIN-SRYCD
                                                       TO  WRK-DATA
                           ELSE
                               MOVE    WRK-CONS-NYUIN-REIWA-SRYCD
                                                       TO  WRK-DATA
                           END-IF
                       END-IF 
                       MOVE    1                   TO  FLG-NYUIN-SRYCD
                   ELSE
                       IF      RECE88-NYUINYMD <   WRK-CONS-YMD-HEISEI
                           MOVE    "再入院　"      TO  WRK-NYUTAI-COM
                           MOVE    WRK-CONS-COMMENT-SRYCD
                                                   TO  WRK-DATA
                       ELSE
                           IF      RECE88-NYUINYMD
                                               <   WRK-CONS-YMD-REIWA
                               MOVE    WRK-CONS-SAINYUIN-SRYCD
                                                           TO  WRK-DATA
                           ELSE
                               MOVE    WRK-CONS-SAINYUIN-REIWA-SRYCD
                                                           TO  WRK-DATA
                           END-IF
                       END-IF
                   END-IF
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ
                   IF      RECE88-NYUINYMD <   WRK-CONS-YMD-HEISEI
                       MOVE    RECE88-NYUINYMD     TO  WRK-SYMD
                       PERFORM 31012-SEIWA-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3    TO  WRK-NYUTAI-YMD
                       MOVE    WRK-NYUTAI-COMMENT  TO  WRK-DATA
                   ELSE
                       MOVE    RECE88-NYUINYMD     TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "2"                 TO  KANACHK-SYORI 
                       MOVE    WRK-HENYMD (2:)     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                       ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                           TO  WRK-DATA
                       END-IF
                   END-IF
                   MOVE    76                  TO  WRK-NAGASA
                   MOVE    1                   TO  WRK-END 
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *
                   PERFORM 5001-FILE-WRITE-SEC
                 END-IF
               END-IF
      *
      *        退院年月日                          
               IF    ( RECE88-TAIINYMD (1:6)   
                                           >   RECE82-SRYYM    )
               OR    ( RECE88-NYUINCHUKBN  =   "1"             )
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-REC
                   MOVE    ZERO                TO  WRK-REC-LENGTH
      *
                   MOVE    "CO,"               TO  WRK-REC
                   MOVE    3                   TO  WRK-REC-LENGTH
      *            診療識別
                   IF      FLG-NYUTAI-RIREKI   =   ZERO
                       MOVE    "90"                TO  WRK-DATA
                       MOVE    2                   TO  WRK-NAGASA
                       MOVE    1                   TO  FLG-NYUTAI-RIREKI
                   END-IF    
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            予備１
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード
                   IF      RECE88-TAIINYMD     <   WRK-CONS-YMD-HEISEI
                       MOVE    WRK-CONS-COMMENT-SRYCD
                                                   TO  WRK-DATA
                   ELSE
                       IF      RECE88-TAIINYMD <   WRK-CONS-YMD-REIWA
                           MOVE    WRK-CONS-TAIIN-SRYCD
                                                       TO  WRK-DATA
                       ELSE
                           MOVE    WRK-CONS-TAIIN-REIWA-SRYCD
                                                       TO  WRK-DATA
                       END-IF
                   END-IF
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ
                   IF      RECE88-TAIINYMD     <   WRK-CONS-YMD-HEISEI
                       MOVE    "退　院　"          TO  WRK-NYUTAI-COM
                       MOVE    RECE88-TAIINYMD     TO  WRK-SYMD
                       PERFORM 31012-SEIWA-HEN-SEC
                       MOVE    LNK-DAY2-EDTYMD3    TO  WRK-NYUTAI-YMD
                       MOVE    WRK-NYUTAI-COMMENT  TO  WRK-DATA
                   ELSE
                       MOVE    RECE88-TAIINYMD     TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "2"                 TO  KANACHK-SYORI 
                       MOVE    WRK-HENYMD (2:)     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                       TO  WRK-DATA
                       END-IF
                   END-IF
                   MOVE    76                  TO  WRK-NAGASA
                   MOVE    1                   TO  WRK-END 
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *
                   PERFORM 5001-FILE-WRITE-SEC
               END-IF
      *
               MOVE    RECE88-NYUINCHUKBN  TO  WRK-NYUINCHUKBN    
      *
               PERFORM 900-RECE88-NEXT-SEC
           END-PERFORM
      *
           .
       20079-NYUTAI-RIREKI-HEN-EXT.
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
           INITIALIZE                      WRK-RECE85-REC
                                           WRK-KASAN-RECE85-REC
                                           WRK-TEIGEN-RECE85-REC
                                           WRK-700-RECE85-AREA
                                           WRK-700-DOUJI-RECE85-AREA
                                           WRK-721-RECE85-REC
                                           WRK-721-DOUJI-RECE85-REC
                                           WRK-SRYKBN70-RECE85-REC
           INITIALIZE                      TBL-RECE855-COMMENT-AREA
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
           MOVE    RECE84-SRYKBN      TO  WRK-SRYKBN
      *
      *    検査（判断料）の退避
           IF     ( RECE84-KNSJISKBN   =   2 )
               ADD     1               TO  TBL-KENSA-RECE84-MAX
               MOVE    RECE84-REC      TO  TBL-KENSA-RECE84-REC
                                           (TBL-KENSA-RECE84-MAX)
           END-IF
           MOVE    ZERO                 TO  TBL-KENSA-RECE85-MAX
      *
           INITIALIZE                      RECE85-REC
           MOVE    RECE84-KEY1         TO  RECE85-KEY1
           MOVE    RECE84-ZAINUM       TO  RECE85-ZAINUM
           START   RECE85-FILE
               KEY IS    >=   RECE85-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE85
               NOT INVALID
                   PERFORM 990-RECE85-NEXT-SEC
           END-START
           PERFORM     UNTIL ( FLG-RECE85          =   1             )
                       OR    ( RECE84-KEY1     NOT =   RECE85-KEY1   )
                       OR    ( RECE84-ZAINUM   NOT =   RECE85-ZAINUM )
      *
             PERFORM   VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX               >   5     )
                       OR      ( RECE85-SRYCD(IDX) =   SPACE )
      *
               MOVE    ZERO                TO  FLG-TAISYO
      *        入院料はコメントか労災治療計画加算、病衣貸与料のみ対象とする
               IF      RECE84-SRYKBN       >=  "90"
                   IF    ( RECE85-SRYCD (IDX) (1:3)  =   "112" )
                       MOVE    1                     TO  FLG-TAISYO
                   ELSE
                     IF    ( RECE85-SRYCD (IDX) (1:1)
                                                 =   "8"   OR
                             RECE85-SRYCD (IDX) (1:3)
                                                 =   "008"    )
                     OR    ( RECE84-JIHOKBN  NOT =   SPACE     )
                         MOVE    1                   TO  FLG-TAISYO
                     ELSE
                       IF    ( RECE85-SRYCD (IDX)  =   "101910050"
                                                   OR  "101910060" )
                           MOVE    1                   TO  FLG-TAISYO
                       END-IF
                     END-IF
                   END-IF
               ELSE
                   MOVE    1                  TO  FLG-TAISYO
               END-IF
      *
               IF      FLG-TAISYO          =   1
                 IF    ( RECE85-RECEDENFLG(IDX) =   SPACE )
                 AND   ( RECE85-MEISKYFLG (IDX)
                                            NOT =   "2"   )
                   IF    ( RECE85-SRYCD (IDX) (1:3) = "059"   )
                       MOVE    "777770000"      TO  RECE85-SRYCD (IDX)
                   END-IF
      *
                   IF    ( RECE85-SRYCD (IDX) (1:5) = "09594" )
                     EVALUATE  RECE85-SRYCD(IDX)(6:2)
                       WHEN      "01"
                         MOVE  "101800390"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "02"
                         MOVE  "101800400"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "03"
                         MOVE  "101800410"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "04"
                         MOVE  "101800420"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "07"
                         MOVE  "101800430"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "08"
                         MOVE  "101800440"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "09"
                         MOVE  "101800450"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "10"
                         MOVE  "101800460"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
      *
                       WHEN      "21"
                         MOVE  "101801150"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "22"
                         MOVE  "101801160"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "23"
                         MOVE  "101801170"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                       WHEN      "24"
                         MOVE  "101801180"      TO  RECE85-SRYCD (IDX)
                         MOVE   1       TO  RECE85-KOKUJISKBKBN1 (IDX)
                     END-EVALUATE
                   END-IF
      *
                   EVALUATE    RECE85-SRYCD (IDX) (1:1)
                       WHEN    "0"
                       WHEN    "8"
      *                    空白または全角スペースのみのフリーコメントは記載しない
                           MOVE    ZERO                TO  WRK-SJ-CHK
                           IF      RECE85-SRYCD (IDX)  =   "810000001"
                               IF      RECE85-NAME  (IDX)  =   SPACE
                                   CONTINUE
                               ELSE
                                   PERFORM VARYING IDY FROM    1   BY  2
                                           UNTIL   IDY >       200
                                           OR  RECE85-NAME(IDX) (IDY:2)
                                                           =   SPACE
                                           OR  WRK-SJ-CHK  =   1
                                       IF       RECE85-NAME(IDX) (IDY:2)
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
                                   MOVE    RECE85-NAME(IDX)
                                                   TO  KANACHK-MAE-INPUT
                                   CALL    "ORCSKANACHK"
                                               USING   ORCSKANACHKAREA
                                   IF    ( KANACHK-RC  =   ZERO )
                                   AND   ( KANACHK-MAX >   ZERO )
                                       MOVE    SPACE
                                                   TO  RECE85-NAME(IDX)
                                       MOVE    KANACHK-OUT-INPUT
                                                       (1:KANACHK-MAX)
                                                   TO  RECE85-NAME(IDX)
                                   END-IF
                               END-IF
                           ELSE
                               MOVE    1               TO  WRK-SJ-CHK
                           END-IF
                           IF      WRK-SJ-CHK          =   ZERO
                               CONTINUE
                           ELSE                      
      *                        検査（判断料）の場合の退避
                               IF      RECE84-KNSJISKBN   =   2
                                   PERFORM
                                       20013-KENSA-HANDANRYO-TAIHI-SEC
                               ELSE
                                   IF      RECE84-SRYKBN   =   "11"   OR
                                                               "12"   OR
                                                               "13"   OR
                                                               "14"   OR
                                                               "70"
                                     PERFORM
                                      200711-SRYKBN13-COM-TAIHI-SEC
                                   ELSE
                                     PERFORM 200711-COMMENT-TAIHI-SEC
                                   END-IF
                               END-IF
                           END-IF
                       WHEN    "1"
      *                    検査（判断料）の場合の退避
                           IF     ( RECE84-KNSJISKBN   =   2 )
                               PERFORM 20013-KENSA-HANDANRYO-TAIHI-SEC
                           ELSE
                               PERFORM 200711-KOUI-TAIHI-SEC
                           END-IF
                       WHEN    "6"
                           PERFORM 200711-YKZ-TAIHI-SEC
                       WHEN    "7"
                           PERFORM 200711-KIZAI-TAIHI-SEC
                           IF      RECE84-ZAIREQFLG       =   1
                               PERFORM 200711-KIZAI-ACCT-TAIHI-SEC
                           END-IF
                   END-EVALUATE
                 END-IF
               END-IF
             END-PERFORM
      *
             PERFORM 990-RECE85-NEXT-SEC
           END-PERFORM
      *
      *    画像診断のとき順番を変える
      *
           IF      FLG-SRYSYUKBN721    >   ZERO
               MOVE     FLG-SRYSYUKBN721   TO  IDX 
               MOVE    WRK-721-RECE85-REC  TO  RECE85-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                   MOVE    WRK-SRYKBN70-RECE85-REC
                                               TO  RECE85-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX 
                       MOVE     FLG-SRYSYUKBN700  (IDXY)  TO  IDX 
                       MOVE    WRK-700-RECE85-REC (IDXY)  TO  RECE85-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-PERFORM    
               END-IF
           END-IF
           IF      FLG-SRYSYUKBN721-DOUJI  >   ZERO
               MOVE    FLG-SRYSYUKBN721-DOUJI      TO  IDX 
               MOVE    WRK-721-DOUJI-RECE85-REC    TO  RECE85-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                   MOVE    WRK-SRYKBN70-RECE85-REC
                                               TO  RECE85-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-DOUJI-MAX  >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >
                                           WRK-SRYSYUKBN700-DOUJI-MAX
                       MOVE    FLG-SRYSYUKBN700-DOUJI (IDXY)
                                                          TO  IDX 
                       MOVE    WRK-700-DOUJI-RECE85-REC (IDXY)
                                                          TO  RECE85-REC
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
           IF      RECE85-SRYCD (IDX) (1:1) =   "0"
               IF    ( RECE85-SRYCD (IDX)(1:3)  =   "002"             )
               OR    ( RECE85-SRYCD (IDX)(1:3)  =   "008"             )
               OR    ( RECE85-SRYCD (IDX)       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE85-SRYCD (IDX)       =   WRK-CONS-COMMENT2 ) 
      *********OR    ( RECE85-SRYCD (IDX)       =   WRK-CONS-COMMENT3 ) 
      *********OR    ( RECE85-SRYCD (IDX)       =   WRK-CONS-COMMENT4 ) 
                   CONTINUE
               ELSE
                   GO  TO  200711-COMMENT-TAIHI-EXT
               END-IF
           END-IF    
      *     
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCR0985 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    RECE85-SRYCD (IDX)  TO
                                           TBL-KOUI-SRYCD (WRK-TBLMAX1)
               MOVE    RECE85-NAME  (IDX)  TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
      *
               IF      RECE85-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE85-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                           TBL-KOUI-SRYCD (WRK-TBLMAX1)
                   END-IF
      *            コメント
                   IF    ( RECE85-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE85-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE85-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE85-INPUTCOMENT (IDX)     TO
                                           TBL-KOUI-NAME  (WRK-TBLMAX1)
                   END-IF
               END-IF
      *        コメントパターン２０のコメントコード
               IF      RECE85-SRYCD (IDX)(1:2)
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
           IF      RECE85-SRYCD (IDX) (1:1) =   "0"
               IF    ( RECE85-SRYCD (IDX)(1:3)  =   "002"             )
               OR    ( RECE85-SRYCD (IDX)(1:3)  =   "008"             )
               OR    ( RECE85-SRYCD (IDX)       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE85-SRYCD (IDX)       =   WRK-CONS-COMMENT2 ) 
      *********OR    ( RECE85-SRYCD (IDX)       =   WRK-CONS-COMMENT3 ) 
      *********OR    ( RECE85-SRYCD (IDX)       =   WRK-CONS-COMMENT4 ) 
                   CONTINUE
               ELSE
                   GO  TO  200711-SRYKBN13-COM-TAIHI-EXT
               END-IF
           END-IF    
      *               
           IF      WRK-TBLMAX8         >=  100
               MOVE    "*** ORCR0985 COMMENT TABLE  OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX8
      *
               MOVE    RECE85-SRYCD (IDX)  TO
                                   TBL-COMMENT-SRYCD (WRK-TBLMAX8)
               MOVE    RECE85-NAME  (IDX)  TO
                                   TBL-COMMENT-NAME  (WRK-TBLMAX8)
      *
               IF      RECE85-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE85-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                       TBL-COMMENT-SRYCD (WRK-TBLMAX8)
                   END-IF
      *            コメント
                   IF    ( RECE85-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE85-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE85-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE85-INPUTCOMENT (IDX)     TO
                                       TBL-COMMENT-NAME  (WRK-TBLMAX8)
                   END-IF
               END-IF
      *        コメントパターン２０のコメントコード
               IF      RECE85-SRYCD (IDX)(1:2)
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
           IF    ( RECE84-SRYKBN      =   "13"    OR  "14"     )
           AND   ( RECE85-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
      *        加算項目だが基本項目扱いのとき剤を別にする
               IF    ( RECE85-KOKUJISKBKBN1 (IDX)
                                       =   "1" OR  "3" OR  "5" )
               AND   ( RECE85-DATAKBN (IDX)
                                       =   "2"                 )
                   ADD     1               TO  TBL-KOUI-SRYKBN12-MAX
                   MOVE    RECE85-KEY      TO  TBL-KOUI-SRYKBN12-KEY
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    IDX             TO
                                       TBL-KOUI-SRYKBN12-SHORI-RENNUM
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE84-ZAIKAISU
                                           TO  TBL-KOUI-SRYKBN12-KAISU
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE85-SRYCD (IDX)
                                           TO  TBL-KOUI-SRYKBN12-SRYCD
                                               (TBL-KOUI-SRYKBN12-MAX)    
                   MOVE    RECE85-CHUKSNCD (IDX)
                                           TO
                                           TBL-KOUI-SRYKBN12-CHUKSNCD
                                               (TBL-KOUI-SRYKBN12-MAX)
                   MOVE    RECE85-TEN (IDX)
                                           TO  TBL-KOUI-SRYKBN12-TEN
                                               (TBL-KOUI-SRYKBN12-MAX)
                   ADD     RECE85-TEN (IDX)
                                           TO  TBL-KOUI-SRYKBN12-TEN-G
                   GO  TO  200711-KOUI-TAIHI-EXT
               END-IF
           END-IF
      *
           IF    ( RECE84-SRYKBN      =   "13"    OR  "14" )
           AND   ( RECE85-KOKUJISKBKBN1 (IDX)
                                       =   "7" OR  "9"      )
           AND   ( RECE85-CHUKSNCD      (IDX)
                                       =   "1419"           )
      *        注加算コードが１４１９の加算項目のとき剤を別にする
               ADD     1               TO  TBL-KOUI-SRYKBN12-CHU-MAX
               MOVE    RECE85-KEY      TO  TBL-KOUI-SRYKBN12-CHU-KEY
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    IDX             TO
                                   TBL-KOUI-SRYKBN12-CHU-SHORI-NUM
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE84-ZAIKAISU
                                       TO  TBL-KOUI-SRYKBN12-CHU-KAISU
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE85-SRYCD (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-SRYCD
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE85-TEN (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-TEN
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               MOVE    RECE85-CHUKSNCD (IDX)
                                       TO
                                       TBL-KOUI-SRYKBN12-CHU-CHUKSNCD
                                           (TBL-KOUI-SRYKBN12-CHU-MAX)
               ADD     RECE85-TEN (IDX)
                                       TO  TBL-KOUI-SRYKBN12-CHU-TEN-G
               GO  TO  200711-KOUI-TAIHI-EXT
           END-IF
      *
      *    画像診断の撮影料のとき順番を変える（同時撮影以外）
           IF     RECE85-SRYSYUKBN-X (IDX)
                                       =   "721"
             MOVE    RECE85-REC          TO  WRK-RECE85-REC
             MOVE    IDX                 TO  IDW1
      * 
             IF     RECE85-SRYCD (IDX)
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
                   MOVE    WRK-721-RECE85-REC  TO  RECE85-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO                TO  FLG-SRYSYUKBN721
                   INITIALIZE                  WRK-721-RECE85-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX 
                       MOVE    WRK-SRYKBN70-RECE85-REC
                                               TO  RECE85-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       MOVE    ZERO            TO  FLG-SRYKBN70-TEIGEN
                       INITIALIZE              WRK-SRYKBN70-RECE85-REC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX 
                           MOVE     FLG-SRYSYUKBN700  (IDXY)
                                                           TO  IDX 
                           MOVE    WRK-700-RECE85-REC (IDXY)
                                                           TO RECE85-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM    
      *****************MOVE    ZERO                TO  FLG-SRYSYUKBN700
                       INITIALIZE                  WRK-700-RECE85-AREA
                   END-IF
               END-IF
               IF      FLG-SRYSYUKBN721-DOUJI  >   ZERO
                   MOVE    FLG-SRYSYUKBN721-DOUJI      TO  IDX 
                   MOVE    WRK-721-DOUJI-RECE85-REC    TO  RECE85-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO               TO  FLG-SRYSYUKBN721-DOUJI
                   INITIALIZE                   WRK-721-DOUJI-RECE85-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                                   TO  IDX 
                       MOVE    WRK-SRYKBN70-RECE85-REC
                                                   TO  RECE85-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-DOUJI-MAX  >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >
                                           WRK-SRYSYUKBN700-DOUJI-MAX
                           MOVE    FLG-SRYSYUKBN700-DOUJI (IDXY)
                                                          TO  IDX 
                           MOVE    WRK-700-DOUJI-RECE85-REC (IDXY)
                                                          TO  RECE85-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM    
                       INITIALIZE              WRK-700-DOUJI-RECE85-AREA
                   END-IF
               END-IF
             END-IF  
      *
             MOVE    WRK-RECE85-REC      TO  RECE85-REC
             MOVE    IDW1                TO  IDX
      *
             IF     RECE85-SRYCD (IDX)
                                   NOT =   WRK-CONS-DOUJI-SATUEI (1)
                                       AND WRK-CONS-DOUJI-SATUEI (2) 
                                       AND WRK-CONS-DOUJI-SATUEI (3) 
                                       AND WRK-CONS-DOUJI-SATUEI (4) 
                                       AND WRK-CONS-DOUJI-SATUEI (5) 
                                       AND WRK-CONS-DOUJI-SATUEI (6) 
                                       AND WRK-CONS-DOUJI-SATUEI (7) 
                                       AND WRK-CONS-DOUJI-SATUEI (8)
               MOVE    IDX                 TO  FLG-SRYSYUKBN721
               MOVE    RECE85-REC          TO WRK-721-RECE85-REC
             ELSE
               MOVE    IDX                 TO  FLG-SRYSYUKBN721-DOUJI
               MOVE    RECE85-REC          TO WRK-721-DOUJI-RECE85-REC
             END-IF
           ELSE
               IF    ( RECE85-SRYCD (IDX)  =  WRK-CONS-SRYKBN70-TEIGEN )
               AND   ( FLG-SRYSYUKBN721    >  ZERO                     )
                   MOVE    IDX         TO  FLG-SRYKBN70-TEIGEN
                   MOVE    RECE85-REC  TO  WRK-SRYKBN70-RECE85-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               ELSE
                   IF    ( RECE85-SRYSYUKBN-X (IDX)
                                               =   "700"  )
                   AND   ( RECE85-DATAKBN   (IDX)
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
                           MOVE    RECE85-REC  TO
                                           WRK-700-DOUJI-RECE85-REC
                                            (WRK-SRYSYUKBN700-DOUJI-MAX) 
                       ELSE
                           ADD     1           TO  WRK-SRYSYUKBN700-MAX
                           MOVE    IDX         TO  FLG-SRYSYUKBN700
                                                  (WRK-SRYSYUKBN700-MAX) 
                           MOVE    RECE85-REC  TO WRK-700-RECE85-REC
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
               MOVE    "*** ORCR0985 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    WRK-TBLMAX1         TO  WRK-SRYCD1-MAX
      *
               MOVE    RECE85-KEY          TO  TBL-KOUI-KEY
                                                        (WRK-TBLMAX1)
               MOVE    IDX                 TO  TBL-KOUI-SHORI-RENNUM
                                                        (WRK-TBLMAX1)
               MOVE    RECE85-SRYACTINF (IDX)  TO
                                     TBL-KOUI-SRYACTINF (WRK-TBLMAX1)
               MOVE    RECE85-NAMEINF (IDX)    TO
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
           ADD     1               TO  TBL-KENSA-RECE85-MAX
           MOVE    RECE85-KEY      TO  TBL-KENSA-RECE85-KEY
                           (TBL-KENSA-RECE84-MAX TBL-KENSA-RECE85-MAX)
           MOVE    IDX             TO  TBL-KENSA-RECE85-SHORI-RENNUM
                           (TBL-KENSA-RECE84-MAX TBL-KENSA-RECE85-MAX)
           MOVE    RECE85-SRYACTINF (IDX)
                                   TO  TBL-KENSA-RECE85-SRYACTINF
                           (TBL-KENSA-RECE84-MAX TBL-KENSA-RECE85-MAX)
           MOVE    RECE85-NAMEINF (IDX)
                                   TO  TBL-KENSA-RECE85-NAMEINF
                           (TBL-KENSA-RECE84-MAX TBL-KENSA-RECE85-MAX)
           IF      RECE85-SRYCD (IDX) (1:1)  =   "1"
               MOVE    TBL-KENSA-RECE85-MAX 
                                       TO  TBL-KENSA-SRYCD-MAX
                                                (TBL-KENSA-RECE84-MAX)
           ELSE
               IF      RECE85-SRYCD (IDX)   =   "810000001"
                   CONTINUE
               ELSE
      *            部位／薬剤／ユーザーコメント
                   IF      RECE85-SRYCD (IDX) (1:1)     =   "0"
                       MOVE    "810000001"     TO  
                                       TBL-KENSA-RECE85-SRYCD
                           (TBL-KENSA-RECE84-MAX TBL-KENSA-RECE85-MAX)
                   END-IF
      *            コメント
                   IF    ( RECE85-SRYCD (IDX) (1:1)     =   "8"   )
                   OR    ( RECE85-SRYCD (IDX) (1:3)     =   "008"  AND
                           RECE85-SRYCD (IDX) (4:1) NOT =   "2"   )
                       MOVE    RECE85-INPUTCOMENT (IDX)     TO
                                       TBL-KENSA-RECE85-NAME
                           (TBL-KENSA-RECE84-MAX TBL-KENSA-RECE85-MAX)
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
           IF      RECE85-SRYCD (IDX)  =   WRK-CONS-TEIGEN
               CONTINUE
           ELSE
               IF    ( RECE84-YKZTEN       =   ZERO  )
               AND   ( RECE84-TOYAKUFLG    =   SPACE )
               AND   ( RECE84-YKZKISAIFLG  =   SPACE )
                   GO  TO  200711-YKZ-TAIHI-EXT
               END-IF
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX6
      *
           IF      WRK-TBLMAX6         =   1
               IF      RECE85-SRYCD (IDX)
                                       =   WRK-CONS-TEIGEN
                   MOVE    1               TO  FLG-TEIGEN
               END-IF
           END-IF
      *     
           IF      WRK-TBLMAX6         >   100
               MOVE    "*** ORCR0985 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               MOVE    RECE85-KEY          TO
                                   TBL-ZAIW-KEY        (1 WRK-TBLMAX6)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM
                                                       (1 WRK-TBLMAX6)
               MOVE    RECE85-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (1 WRK-TBLMAX6)
               MOVE    RECE85-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (1 WRK-TBLMAX6)
               MOVE    RECE85-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (1 WRK-TBLMAX6)
               MOVE    RECE85-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(1 WRK-TBLMAX6)
               MOVE    RECE85-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (1 WRK-TBLMAX6)
               MOVE    RECE85-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (1 WRK-TBLMAX6)
               MOVE    RECE85-NAME       (IDX)  TO
                                   TBL-ZAIW-NAME       (1 WRK-TBLMAX6)
               MOVE    RECE85-YUKOKETA   (IDX)  TO
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
           IF      RECE84-SRYKBN    NOT =   "70"
               IF    ( RECE84-KIZAITEN  =   ZERO  )
               AND   ( RECE84-TOYAKUFLG =   SPACE )
                   GO  TO  200711-KIZAI-TAIHI-EXT
               END-IF
           END-IF
      *
      *    酸素補正率１．３（１気圧）のとき
           IF     RECE85-SRYCD (IDX)    =   WRK-CONS-SANSO-HOSEI
               GO  TO  200711-KIZAI-TAIHI-EXT
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX7
      *     
           IF      WRK-TBLMAX7         >   100
               MOVE    "*** ORCR0985 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               MOVE    RECE85-KEY          TO
                                   TBL-ZAIW-KEY        (2 WRK-TBLMAX7)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM(2 WRK-TBLMAX7)
               MOVE    RECE85-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
               MOVE    RECE85-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
               MOVE    RECE85-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (2 WRK-TBLMAX7)
               MOVE    RECE85-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(2 WRK-TBLMAX7)
               MOVE    RECE85-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (2 WRK-TBLMAX7)
               MOVE    RECE85-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (2 WRK-TBLMAX7)
               MOVE    RECE85-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF    (2 WRK-TBLMAX7)
               MOVE    RECE85-RECEDENNAME(IDX)  TO
                                   TBL-ZAIW-RECEDENNAME(2 WRK-TBLMAX7)
      *
               IF    ( RECE85-DATAKBN (IDX)
                                           =   "3"  )
               AND   ( RECE84-SRYKBN      =   "70" )
                   MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX 
               ELSE
                   IF       RECE85-SRYCD (IDX)
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
           IF    ( RECE85-SRYCD (IDX)  =   WRK-CONS-SANSO1
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
                   MOVE    "*** ORCR0985 TABLE 100 OVER  ***"
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
           IF      RECE84-SRYKBN    NOT =   "70"
               IF    ( RECE84-KIZAITEN  =   ZERO  )
               AND   ( RECE84-TOYAKUFLG =   SPACE )
                   GO  TO  200711-KIZAI-ACCT-TAIHI-EXT
               END-IF
           END-IF
      *                                   
           ADD     1                   TO  WRK-TBLMAX7-SRT
      *     
           IF      WRK-TBLMAX7-SRT     >   100
               MOVE    "*** ORCR0985 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               MOVE    RECE85-KEY          TO
                                   TBL-ZAIW-KEY      (3 WRK-TBLMAX7-SRT)
               MOVE    IDX                 TO 
                                   TBL-ZAIW-SHORI-RENNUM
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD    (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD   (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-TEN        (IDX) TO
                                   TBL-ZAIW-TEN      (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN  (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF  (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-TOKUKIZAISBT1 (IDX)  TO
                                   TBL-ZAIW-TOKUKIZAISBT1
                                                     (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-SANSOKBN  (IDX)  TO
                                   TBL-ZAIW-SANSOKBN (3 WRK-TBLMAX7-SRT)
               MOVE    RECE85-RECEDENNAME  (IDX)   TO
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
           IF      RECE84-SRYKBN       =   "50"    OR  "54"    OR
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
               IF    ( RECE84-SRYKBN      =   "40" )
               AND   ( TBL-KOUI-KOKUJISKBKBN1 (IDX)
                                           =   "7"  )
                   MOVE    1                   TO
                                   WRK-SRYKBN40-KOKUJISKBKBN1
               END-IF
      *
      *        コメントを最初に編集（手術のとき）
               IF      RECE84-SRYKBN      =   "50"
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
                                      AND   ( RECE84-SRYKBN
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
           IF      RECE84-SRYKBN      =   "70"                 
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
               INITIALIZE                  TBL-RECE855-COMMENT-AREA
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
                       IF      RECE84-SETCOMFLG   =   "1"
                           INITIALIZE               WRK-RECE855-REC
                           MOVE    TBL-KOUI-KEY (IDX)
                                                TO  WRK-RECE855-KEY1
                           MOVE    TBL-KOUI-SHORI-RENNUM (IDX)
                                                TO
                                                WRK-RECE855-SHORI-RENNUM
                           MOVE    ZERO         TO  WRK-RECE855-SEQNO
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
                         IF      RECE84-ZAI-KINGAKU    >   ZERO
                           IF      RECE84-ZAI-TENSU      >   ZERO
                               EVALUATE    RECE84-SRYKBN
                                   WHEN    "70"
                                       COMPUTE WRK-TENSU   =
                                               RECE84-ZAI-TENSU    -
                                               RECE84-SYUTEN2
                                   WHEN    "12"
                                   WHEN    "13"
                                   WHEN    "14"
                                       COMPUTE WRK-TENSU   =
                                           RECE84-ZAI-TENSU    -
                                           TBL-KOUI-SRYKBN12-TEN-G
                                                               -
                                           TBL-KOUI-SRYKBN12-CHU-TEN-G
                                   WHEN    OTHER  
                                       MOVE    RECE84-ZAI-TENSU
                                                       TO  WRK-TENSU
                               END-EVALUATE
                               MOVE    WRK-TENSU         TO  WRK-NUM
                               MOVE    7                 TO  WRK-NAGASA
                           END-IF
                         ELSE
                           IF      RECE84-SYUTEN1      >   ZERO
                               EVALUATE    RECE84-SRYKBN
                                   WHEN    "70"
                                       COMPUTE WRK-TENSU   =
                                               RECE84-SYUTEN1 -
                                               RECE84-SYUTEN2
                                   WHEN    "12"
                                   WHEN    "13"
                                   WHEN    "14"
                                       COMPUTE WRK-TENSU   =
                                           RECE84-SYUTEN1  -
                                           TBL-KOUI-SRYKBN12-TEN-G
                                                           -
                                           TBL-KOUI-SRYKBN12-CHU-TEN-G
                                   WHEN    OTHER  
                                       MOVE    RECE84-SYUTEN1
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
                       AND   ( RECE84-ZAI-KINGAKU  >   ZERO           )
                           MOVE    RECE84-ZAI-KINGAKU  TO  WRK-NUM
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF    
                       PERFORM 5005-NUM-HENSYU-SEC  
      *                回数
                       MOVE    RECE84-ZAIKAISU    TO  WRK-KAISU
                       MOVE    WRK-KAISU           TO  WRK-NUM
                       MOVE    3                   TO  WRK-NAGASA
                       PERFORM 5005-NUM-HENSYU-SEC
      *                コメントコード１
                       IF      TBL-RECE855-MAX >   ZERO
                           MOVE    TBL-RECE855-SRYCD (1)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ１
                       IF      TBL-RECE855-MAX >   ZERO
                           MOVE    TBL-RECE855-NAME  (1)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード２
                       IF      TBL-RECE855-MAX >   1
                           MOVE    TBL-RECE855-SRYCD (2)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ２
                       IF      TBL-RECE855-MAX >   1
                           MOVE    TBL-RECE855-NAME  (2)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                コメントコード３
                       IF      TBL-RECE855-MAX >   2
                           MOVE    TBL-RECE855-SRYCD (3)
                                                       TO  WRK-DATA
                           MOVE    9                   TO  WRK-NAGASA
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                文字データ３
                       IF      TBL-RECE855-MAX >   2
                           MOVE    TBL-RECE855-NAME  (3)
                                                       TO  WRK-DATA
                           MOVE    100                 TO  WRK-NAGASA
                           MOVE    1                   TO  WRK-MOJI-CHK
                       END-IF
                       PERFORM 5004-MOJI-HENSYU-SEC  
      *                日付情報
                       PERFORM VARYING IDV     FROM    1   BY  1
                               UNTIL   IDV     >       31
                           IF      RECE84-KAISU (1 IDV)  >   ZERO
                               MOVE    RECE84-KAISU (1 IDV)
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
               PERFORM 200725-SRYKBN12-JIKANGAIHEN-SEC
           END-IF
      *
      *    コメントデータの編集（診療区分１１〜１４）
           IF    ( RECE84-SRYKBN       =   "11"    OR  "12"    OR
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
      *    特定器材データの編集
           IF      RECE84-ZAIREQFLG       =   SPACE
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
               MOVE    RECE82-HOSPNUM      TO  ORCSRECESORT-HOSPNUM
               MOVE    RECE82-NYUGAIKBN    TO  ORCSRECESORT-NYUGAIKBN
               MOVE    RECE82-PTID         TO  ORCSRECESORT-PTID
               MOVE    RECE82-SRYYM        TO  ORCSRECESORT-SRYYM
               MOVE    RECE84-SRYKBN       TO  ORCSRECESORT-SRYKBN
               MOVE    RECE84-ZAINUM       TO  ORCSRECESORT-ZAINUM
               MOVE    RECE84-KIZAITEN     TO  ORCSRECESORT-KIZAITEN
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
      *    摘要データ（医薬品）編集処理
      *****************************************************************
       200722-YKZ-HENSYU-SEC         SECTION.
      *
      *    関連コメント退避処理
           INITIALIZE              TBL-RECE855-COMMENT-AREA
           IF      RECE84-SETCOMFLG   =   "1"
               INITIALIZE               WRK-RECE855-REC
               MOVE    TBL-ZAIW-KEY (1 IDX)
                                           TO  WRK-RECE855-KEY1
               MOVE    TBL-ZAIW-SHORI-RENNUM (1 IDX)
                                           TO  WRK-RECE855-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE855-SEQNO
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
               CONTINUE
           ELSE
               IF  ( TBL-ZAIW-SRYSURYO (1 IDX)  >  ZERO )
               OR  ((RECE84-JIHOKBN  NOT =  SPACE) AND
                    (RECE84-ZAIKBN       =   4   )      )
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
                   MOVE    RECE84-ZAITEN      TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               ELSE
                   IF      RECE84-YKZTEN          >   ZERO
                   OR      RECE84-TOYAKUFLG   NOT =   SPACE
                   OR      RECE84-YKZKISAIFLG     =   "1"
                       MOVE    RECE84-YKZTEN      TO  WRK-NUM
                       MOVE    7                   TO  WRK-NAGASA
                   END-IF
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
           MOVE    RECE84-ZAIKAISU    TO  WRK-KAISU
           MOVE    WRK-KAISU           TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    コメントコード１
           IF      TBL-RECE855-MAX >   ZERO
               MOVE    TBL-RECE855-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE855-MAX >   ZERO
               MOVE    TBL-RECE855-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE855-MAX >   1
               MOVE    TBL-RECE855-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE855-MAX >   1
               MOVE    TBL-RECE855-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE855-MAX >   2
               MOVE    TBL-RECE855-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE855-MAX >   2
               MOVE    TBL-RECE855-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE84-KAISU (1 IDV)   >   ZERO
                   MOVE    RECE84-KAISU (1 IDV)   TO  WRK-NUM
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
           INITIALIZE              TBL-RECE855-COMMENT-AREA
           IF      RECE84-SETCOMFLG    =   "1"
               INITIALIZE               WRK-RECE855-REC
               MOVE    TBL-ZAIW-KEY (2 IDX)
                                           TO  WRK-RECE855-KEY1
               MOVE    TBL-ZAIW-SHORI-RENNUM (2 IDX)
                                           TO  WRK-RECE855-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE855-SEQNO
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
           AND   ( RECE84-SRYKBN      =   "70"            )
               MOVE    RECE84-SYUTEN2     TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF
           IF    ( IDXX                =   2               )
           AND   ( IDX                 =   WRK-SRYCD72-MAX )
               MOVE    RECE84-KIZAITEN    TO  WRK-TENSU
               IF      WRK-TENSU       >   ZERO
                   MOVE    WRK-TENSU           TO  WRK-NUM
                   MOVE    7                   TO  WRK-NAGASA
               END-IF
           END-IF        
           PERFORM 5005-NUM-HENSYU-SEC  
      *    回数
           MOVE    RECE84-ZAIKAISU    TO  WRK-NUM
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
           IF      TBL-RECE855-MAX >   ZERO
               MOVE    TBL-RECE855-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE855-MAX >   ZERO
               MOVE    TBL-RECE855-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE855-MAX >   1
               MOVE    TBL-RECE855-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE855-MAX >   1
               MOVE    TBL-RECE855-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE855-MAX >   2
               MOVE    TBL-RECE855-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE855-MAX >   2
               MOVE    TBL-RECE855-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE84-KAISU (1 IDV)  >    ZERO
                   MOVE    RECE84-KAISU (1 IDV)   TO  WRK-NUM
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
      *****************************************************************
      *    摘要データ（再編集特定機材）編集処理
      *****************************************************************
       200723-KIZAI-SORT-HENSYU-SEC    SECTION.
      *
      *    関連コメント退避処理
           INITIALIZE              TBL-RECE855-COMMENT-AREA
           IF      RECE84-SETCOMFLG    =   "1"
               INITIALIZE               WRK-RECE855-REC
               MOVE    ORCSRECESORT-KEY-HOSPNUM  (IDXX IDX)
                                           TO  WRK-RECE855-HOSPNUM
               MOVE    ORCSRECESORT-KEY-SRYYM    (IDXX IDX)
                                           TO  WRK-RECE855-SRYYM
               MOVE    ORCSRECESORT-KEY-NYUGAIKBN(IDXX IDX)
                                           TO  WRK-RECE855-NYUGAIKBN
               MOVE    ORCSRECESORT-KEY-PTID     (IDXX IDX)
                                           TO  WRK-RECE855-PTID
               MOVE    ORCSRECESORT-KEY-RECEKA   (IDXX IDX)
                                           TO  WRK-RECE855-RECEKA
               MOVE    ORCSRECESORT-KEY-KOHID    (IDXX IDX)
                                           TO  WRK-RECE855-HKNID
               MOVE    ORCSRECESORT-KEY-TEKSTYMD (IDXX IDX)
                                           TO  WRK-RECE855-TEKSTYMD
               MOVE    ORCSRECESORT-KEY-ZAINUM   (IDXX IDX)
                                           TO  WRK-RECE855-ZAINUM
               MOVE    ORCSRECESORT-KEY-RENNUM   (IDXX IDX)
                                           TO  WRK-RECE855-RENNUM
               MOVE    ORCSRECESORT-SHORI-RENNUM (IDXX IDX)
                                           TO  WRK-RECE855-SHORI-RENNUM
               MOVE    ZERO                TO  WRK-RECE855-SEQNO
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
           MOVE    RECE84-ZAIKAISU    TO  WRK-NUM
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
           IF      TBL-RECE855-MAX     >   ZERO
               MOVE    TBL-RECE855-SRYCD (1)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ１
           IF      TBL-RECE855-MAX     >   ZERO
               MOVE    TBL-RECE855-NAME (1)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード２
           IF      TBL-RECE855-MAX     >   1
               MOVE    TBL-RECE855-SRYCD (2)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ２
           IF      TBL-RECE855-MAX     >   1
               MOVE    TBL-RECE855-NAME  (2)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    コメントコード３
           IF      TBL-RECE855-MAX     >   2
               MOVE    TBL-RECE855-SRYCD (3)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    文字データ３
           IF      TBL-RECE855-MAX     >   2
               MOVE    TBL-RECE855-NAME  (3)
                                           TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               MOVE    1                   TO  WRK-MOJI-CHK
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    日付情報
           PERFORM VARYING IDV     FROM    1   BY  1
                   UNTIL   IDV     >       31
               IF      RECE84-KAISU (1 IDV)   >   ZERO
                   MOVE    RECE84-KAISU (1 IDV)    TO  WRK-NUM
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
      *    加算項目扱いの基本項目の編集処理
      *****************************************************************
       200725-SRYKBN12-JIKANGAIHEN-SEC   SECTION.
      *
      *    診療行為データの編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       TBL-KOUI-SRYKBN12-MAX
               IF      TBL-KOUI-SRYKBN12-KAISU (IDX)   >   ZERO
      *            関連コメント退避処理
                   INITIALIZE              TBL-RECE855-COMMENT-AREA
                   IF      RECE84-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE855-REC
                       MOVE    TBL-KOUI-SRYKBN12-KEY (IDX)
                                                TO  WRK-RECE855-KEY1
                       MOVE    TBL-KOUI-SRYKBN12-SHORI-RENNUM (IDX)
                                                TO
                                                WRK-RECE855-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE855-SEQNO
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
                   IF      TBL-RECE855-MAX >   ZERO
                       MOVE    TBL-RECE855-SRYCD (1)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE855-MAX >   ZERO
                       MOVE    TBL-RECE855-NAME  (1)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE855-MAX >   1
                       MOVE    TBL-RECE855-SRYCD (2)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE855-MAX >   1
                       MOVE    TBL-RECE855-NAME  (2)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE855-MAX >   2
                       MOVE    TBL-RECE855-SRYCD (3)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE855-MAX >   2
                       MOVE    TBL-RECE855-NAME  (3)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE84-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE84-KAISU (1 IDV)
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
       200725-SRYKBN12-JIKANGAIHEN-EXT.
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
                   INITIALIZE              TBL-RECE855-COMMENT-AREA
                   IF      RECE84-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE855-REC
                       MOVE    TBL-KOUI-SRYKBN12-CHU-KEY (IDY4)
                                                TO  WRK-RECE855-KEY1
                       MOVE    TBL-KOUI-SRYKBN12-CHU-SHORI-NUM    (IDY4)
                                                TO
                                                WRK-RECE855-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE855-SEQNO
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
                   IF      TBL-RECE855-MAX >   ZERO
                       MOVE    TBL-RECE855-SRYCD (1)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE855-MAX >   ZERO
                       MOVE    TBL-RECE855-NAME  (1)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE855-MAX >   1
                       MOVE    TBL-RECE855-SRYCD (2)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE855-MAX >   1
                       MOVE    TBL-RECE855-NAME  (2)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE855-MAX >   2
                       MOVE    TBL-RECE855-SRYCD (3)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE855-MAX >   2
                       MOVE    TBL-RECE855-NAME  (3)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE84-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE84-KAISU (1 IDV)
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
      *    入院基本料・特定入院料・食事療養編集処理
      *****************************************************************
       20077-NYUIN-HENSYU-SEC           SECTION.
      *
      *    入院料
           PERFORM VARYING IDZ1    FROM    1   BY  1
                   UNTIL   IDZ1    >       3
               PERFORM VARYING IDZ2    FROM    1   BY  1
                       UNTIL   IDZ2    >       31
                   MOVE    ZERO                    TO  FLG-TOYAKUFLG
      *
                   PERFORM VARYING IDXZ    FROM    1   BY  1
                           UNTIL   IDXZ    >  
                                   LNK-RNYURECEDEN-MEISAISU (IDZ1 IDZ2)
                       MOVE    SPACE               TO  WRK-REC
                       MOVE    ZERO                TO  WRK-REC-LENGTH
      *
                       EVALUATE    LNK-RNYURECEDEN-RECSKB
                                                   (IDZ1 IDZ2 IDXZ)
                           WHEN    "CO"
                               MOVE    "CO,"           TO  WRK-REC
                               MOVE    3               TO
                                                       WRK-REC-LENGTH
      *                        診療識別
                               MOVE    LNK-RNYURECEDEN-SRYSKB
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-DATA
                               MOVE    2               TO  WRK-NAGASA
                               PERFORM 5004-MOJI-HENSYU-SEC  
      *                        予備１
                               PERFORM 5004-MOJI-HENSYU-SEC  
      *                        コメントコード
                               MOVE    LNK-RNYURECEDEN-SRYCD
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-DATA
                               MOVE    9               TO  WRK-NAGASA
                               PERFORM 5004-MOJI-HENSYU-SEC  
      *                        文字データ
                               MOVE    LNK-RNYURECEDEN-COMMENT
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-DATA
                               MOVE    76              TO  WRK-NAGASA
                               MOVE    1               TO  WRK-MOJI-CHK
                               MOVE    1               TO  WRK-END 
                               PERFORM 5004-MOJI-HENSYU-SEC  
                           WHEN    OTHER
                               MOVE    "RI,"           TO  WRK-REC
                               MOVE    3               TO
                                                       WRK-REC-LENGTH
      *                        診療識別
                               MOVE    LNK-RNYURECEDEN-SRYSKB
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-DATA
                               MOVE    2               TO  WRK-NAGASA
                               PERFORM 5004-MOJI-HENSYU-SEC  
      *                        診療行為コード
                               MOVE    LNK-RNYURECEDEN-SRYCD
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-DATA
                               MOVE    9               TO  WRK-NAGASA
                               PERFORM 5004-MOJI-HENSYU-SEC  
      *                        数量
                               IF    ( LNK-RNYURECEDEN-SURYO
                                                    (IDZ1 IDZ2 IDXZ)
                                                       >   ZERO    )
                                   MOVE    LNK-RNYURECEDEN-SURYO
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-SURYO
                                   MOVE    WRK-SURYO   TO  WRK-NUM
                                   MOVE    8           TO  WRK-NAGASA
                               END-IF
                               PERFORM 5005-NUM-HENSYU-SEC
      *                        点数
                               IF    ( IDZ1            <   3     )
                               AND   ( LNK-RNYURECEDEN-TEN
                                                    (IDZ1 IDZ2 IDXZ)
                                                        >   ZERO )
                                   MOVE    LNK-RNYURECEDEN-TEN
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-TENSU
                                   MOVE    WRK-TENSU   TO  WRK-NUM
                                   MOVE    7           TO  WRK-NAGASA
                               END-IF
                               PERFORM 5005-NUM-HENSYU-SEC  
      *                        金額
                               IF    ( IDZ1            =   3     )
                               AND   ( LNK-RNYURECEDEN-TEN
                                                    (IDZ1 IDZ2 IDXZ)
                                                        >   ZERO )
                                   MOVE    LNK-RNYURECEDEN-TEN
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-TENSU
                                   MOVE    WRK-TENSU   TO  WRK-NUM
                                   MOVE    9           TO  WRK-NAGASA
                               END-IF
                               PERFORM 5005-NUM-HENSYU-SEC  
      *                        回数
                               MOVE    LNK-RNYURECEDEN-KAISU
                                                       (IDZ1 IDZ2 IDXZ)
                                                       TO  WRK-KAISU
                               MOVE    WRK-KAISU       TO  WRK-NUM
                               MOVE    3               TO  WRK-NAGASA
                               PERFORM 5005-NUM-HENSYU-SEC
      *                        コメントコード１
                               PERFORM 5005-NUM-HENSYU-SEC
      *                        文字データ１
                               PERFORM 5004-MOJI-HENSYU-SEC  
      *                        コメントコード２
                               PERFORM 5005-NUM-HENSYU-SEC
      *                        文字データ２
                               PERFORM 5004-MOJI-HENSYU-SEC  
      *                        コメントコード３
                               PERFORM 5005-NUM-HENSYU-SEC
      *                        文字データ３
                               PERFORM 5004-MOJI-HENSYU-SEC  
      *                        日付情報
                               PERFORM VARYING IDV FROM    1   BY  1
                                       UNTIL   IDV >       31
                                   IF      LNK-RNYURECEDEN-DAY
                                                       (IDZ1 IDZ2 IDV)
                                                       >   ZERO
                                       MOVE    LNK-RNYURECEDEN-DAY
                                                       (IDZ1 IDZ2 IDV)
                                                       TO  WRK-NUM
                                       MOVE    3       TO  WRK-NAGASA
                                   END-IF
                                   IF      IDV         =   31
                                       MOVE    1       TO  WRK-END 
                                   END-IF     
                                   PERFORM 5005-NUM-HENSYU-SEC
                               END-PERFORM  
                       END-EVALUATE
                       IF      WRK-REC-LENGTH      >   ZERO 
                           PERFORM 5001-FILE-WRITE-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
      *        差し込みコメント編集
               PERFORM 200771-NYUIN-COMMENT-HENSYU-SEC
           END-PERFORM
      *
      *    摘要欄下部コメント編集
           MOVE    9                   TO  IDZ1 
           PERFORM 200771-NYUIN-COMMENT-HENSYU-SEC
           .
       20077-NYUIN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *        差し込みコメント編集処理
      *****************************************************************
       200771-NYUIN-COMMENT-HENSYU-SEC      SECTION.
      *
           PERFORM     UNTIL  ( FLG-RECE84           =   1           )
                       OR     ( RECE82-KEY       NOT =   RECE84-KEY1 )
               EVALUATE    IDZ1
                   WHEN    1 
                       IF      RECE84-SRYKBN       >  "90"
                           GO  TO  200771-NYUIN-COMMENT-HENSYU-EXT
                       END-IF
                   WHEN    2 
                       IF      RECE84-SRYKBN       >  "92"
                           GO  TO  200771-NYUIN-COMMENT-HENSYU-EXT
                       END-IF
                   WHEN    3 
                       IF      RECE84-SRYKBN       >  "97"
                           GO  TO  200771-NYUIN-COMMENT-HENSYU-EXT
                       END-IF
               END-EVALUATE
      *
               IF    ( IDZ1                =   9    ) 
               AND   (  RECE84-SRYKBN      <   "99" )
                   PERFORM 990-RECE84-NEXT-SEC
               ELSE
      *            明細テーブル退避処理
                   PERFORM 20071-ZAI-TAIHI-SEC
      *
      *            仮レセデータフラグ設定
                   IF      RECE84-TOYAKUFLG  =  SPACE
                       MOVE    ZERO     TO  FLG-TOYAKUFLG
                   ELSE
                       MOVE    1        TO  FLG-TOYAKUFLG
                   END-IF
      *
      *            摘要データ編集処理
                   IF      FLG-END           =   ZERO
                       PERFORM 20072-ZAI-HENSYU-SEC
      * 
                       PERFORM 990-RECE84-NEXT-SEC
                   END-IF
               END-IF
           END-PERFORM
           .
       200771-NYUIN-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検査に判断料がある場合の検査の編集処理
      *****************************************************************
       20078-KENSA-HANDANRYO-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDZ3 FROM    1   BY  1
                   UNTIL   IDZ3 >   TBL-KENSA-RECE84-MAX
               IF      TBL-KENSA4-ZAIKAISU (IDZ3)
                                           >   0
                   MOVE    TBL-KENSA-RECE84-REC (IDZ3)
                                               TO  RECE84-REC
                   MOVE    "60"                TO  WRK-SRYKBN
      *
      *            仮レセデータフラグ設定
                   IF      RECE84-TOYAKUFLG =   SPACE
                       MOVE    ZERO              TO  FLG-TOYAKUFLG
                   ELSE
                       MOVE    1                 TO  FLG-TOYAKUFLG
                   END-IF 
      *
      *            検査に判断料がある場合の検査の編集処理
                   PERFORM VARYING IDZ4    FROM    1   BY  1
                           UNTIL   IDZ4    >       50
                           OR      TBL-KENSA-RECE85-REC  (IDZ3 IDZ4)
                                                   =   SPACE
                       MOVE    TBL-KENSA-SRYCD-MAX (IDZ3) 
                                           TO  IDZ5
                       INITIALIZE      WRK-RECE85-REC
                       MOVE    TBL-KENSA-RECE85-KEY        (IDZ3 IDZ4)
                                           TO  WRK-RECE85-KEY
                       MOVE    TBL-KENSA-RECE85-SRYACTINF  (IDZ3 IDZ4)
                                           TO  WRK-RECE85-SRYACTINF (1)
                       MOVE    TBL-KENSA-RECE85-NAMEINF    (IDZ3 IDZ4)
                                           TO  WRK-RECE85-NAMEINF   (1)
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
           INITIALIZE                  TBL-RECE855-COMMENT-AREA
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
           EVALUATE    WRK-RECE85-SRYCD (1) (1:1)
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
                   MOVE    WRK-RECE85-SRYCD (1)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ
                   MOVE    WRK-RECE85-NAME (1) TO  WRK-DATA
                   MOVE    76                  TO  WRK-NAGASA
                   MOVE    1                   TO  WRK-MOJI-CHK
                   MOVE    1                   TO  WRK-END 
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *
                   IF      WRK-RECE85-NAME (1) (77:)
                                           NOT =   SPACE
                       PERFORM 5001-FILE-WRITE-SEC
      *
                       MOVE    WRK-RECE85-SRYCD (1)
                                                   TO  WRK-CO-SRYCD
                       MOVE    WRK-RECE85-NAME  (1) (77:)
                                                   TO  WRK-CO-COMMENT
                       PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
                   END-IF
               WHEN    OTHER     
      *            関連コメント退避処理
                   IF      RECE84-SETCOMFLG   =   "1"
                       INITIALIZE               WRK-RECE855-REC
                       MOVE    TBL-KENSA-RECE85-KEY (IDZ3 IDZ4)
                                            TO  WRK-RECE855-KEY1
                       MOVE    TBL-KENSA-RECE85-SHORI-RENNUM
                                                    (IDZ3 IDZ4)
                                            TO  WRK-RECE855-SHORI-RENNUM
                       MOVE    ZERO         TO  WRK-RECE855-SEQNO
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
                   MOVE    WRK-RECE85-SRYCD (1)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            数量
                   IF      WRK-RECE85-SRYSURYO (1)
                                           >   ZERO
                       IF    ( WRK-RECE85-SRYCD (1) (1:1)
                                               =   "1"        )
                       AND   ( WRK-RECE85-KZMCOMPSIKIBETU (1)
                                           NOT =   "1"        )
                           CONTINUE
                       ELSE
                           MOVE    WRK-RECE85-SRYSURYO (1)
                                               TO  WRK-SURYO
                           MOVE    WRK-SURYO   TO  WRK-NUM
                           MOVE    8           TO  WRK-NAGASA
                       END-IF
                   END-IF
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            点数
                   IF    ( IDZ4            =   IDZ5 )
                   AND   ( RECE84-SYUTEN1  >   ZERO )
                       MOVE    RECE84-SYUTEN1      TO  WRK-TENSU
                       MOVE    WRK-TENSU           TO  WRK-NUM
                       MOVE    7                   TO  WRK-NAGASA
                   END-IF    
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            金額
                   PERFORM 5005-NUM-HENSYU-SEC  
      *            回数
                   MOVE    RECE84-ZAIKAISU     TO  WRK-KAISU
                   MOVE    WRK-KAISU           TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC
      *            コメントコード１
                   IF      TBL-RECE855-MAX >   ZERO
                       MOVE    TBL-RECE855-SRYCD (1)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ１
                   IF      TBL-RECE855-MAX >   ZERO
                       MOVE    TBL-RECE855-NAME  (1)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード２
                   IF      TBL-RECE855-MAX >   1
                       MOVE    TBL-RECE855-SRYCD (2)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ２
                   IF      TBL-RECE855-MAX >   1
                       MOVE    TBL-RECE855-NAME  (2)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            コメントコード３
                   IF      TBL-RECE855-MAX >   2
                       MOVE    TBL-RECE855-SRYCD (3)
                                                   TO  WRK-DATA
                       MOVE    9                   TO  WRK-NAGASA
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            文字データ３
                   IF      TBL-RECE855-MAX >   2
                       MOVE    TBL-RECE855-NAME  (3)
                                                   TO  WRK-DATA
                       MOVE    100                 TO  WRK-NAGASA
                       MOVE    1                   TO  WRK-MOJI-CHK
                   END-IF
                   PERFORM 5004-MOJI-HENSYU-SEC  
      *            日付情報
                   PERFORM VARYING IDV     FROM    1   BY  1
                           UNTIL   IDV     >       31
                       IF      RECE84-KAISU (1 IDV)   >   ZERO
                           MOVE    RECE84-KAISU (1 IDV)
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
      *    関連コメント退避処理
      *****************************************************************
       200799-KANREN-COM-TAIHI-SEC     SECTION.
      *
           INITIALIZE                  TBL-RECE855-COMMENT-AREA
      *
           MOVE    WRK-RECE855-REC     TO  RECE855-REC
           START   RECE855-FILE
               KEY IS    >=   RECE855-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE855
               NOT INVALID
                   PERFORM 900-RECE855-NEXT-SEC
           END-START
           PERFORM UNTIL  ( FLG-RECE855    =   1                    )
                   OR     ( WRK-RECE855-KEY1 
                                       NOT =   RECE855-KEY1         )
                   OR     ( WRK-RECE855-SHORI-RENNUM
                                       NOT =   RECE855-SHORI-RENNUM )
               PERFORM 2007991-KANREN-COM-TAIHIHEN-SEC
      *
               PERFORM 900-RECE855-NEXT-SEC
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
           IF      RECE855-SRYCD (1:1) =   "0"
               IF    ( RECE855-SRYCD (1:3) =   "002"             )
               OR    ( RECE855-SRYCD (1:3) =   "008"             )
               OR    ( RECE855-SRYCD       =   WRK-CONS-COMMENT1 ) 
               OR    ( RECE855-SRYCD       =   WRK-CONS-COMMENT2 ) 
      *****    OR    ( RECE855-SRYCD       =   WRK-CONS-COMMENT3 ) 
               OR    ( RECE855-SRYCD       =   WRK-CONS-COMMENT4 )
                   CONTINUE
               ELSE
                   GO  TO  2007991-KANREN-COM-TAIHIHEN-EXT
               END-IF     
           END-IF
      *     
           ADD     1                TO  TBL-RECE855-MAX
           MOVE    RECE855-SRYCD    TO  TBL-RECE855-SRYCD
                                               (TBL-RECE855-MAX)         
           MOVE    RECE855-NAME     TO  TBL-RECE855-NAME
                                               (TBL-RECE855-MAX)         
      *
           IF      RECE855-SRYCD    =   "810000001"
               CONTINUE
           ELSE
      *        部位／薬剤／ユーザーコメント
               IF      RECE855-SRYCD (1:1) =   "0"
                   MOVE    "810000001"     TO  
                                           TBL-RECE855-SRYCD
                                               (TBL-RECE855-MAX)   
               END-IF
      *        コメント
               IF    ( RECE855-SRYCD (1:1) =   "8"   )
               OR    ( RECE855-SRYCD (1:3) =   "008"  AND
                       RECE855-SRYCD (4:1)
                                       NOT =   "2"   )
                   MOVE    RECE855-INPUTCOMENT TO
                                           TBL-RECE855-NAME
                                               (TBL-RECE855-MAX)
               END-IF
           END-IF
      *    コメントパターン２０のコメントコード
           IF      RECE855-SRYCD (1:2) =   "82"
               MOVE    SPACE              TO
                                           TBL-RECE855-NAME
                                               (TBL-RECE855-MAX)
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
           IF      TBL-RECE855-MAX     <=  3
               GO  TO  200798-KANREN-COM-HEN-EXT
           END-IF
      *
           PERFORM VARYING IDVV    FROM    4   BY  1
                   UNTIL   IDVV    >       TBL-RECE855-MAX
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
               MOVE    TBL-RECE855-SRYCD (IDVV)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        文字データ
               MOVE    TBL-RECE855-NAME (IDVV)
                                           TO  WRK-DATA
               MOVE    76                  TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END 
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               IF      TBL-RECE855-NAME (IDVV) (77:)
                                       NOT =   SPACE
                   PERFORM 5001-FILE-WRITE-SEC
      *
                   MOVE    TBL-RECE855-SRYCD (IDVV)
                                               TO  WRK-CO-SRYCD
                   MOVE    TBL-RECE855-NAME (IDVV) (77:)
                                               TO  WRK-CO-COMMENT
                   PERFORM 5010-CO-RECORD-NEXT-HENSYU-SEC
               END-IF
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-PERFORM
      *
           INITIALIZE                  TBL-RECE855-COMMENT-AREA
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
           MOVE    RECE82-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE82-PTID         TO  OPECOM-PTID
           MOVE    RECE82-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE82-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key4"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM       =   RECE82-HKNNUM )
               AND   ( OPECOM-HKNID        =   RECE82-HKNID  )
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
           MOVE    RECE82-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE82-PTID         TO  OPECOM-PTID
           MOVE    RECE82-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE82-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key5"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM       =   RECE82-HKNNUM )
               AND   ( OPECOM-HKNID        =   RECE82-HKNID  )
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
      *    症状詳記コメント（５０、５１）       
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key12"             TO  WRK-MCP-PATHNAME
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
           MOVE    RECE82-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE82-PTID         TO  RECECOM-PTID
           MOVE    RECE82-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
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
               IF      RECECOM-HKNCOMBI    =   RECE82-HKNCOMBI
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
           CLOSE   RECE82-FILE
                   RECE83-FILE
                   RECE84-FILE
                   RECE85-FILE
                   RECE855-FILE
                   RECE88-FILE
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
           DISPLAY "RECE82  CNT-IN  :" CNT-IN  
           DISPLAY "RECE84  CNT-IN  :" CNT-RECE84  
           DISPLAY "DB      CNT-OUT :" CNT-OUT  
           DISPLAY "*** ORCR0985 END ***"
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
      *
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
           STRING  "ORCR0985 "         DELIMITED  BY  SIZE
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
           STRING  "ORCR0985 "             DELIMITED   BY  SIZE
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
           MOVE    RECE82-HOSPNUM      TO  RECEDEN-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECEDEN-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECEDEN-PTID
           MOVE    ZERO                TO  RECEDEN-RECEKA
           MOVE    3                   TO  RECEDEN-TEISYUTUSAKI
           MOVE    RECE82-RECESYUBETU  TO  RECEDEN-RECESYUBETU
           MOVE    ZERO                TO  RECEDEN-HKNJANUM
           MOVE    ZERO                TO  RECEDEN-HOJOKBN-KEY
           MOVE    RECE82-HKNID        TO  RECEDEN-KOHID-KEY
           MOVE    RECE82-TEKSTYMD     TO  RECEDEN-TEKSTYMD
           MOVE    RECE82-TEKSTYMD     TO  RECEDEN-TEKSTYMD
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
      *
      *****************************************************************
      *    レセプトコメントマスタＲＥＡＤ
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
       900-RECE82-READ-SEC             SECTION.
      *
           READ    RECE82-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                NOT AT  END
                   IF      STS-RECE82          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE82PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE82      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
      *            公務災害、種別不明のとき対象外
      *            指定医療機関ではないとき対象外
      *            ０９５９３で始まるコードがあるとき対象外
                   IF  (   RECE82-SRYYM        <   201307 )
                   OR  (   RECE82-PTNUM        =   SPACE  )
                   OR  (   RECE82-TSTPTNUMKBN  =   "1"    )
                   OR  (   RECE82-RECESYUBETU  =   ZERO
                                               OR  7061   )
                   OR  (   RECE82-ERR-ERRKBN   >   ZERO   )
                   OR  (   RECE82-RSIIRYOKBN   =   "0"    )
                   OR  ((  RECE82-HKNKBN    = "1" OR "2" ) AND
                        (  RECE82-TOTALKIN        = ZERO ) AND
                        (  RECE82-KINGAKU2-SYO    = ZERO ) AND
                        (  RECE82-SHOKUJITOTALKIN = ZERO ))
                   OR  (   RECE82-ORIGINALCDKBN
                                               =   1      )
                       GO  TO  900-RECE82-READ-SEC
                   ELSE
      *                医療機関情報取得
                       IF      WRK-PARA-SRYYM  =   ZERO
                           MOVE  RECE82-SRYYM      TO
                                                   WRK-SYS-SRYYMD (1:6)
                           MOVE  "01"              TO
                                                   WRK-SYS-SRYYMD (7:2)
                           PERFORM 900-SYSKANRI-HENSYU-SEC
                       END-IF
       *
                       ADD     1               TO  CNT-IN
                       DISPLAY "RECE82  PTNUM=" RECE82-PTNUM     
                               " SRYYM="        RECE82-SRYYM 
                               " RECESYUBETU="  RECE82-RECESYUBETU
                   END-IF
           END-READ
           .
       900-RECE82-READ-EXT.
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
      *
      ****************************************************************
      *    傷病名読み込み
      ****************************************************************
       990-RECE83-NEXT-SEC           SECTION.
      *
           READ    RECE83-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE83
               NOT AT  END
                   IF      STS-RECE83          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE83PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE83          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE83
           END-READ
      *
           .
       990-RECE83-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    摘要情報読み込み
      ****************************************************************
       990-RECE84-NEXT-SEC           SECTION.
      *
           READ    RECE84-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE84
               NOT AT  END
                   IF      STS-RECE84          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE84PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE84          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE84
                   ADD     1         TO  CNT-RECE84
           END-READ
      *
           .
       990-RECE84-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE85-NEXT-SEC           SECTION.
      *
           READ    RECE85-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE85
               NOT AT  END
                   IF      STS-RECE85          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE85PARA          TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE85          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE85
           END-READ
      *
           .
       990-RECE85-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    関連コメント読み込み
      ****************************************************************
       900-RECE855-NEXT-SEC           SECTION.
      *
           READ    RECE855-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE855
               NOT AT  END
                   IF      STS-RECE855         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE855PARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE855         TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE855
           END-READ
      *
           .
       900-RECE855-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    入退院履歴情報読み込み
      ****************************************************************
       900-RECE88-NEXT-SEC           SECTION.
      *
           READ    RECE88-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE88
               NOT AT  END
                   IF      STS-RECE88          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE88PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE88      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE88
           END-READ
      *
           .
       900-RECE88-NEXT-EXT.
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
               STRING  "ORCR0985 insert err TABLE="
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
               STRING  "ORCR0985 select err TABLE="
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
