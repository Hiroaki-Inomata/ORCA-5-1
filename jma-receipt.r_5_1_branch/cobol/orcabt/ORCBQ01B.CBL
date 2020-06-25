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
       PROGRAM-ID.             ORCBQ01B.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 患者一覧出力（診療行為）（ＯＲＣＢＱ０１Ｂ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/12/24    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付       内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PRT.
      *
      *    CSVファイル
           SELECT  CSV-FILE        ASSIGN  CSVNAME
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-CSV.
      *    PUSH用ファイル
      *    LINE SEQUENTIALにしない。最後の項目に改行が入るため
           SELECT  PUSHFOUND-FILE  ASSIGN  PUSHFOUNDNAME
                                   ORGANIZATION    IS  SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PUSH.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "CPPRTALL.INC".
      *
      *    CSVファイル
       FD  CSV-FILE.
       01  CSV-REC                     PIC X(2000).
      *
      *    PUSH用ファイル
       FD  PUSHFOUND-FILE.
       COPY    "CPPUSHFOUND.INC".
      *
       WORKING-STORAGE                 SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *
      *    シェル用領域
      *    COPY    "CPCOMMONSHELL.INC".
           COPY    "CPSHELLTBL.INC".
      *
      *    ワークパラメータファイル
           COPY    "CPBQ01.INC"
                                   REPLACING  //BQ01//
                                   BY         //WKBQ01//.
      *
           COPY    "CPASGNPARA.INC".
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //CSVNAME//.
      *
           COPY    "CPASGNPARA.INC"
                                   REPLACING  //ASGNPARA//
                                   BY         //PUSHFOUNDNAME//.
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PRT                 PIC X(02).
           03  STS-CSV                 PIC X(02).
           03  STS-PUSH                PIC X(02).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX1                    PIC 9(09).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDX-LEN                 PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-CSV-EDIT            PIC 9(01).
           03  FLG-PRT-EDIT            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE                PIC 9(09).
           03  CNT-LINE                PIC 9(05).
           03  CNT-PRT-RENNUM          PIC 9(05).
           03  CNT-CSV-RENNUM          PIC 9(05).
           03  CNT-KNS                 PIC 9(05).
           03  CNT-CSV                 PIC 9(09).
      *
      *    一時領域
       01  WRK-AREA. 
           03  WRK-STR                 PIC X(20000).
           03  WRK-STR-LEN             PIC 9(05).
           03  WRK-STR-PNT             PIC 9(05).
           03  WRK-MAE-INPUT           PIC X(400).
           03  WRK-OUT-INPUT           PIC X(400).
           03  WRK-OUT-MAX             PIC 9(05).
           03  WRK-PTINF-ADR-LEN       PIC 9(05).
           03  WRK-PTINF-BANTI-LEN     PIC 9(05).
           03  WRK-ADRS                PIC X(400).
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-ID         PIC X(36).
               05  WRK-PARA-TERMID     PIC X(64).
               05  WRK-PARA-CSVFILENAME  PIC X(100).
               05  WRK-PARA-PUSHFILENAME PIC X(100).
               05  WRK-PARA-TBL-KEY    PIC X(8).
               05  WRK-PARA-TBL-GROUP  PIC X(14).
           03  WRK-PROC-KNS            PIC 9(09).
           03  WRK-ZZZZ9               PIC ZZZZ9.
           03  WRK-ZZZZ9C              PIC ZZ,ZZ9.
           03  WRK-ZZZZZZZ9            PIC ZZZZZZZ9.
           03  WRK-ZZZZZZZ9C           PIC ZZ,ZZZ,ZZ9.
           03  WRK-NUM-X.
               05  WRK-NUM             PIC 9(10).
           03  WRK-NUM-Z               PIC Z(9)9.
           03  WRK-SYMD                PIC X(08).
           03  WRK-HENYMDG1            PIC X(09).
           03  WRK-HENYMDG3            PIC X(22).
           03  WRK-PRTNM               PIC X(20).
           03  WRK-CALC-YMD1.
               05  WRK-CALC-YY1                    PIC 9(04).
               05  WRK-CALC-MM1                    PIC 9(02).
               05  WRK-CALC-DD1                    PIC 9(02).
      *
           03  WRK-CALC-YMD2.
               05  WRK-CALC-YY2                    PIC 9(04).
               05  WRK-CALC-MM2                    PIC 9(02).
               05  WRK-CALC-DD2                    PIC 9(02).
           03  WRK-AGE-YY                          PIC S9(04).
           03  WRK-AGE-MM                          PIC S9(02).
           03  WRK-AGE-G.
               05  WRK-AGE-Z                       PIC ---9.
               05  WRK-AGE-MEI                     PIC X(04).
           03  WRK-RENNUM              PIC 9(03).
           03  WRK-ENCODING            PIC X(01).
           03  WRK-ENCODING9           REDEFINES WRK-ENCODING
                                       PIC 9(01).
           03  WRK-SRYSURYOX.
               05  WRK-SRYSURYOZ       PIC ZZZZ9.99999.
           03  WRK-ZAIKAISUZ           PIC ZZZZ9.
           03  WRK-SURYOTOTALX.
               05  WRK-SURYOTOTALZ     PIC ZZZZZZ9.99999.
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-DAY-G.
               05  WRK-DAY                             PIC 9(02).
           03  WRK-NISSU-DAY-G.
                   07  WRK-NISSU-DAY                   PIC 9(01)
                                                       OCCURS  31.
           03  WRK-NISSU-AREA.
               05  WRK-NISSU-KUGIRI-FLG                PIC 9(01).
               05  WRK-NISSU-RENZOKU-CNT               PIC 9(03).
               05  WRK-NISSU-ST-G.
                   07  WRK-NISSU-ST                    PIC 9(02).
                   07  WRK-NISSU-ST-X                  PIC X(02).
                   07  WRK-NISSU-ST-R1 REDEFINES   WRK-NISSU-ST-X.
                       09  WRK-NISSU-ST-Z9             PIC Z9.
                   07  WRK-NISSU-ST-R2 REDEFINES   WRK-NISSU-ST-X.
                       09  WRK-NISSU-ST-9              PIC 9(01).
                       09  WRK-NISSU-ST-FL             PIC X(01).
               05  WRK-NISSU-ED-G.
                   07  WRK-NISSU-ED                    PIC 9(02).
                   07  WRK-NISSU-ED-X                  PIC X(02).
                   07  WRK-NISSU-ED-R1 REDEFINES   WRK-NISSU-ED-X.
                       09  WRK-NISSU-ED-Z9             PIC Z9.
                   07  WRK-NISSU-ED-R2 REDEFINES   WRK-NISSU-ED-X.
                       09  WRK-NISSU-ED-9              PIC 9(01).
                       09  WRK-NISSU-ED-FL             PIC X(01).
               05  WRK-NISSU-MES                       PIC X(200).
               05  WRK-NISSU-PTR                       PIC 9(03).
           03  WRK-UUID                PIC X(36).
      *
       01  KEY-AREA.
           03  KEY-CSV-G.
               05  KEY-CSV-PTID        PIC 9(10).
               05  KEY-CSV-SRYCD       PIC X(09).
           03  KEY-PRT-G.
               05  KEY-PRT-PTID        PIC 9(10).
               05  KEY-PRT-SRYCD       PIC X(09).
      *
       01  CSV-EDIT-AREA.
           03  CSV-NO                  PIC Z(10).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-PTNUM               PIC X(20).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-NAME                PIC X(100).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-KANANAME            PIC X(100).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SEX                 PIC X(2).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-BIRTHDAY            PIC X(10).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-AGENUM              PIC X(04).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-AGETANI             PIC X(04).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYCD               PIC X(09).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYNAME             PIC X(80).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYKANANAME         PIC X(80).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYSURYO            PIC X(11).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-ZAIKAISU            PIC X(05).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SURYOTOTAL          PIC X(13).
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CSV-SRYYM               PIC X(07).
           03  CSV-SRYYMD-OCC          OCCURS  31.
               05  FILLER              PIC X(01)   VALUE ",".
               05  CSV-SRYYMD          PIC X(10).
      *
       01  CONST-AREA.
           03  CONST-FORMID            PIC X(20)   VALUE  "HCM28B.red".
           03  CONST-LINE              PIC 9(05)   VALUE   30.
           03  CONST-JOBID             PIC 9(07)   VALUE   1.
           03  CONST-JOB-SHELLID                   PIC X(08)
               VALUE   "ORCBQ01".
      *
       01  CONST-CSV-HEADER.
           03  CONST-CSV-NO            PIC X(20)   VALUE "番号".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-PTNUM         PIC X(20)   VALUE "患者番号".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-NAME          PIC X(20)   VALUE "氏名".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-KANANAME      PIC X(20)   VALUE "カナ氏名".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SEX           PIC X(20)   VALUE "性別".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-BIRTHDAY      PIC X(20)   VALUE "生年月日".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-AGENUM        PIC X(20)   VALUE "年齢".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-AGETANI       PIC X(20)   VALUE "年齢ラベル".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYCD         PIC X(20)   VALUE "診療コード".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYNAME       PIC X(20)   VALUE "診療名称".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYKANANAME   PIC X(20)   VALUE "診療カナ名称".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYSURYO      PIC X(20)   VALUE "数量".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-ZAIKAISU      PIC X(20)   VALUE "回数".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SURYOTOTAL    PIC X(20)   VALUE "合計".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYM         PIC X(20)   VALUE "診療年月".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD01      PIC X(20)   VALUE "算定日１".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD02      PIC X(20)   VALUE "算定日２".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD03      PIC X(20)   VALUE "算定日３".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD04      PIC X(20)   VALUE "算定日４".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD05      PIC X(20)   VALUE "算定日５".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD06      PIC X(20)   VALUE "算定日６".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD07      PIC X(20)   VALUE "算定日７".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD08      PIC X(20)   VALUE "算定日８".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD09      PIC X(20)   VALUE "算定日９".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD10      PIC X(20)   VALUE "算定日１０".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD11      PIC X(20)   VALUE "算定日１１".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD12      PIC X(20)   VALUE "算定日１２".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD13      PIC X(20)   VALUE "算定日１３".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD14      PIC X(20)   VALUE "算定日１４".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD15      PIC X(20)   VALUE "算定日１５".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD16      PIC X(20)   VALUE "算定日１６".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD17      PIC X(20)   VALUE "算定日１７".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD18      PIC X(20)   VALUE "算定日１８".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD19      PIC X(20)   VALUE "算定日１９".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD20      PIC X(20)   VALUE "算定日２０".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD21      PIC X(20)   VALUE "算定日２１".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD22      PIC X(20)   VALUE "算定日２２".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD23      PIC X(20)   VALUE "算定日２３".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD24      PIC X(20)   VALUE "算定日２４".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD25      PIC X(20)   VALUE "算定日２５".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD26      PIC X(20)   VALUE "算定日２６".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD27      PIC X(20)   VALUE "算定日２７".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD28      PIC X(20)   VALUE "算定日２８".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD29      PIC X(20)   VALUE "算定日２９".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD30      PIC X(20)   VALUE "算定日３０".
           03  FILLER                  PIC X(01)   VALUE ",".
           03  CONST-CSV-SRYYMD31      PIC X(20)   VALUE "算定日３１".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    monpe印刷データ泣き別れ修正サブ
           COPY  "CPORCSFIXREDDATA.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    職員情報
           COPY  "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
           COPY  "CPSK1044.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者照会
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    患者一覧
           COPY "HCM28B.INC"
                                   REPLACING  //HCM28B//
                                   BY         //HC28//.
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCXKANACONV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    パラメタ取得・保存
           COPY    "CPORCSPARA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      *
      ******************************************************************
       PROCEDURE                       DIVISION
                                       USING     COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
                                       FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       CNT-AREA
      *
      *    ＤＢオープン
           PERFORM 120-DBOPEN-SEC
      *
           MOVE    SPACE               TO  SPA-AREA
           INITIALIZE                      SPA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPNUM
                                               WRK-PARA-ID
                                               WRK-PARA-TERMID
                                               WRK-PARA-CSVFILENAME
                                               WRK-PARA-PUSHFILENAME
                                               WRK-PARA-TBL-KEY
                                               WRK-PARA-TBL-GROUP
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SPARA-AREA
           MOVE    "LOAD"              TO  SPARA-REQUEST
           MOVE    WRK-PARA-ID         TO  SPARA-ID
           CALL    "ORCSPARA"          USING
                                       SPARA-AREA
                                       SPA-AREA
           MOVE    SPARA-DATA          TO  WKBQ01
      *
           MOVE    WKBQ01-STAFFCD      TO  SPA-STAFFCD
           MOVE    WKBQ01-SYSYMD       TO  SPA-SYSYMD
           MOVE    MCP-USER            TO  SPA-OPID
      *
           INITIALIZE              ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"   USING
                                   ORCSMCNDATEAREA
      *
           INITIALIZE              UIDIO-AREA.
           CALL    "orcuidset"     USING
                                   UIDIO-ST
                                   UIDIO-AREA
      *
           MOVE    UIDIO-UID           TO  WRK-UUID
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *
           MOVE    WRK-PARA-TERMID     TO  SPA-TERMID
      *
           CALL    "ORCSENCODING"          USING
                                           MCPAREA
                                           SPA-AREA
           MOVE    SPA-ENCODING        TO  WRK-ENCODING9
      *
           PERFORM 900-JOB-STEP-START-SEC
      *
      *    ファイルオープン
           MOVE    WRK-PARA-HOSPNUM TO  HC01PARA-HOSPNUM
           MOVE    "HC99"           TO  HC01PARA-FORM-ID
           MOVE    WRK-PARA-TERMID  TO  HC01PARA-TERMID
           INSPECT HC01PARA-TERMID  REPLACING   ALL " " BY  "Z"
           MOVE    WKBQ01-SYSTIME   TO  HC01PARA-SYOHMS
           MOVE    1                TO  HC01PARA-CNT
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME
                                    TO  SGETTEMP-BASENAMES (1)
           MOVE    WRK-PARA-CSVFILENAME
                                    TO  SGETTEMP-BASENAMES (2)
           MOVE    WRK-PARA-PUSHFILENAME
                                    TO  SGETTEMP-BASENAMES (3)
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                    TO  ASGNPARA
           MOVE    SGETTEMP-ST      TO  ASGNPARA-ST
           MOVE    SGETTEMP-FULLNAMES (2)
                                    TO  CSVNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                    TO  PUSHFOUNDNAME
           MOVE    SGETTEMP-TEMPDIR TO  MCP-TEMPDIR
      *
           OPEN    OUTPUT              CSV-FILE
           OPEN    OUTPUT              PRT-FILE
      *
      *    プリンタ名取得処理
           PERFORM 110-PRTNUM-GET-SEC
           MOVE    SYS-1031-PRTNM(6)
                                    TO  WRK-PRTNM
      *
           PERFORM 800-OFFSET-GET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ名取得処理
      *****************************************************************
       110-PRTNUM-GET-SEC                  SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    WKBQ01-SYSYMD     TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           .
      *
       110-PRTNUM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
                                       CNT-KNS
                                       CNT-CSV-RENNUM
      *
           MOVE    ZERO            TO  KEY-CSV-PTID
           MOVE    LOW-VALUE       TO  KEY-CSV-SRYCD
           MOVE    ZERO            TO  FLG-CSV-EDIT
      *
      *    患者対象データ検索処理
           PERFORM 910-PTSRH-ODR-SEL-SEC
      *
      *    患者データ読み飛ばし処理
           PERFORM 910-PTSRH-SKIP-SEC
      *
           MOVE    KEY-CSV-G       TO  KEY-PRT-G
           MOVE    FLG-CSV-EDIT    TO  FLG-PRT-EDIT
           MOVE    CNT-CSV-RENNUM  TO  CNT-PRT-RENNUM
      *
      *    処理件数算出
           COMPUTE WRK-PROC-KNS    =   WKBQ01-EDKNS
                                   -   WKBQ01-STKNS
                                   +   1
      *
           COMPUTE CNT-PAGE        =   WKBQ01-STPAGE
                                   -   1
      *
           INITIALIZE                  HC28
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1        >   WRK-PROC-KNS )
                OR       ( FLG-PTSRH   NOT =   ZERO )
      *
               PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *        ＣＳＶ編集処理
               PERFORM 800-CSV-EDIT-SEC
      *
      *        帳票編集処理
               PERFORM 800-PRT-EDIT-SEC
      *
      *        患者マスタ読込処理
               PERFORM 910-PTSRH-ODR-FET-SEC
      *
           END-PERFORM
      *
           IF    ( WKBQ01-REQUEST-PRINT    =   "1" )
               IF  ( CNT-LINE      >   ZERO )
      *            帳票出力処理
                   PERFORM 800-PRT-WRITE-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE   CSV-FILE
                   PRT-FILE
      *
           IF    ( WKBQ01-REQUEST-PRINT    =   "1" )
      *
               MOVE    SPACE           TO  SHELLTBL
               MOVE    "HCALL_prv.sh"  TO  SHELLTBL-NAME
      *
               MOVE    ASGNPARA (ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG1
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
               MOVE    WRK-ENCODING    TO  SHELLTBL-ARG3
               MOVE    "0"             TO  SHELLTBL-ARG4
               IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                   MOVE    "患者照会検索結果"  TO  SHELLTBL-ARG5
                   MOVE    SPA-CLIENT-SHOWDIALOG
                                       TO  SHELLTBL-ARG6
               END-IF
               MOVE    SHELLTBL        TO  MCPDATA-REC
               IF    ( WKBQ01-SHELLID  =   "API" )
                   MOVE    "EXCOMMAND" TO  MCP-FUNC
               ELSE
                   MOVE    "SHELL"     TO  MCP-FUNC
               END-IF
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "daily"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           ELSE
                CALL    "CBL_DELETE_FILE"   USING
                                            ASGNPARA
           END-IF
      *
           IF    ( WKBQ01-REQUEST-CSV  =   "1" )
               IF    ( WKBQ01-SHELLID  =   "API" )
      *            大容量APIではシェルでファイルを保存する。
                   CONTINUE
               ELSE
                   MOVE    CNT-CSV         TO  WKBQ01-CSV-ROWS
                   INITIALIZE                  SPARA-AREA
                   MOVE    WRK-PARA-ID     TO  SPARA-ID
                   MOVE    WKBQ01          TO  SPARA-DATA
                   MOVE    "UPDATE"        TO  SPARA-REQUEST
                   CALL    "ORCSPARA"          USING
                                               SPARA-AREA
                                               SPA-AREA
               END-IF
           ELSE
                CALL    "CBL_DELETE_FILE"   USING
                                            CSVNAME
           END-IF
      *
           IF   (  WKBQ01-SHELLID    =    "API" )
               PERFORM 900-PUSH-SEC
           END-IF
      *
           PERFORM 900-PTSRH-DEL1-SEC
      *
           PERFORM 900-JOB-STEP-END-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    オフセット取得処理
      *****************************************************************
       800-OFFSET-GET-SEC              SECTION.
      *
           INITIALIZE                  SYS-1044-REC
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1044-HOSPNUM
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    CONST-FORMID       TO  SYS-1044-FORMID
           STRING  WRK-PRTNM           DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
           INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE  MCPDATA-REC       TO  SYS-1044-REC
           ELSE
               INITIALIZE                  SYS-1044-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF   (  SYS-1044-FORMID  =   CONST-FORMID  )
            AND (  SYS-1044-PRTNM   =   WRK-PRTNM    )
               CONTINUE
           ELSE
               INITIALIZE  SYS-1044-REC
           END-IF
      *
           .
       800-OFFSET-GET-EXT.
            EXIT.
      *****************************************************************
      *    ＣＳＶ編集処理
      *****************************************************************
       800-CSV-EDIT-SEC                SECTION.
      *
           INITIALIZE                  CSV-EDIT-AREA
      *
           COMPUTE CNT-CSV =   CNT-CSV +   1
           IF    ( CNT-CSV =   1 )
               MOVE    CONST-CSV-HEADER
                                   TO  CSV-REC
               WRITE   CSV-REC
      *
               INITIALIZE              CSV-EDIT-AREA
           END-IF
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               MOVE    SPACE       TO  CSV-SRYYMD (IDXD)
           END-PERFORM
      *
           IF    ( PTSRH-PTID  NOT =   KEY-CSV-PTID  )
            OR   ( PTSRH-SRYCD NOT =   KEY-CSV-SRYCD )
               COMPUTE CNT-CSV-RENNUM  =   CNT-CSV-RENNUM  +   1
               MOVE    PTSRH-PTID  TO  KEY-CSV-PTID
               MOVE    PTSRH-SRYCD TO  KEY-CSV-SRYCD
               MOVE    1           TO  FLG-CSV-EDIT
           END-IF
      *
           IF    ( FLG-CSV-EDIT    NOT =   ZERO )
      *
               MOVE    ZERO        TO  FLG-CSV-EDIT
      *
               MOVE    CNT-CSV-RENNUM  TO  CSV-NO
               MOVE    PTSRH-PTNUM TO  CSV-PTNUM
               MOVE    PTINF-NAME
                                   TO  CSV-NAME
               MOVE    PTINF-KANANAME
                                   TO  CSV-KANANAME
               EVALUATE    PTINF-SEX
               WHEN    1
                   MOVE    "男"    TO  CSV-SEX
               WHEN    2
                   MOVE    "女"    TO  CSV-SEX
               WHEN    OTHER
                   MOVE    SPACE   TO  CSV-SEX
               END-EVALUATE
      *
               MOVE    PTINF-BIRTHDAY (1:4)
                                   TO  CSV-BIRTHDAY (1:4)
               MOVE    "/"         TO  CSV-BIRTHDAY (5:1)
               MOVE    PTINF-BIRTHDAY (5:2)
                                   TO  CSV-BIRTHDAY (6:2)
               MOVE    "/"         TO  CSV-BIRTHDAY (8:1)
               MOVE    PTINF-BIRTHDAY (7:2)
                                   TO  CSV-BIRTHDAY (9:2)
      *
               PERFORM 900-AGE-EDIT-SEC
               MOVE    WRK-AGE-Z   TO  CSV-AGENUM
               MOVE    WRK-AGE-MEI TO  CSV-AGETANI
      *
               MOVE    PTSRH-SRYCD     TO  CSV-SRYCD
               MOVE    PTSRH-SRYNAME   TO  CSV-SRYNAME
               MOVE    PTSRH-SRYKANANAME   TO  CSV-SRYKANANAME
           END-IF
      *
           IF    ( PTSRH-SURYO-EDIT    =   1 )
               PERFORM 8001-SRYSURYO-EDIT-SEC
               MOVE    WRK-SRYSURYOX
                                   TO  CSV-SRYSURYO
           ELSE
               IF    ( PTSRH-SRYYM     =   "999999" )
                   MOVE    "合計"  TO  CSV-SRYSURYO
               END-IF
           END-IF
      *
           MOVE    PTSRH-ZAIKAISU  TO  WRK-ZAIKAISUZ
           MOVE    WRK-ZAIKAISUZ   TO  CSV-ZAIKAISU
      *
           PERFORM 8001-SURYOTOTAL-EDIT-SEC
           MOVE    WRK-SURYOTOTALX TO  CSV-SURYOTOTAL
      *
           IF    ( PTSRH-SRYYM     NOT =   "999999" )
               MOVE    PTSRH-SRYYM(1:4)
                                           TO  CSV-SRYYM (1:4)
               MOVE    "/"                 TO  CSV-SRYYM (5:1)
               MOVE    PTSRH-SRYYM(5:2)    TO  CSV-SRYYM (6:2)
           END-IF
      *
           MOVE    ZERO            TO  IDXA
           MOVE    PTSRH-SRYYM     TO  WRK-SRYYM
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    SPACE       TO  CSV-SRYYMD (IDXD)
               IF    ( PTSRH-DAY (IDXD)    >   ZERO )
                   COMPUTE IDXA    =   IDXA    +   1
                   MOVE    IDXD        TO  WRK-SRYDD
                   MOVE    WRK-SRYYMD(1:4)
                                       TO  CSV-SRYYMD (IDXA)(1:4)
                   MOVE    "/"         TO  CSV-SRYYMD (IDXA)(5:1)
                   MOVE    WRK-SRYYMD(5:2)
                                       TO  CSV-SRYYMD (IDXA)(6:2)
                   MOVE    "/"         TO  CSV-SRYYMD (IDXA)(8:1)
                   MOVE    WRK-SRYYMD(7:2)
                                       TO  CSV-SRYYMD (IDXA)(9:2)
               END-IF
      *
           END-PERFORM
      *
           MOVE    CSV-EDIT-AREA   TO  CSV-REC
      *
           WRITE   CSV-REC
      *
           .
       800-CSV-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       8001-SRYSURYO-EDIT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-SRYSURYOX
      *
           IF    ( PTSRH-SRYSURYO  =   ZERO )
               MOVE    "***"
                                   TO  WRK-SRYSURYOX
           ELSE
               MOVE    PTSRH-SRYSURYO
                                   TO  WRK-SRYSURYOZ
               PERFORM VARYING IDXA    FROM    11  BY  -1
                       UNTIL ( IDXA    <   1 )
                        OR   ( WRK-SRYSURYOX(IDXA:1)
                                   NOT =   "0" )
                       MOVE    SPACE   TO WRK-SRYSURYOX(IDXA:1)
               END-PERFORM
               IF    ( IDXA    >   0 )
                   IF    ( WRK-SRYSURYOX (IDXA:1)
                                    =   "." )
                       MOVE    SPACE   TO WRK-SRYSURYOX(IDXA:1)
                   END-IF
               END-IF
           END-IF
      *
           .
       8001-SRYSURYO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    合計編集処理
      *****************************************************************
       8001-SURYOTOTAL-EDIT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-SURYOTOTALX
      *
           MOVE    PTSRH-SURYOTOTAL    TO  WRK-SURYOTOTALZ
           PERFORM VARYING IDXA    FROM    13  BY  -1
                   UNTIL ( IDXA    <   1 )
                    OR   ( WRK-SURYOTOTALX(IDXA:1)
                               NOT =   "0" )
                   MOVE    SPACE   TO WRK-SURYOTOTALX(IDXA:1)
           END-PERFORM
           IF    ( IDXA    >   0 )
               IF    ( WRK-SURYOTOTALX (IDXA:1)
                                    =   "." )
                   MOVE    SPACE   TO WRK-SURYOTOTALX(IDXA:1)
               END-IF
           END-IF
      *
           .
       8001-SURYOTOTAL-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       800-PRT-EDIT-SEC                SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           MOVE    ZERO        TO  FLG-PRT-EDIT
           IF    ( CNT-LINE    =   1 )
               MOVE    1       TO  FLG-PRT-EDIT
           END-IF
      *
           IF    ( PTSRH-PTID  NOT =   KEY-PRT-PTID  )
            OR   ( PTSRH-SRYCD NOT =   KEY-PRT-SRYCD )
               MOVE    1       TO  FLG-PRT-EDIT
               COMPUTE CNT-PRT-RENNUM  =   CNT-PRT-RENNUM  +   1
               MOVE    PTSRH-PTID  TO  KEY-PRT-PTID
               MOVE    PTSRH-SRYCD TO  KEY-PRT-SRYCD
           END-IF
      *
           IF    ( FLG-PRT-EDIT    =   1 )
               PERFORM 8001-PRT-EDIT-SEC
           END-IF
      *
           IF    ( PTSRH-SURYO-EDIT    =   1 )
               PERFORM 8001-SRYSURYO-EDIT-SEC
               MOVE    WRK-SRYSURYOX
                                   TO  HC28-SRYSURYO (CNT-LINE)
           ELSE
               IF    ( PTSRH-SRYYM     =   "999999" )
                   MOVE    "    合計"
                                   TO  HC28-SRYSURYO (CNT-LINE)
               END-IF
           END-IF
      *
           MOVE    PTSRH-ZAIKAISU  TO  WRK-ZAIKAISUZ
           MOVE    WRK-ZAIKAISUZ   TO  HC28-ZAIKAISU (CNT-LINE)
      *
           PERFORM 8001-SURYOTOTAL-EDIT-SEC
           MOVE    WRK-SURYOTOTALX TO  HC28-SURYOTOTAL (CNT-LINE)
      *
           IF    ( PTSRH-SRYYM     NOT =   "999999" )
               MOVE    PTSRH-SRYYM TO  WRK-SYMD
               MOVE    "01"        TO  WRK-SYMD(7:2)
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG1 TO HC28-SRYYM (CNT-LINE)
           END-IF
      *
      *    備考
           INITIALIZE                  WRK-NISSU-DAY-G
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
               IF    ( PTSRH-DAY(IDXD)     >   ZERO )
                   MOVE    1       TO  WRK-NISSU-DAY (IDXD)
               END-IF
           END-PERFORM
           PERFORM 8001-NISSU-HEN-SEC
           IF    ( WRK-NISSU-MES (47:) NOT =   SPACE )
               MOVE    WRK-NISSU-MES
                                   TO  HC28-SRYYMD2(CNT-LINE)
           ELSE
               MOVE    WRK-NISSU-MES
                                   TO  HC28-SRYYMD (CNT-LINE)
           END-IF
      *
           IF  ( CNT-LINE         >=   CONST-LINE )
      *
      *        帳票出力処理
               PERFORM 800-PRT-WRITE-SEC
      *
               INITIALIZE              HC28
               MOVE    ZERO        TO  CNT-LINE
      *
      *
           END-IF
      *
           .
       800-PRT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       8001-PRT-EDIT-SEC               SECTION.
      *
           MOVE    CNT-PRT-RENNUM  TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9       TO  HC28-CD     (CNT-LINE)
           IF    ( PTSRH-PTNUM (11:)   NOT =   SPACE )
              MOVE    PTSRH-PTNUM(1:10)
                                   TO  HC28-PTNUMUP(CNT-LINE)
              MOVE    PTSRH-PTNUM(11:)
                                   TO  HC28-PTNUMDN(CNT-LINE)
           ELSE
              MOVE    PTSRH-PTNUM
                                   TO  HC28-PTNUM  (CNT-LINE)
           END-IF
      *
           IF    ( PTINF-NAME (21: ) NOT =   SPACE )
               MOVE    PTINF-NAME
                                   TO  HC28-NAME2  (CNT-LINE)
           ELSE
               MOVE    PTINF-NAME
                                   TO  HC28-NAME   (CNT-LINE)
           END-IF
      *
           EVALUATE    PTINF-SEX
           WHEN    1
               MOVE    "男"        TO  HC28-SEIBETU(CNT-LINE)
           WHEN    2
               MOVE    "女"        TO  HC28-SEIBETU(CNT-LINE)
           END-EVALUATE
      *
           PERFORM 900-AGE-EDIT-SEC
           MOVE    WRK-AGE-Z       TO  HC28-AGENUM (CNT-LINE)
           MOVE    WRK-AGE-MEI     TO  HC28-AGETANI(CNT-LINE)
      *
           MOVE    PTSRH-SRYCD     TO  HC28-SRYCD (CNT-LINE)
      *
           IF    ( PTSRH-SRYNAME (41:) NOT =   SPACE )
              MOVE    PTSRH-SRYNAME
                                   TO  HC28-SRYNAME2(CNT-LINE)
           ELSE
              MOVE    PTSRH-SRYNAME
                                   TO  HC28-SRYNAME (CNT-LINE)
           END-IF
      *
           .
       8001-PRT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理
      *****************************************************************
       8001-NISSU-HEN-SEC            SECTION.
      *
           INITIALIZE  WRK-NISSU-AREA
      *
           MOVE    1                   TO  WRK-NISSU-PTR
           MOVE    1                   TO  WRK-NISSU-ST
           MOVE    SPACE               TO  WRK-NISSU-MES
           MOVE    ZERO                TO  WRK-NISSU-KUGIRI-FLG
      *
           PERFORM UNTIL ( WRK-NISSU-ST    >   31 )
      *
               IF    ( WRK-NISSU-DAY (WRK-NISSU-ST )
                                       NOT =   ZERO )
      *
                   MOVE    ZERO        TO  WRK-NISSU-RENZOKU-CNT
                   COMPUTE WRK-NISSU-ED    =   WRK-NISSU-ST    +   1
                   PERFORM UNTIL ( WRK-NISSU-ED    >   31 )
                            OR   ( WRK-NISSU-DAY (WRK-NISSU-ED)
                                           =   ZERO )
                       COMPUTE WRK-NISSU-ED
                            =  WRK-NISSU-ED  +   1
      *
                       COMPUTE WRK-NISSU-RENZOKU-CNT
                            =  WRK-NISSU-RENZOKU-CNT   +   1
      *
                   END-PERFORM
      *
                   IF    ( WRK-NISSU-KUGIRI-FLG   =   ZERO )
                       STRING  "　"            DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
      *
                       MOVE    1       TO  WRK-NISSU-KUGIRI-FLG
      *
                   ELSE
                       STRING  "，"            DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                   END-IF
      *
                   IF    ( WRK-NISSU-RENZOKU-CNT  >   ZERO )
                       COMPUTE WRK-NISSU-ED    =   WRK-NISSU-ED    -   1
      *
                       IF    ( WRK-NISSU-ST    <   10 )
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-9
                       ELSE
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-Z9
                       END-IF
      *
                       IF    ( WRK-NISSU-ED    <   10 )
                           MOVE    WRK-NISSU-ED    TO  WRK-NISSU-ED-9
                       ELSE
                           MOVE    WRK-NISSU-ED    TO  WRK-NISSU-ED-Z9
                       END-IF
      *
                       STRING  WRK-NISSU-ST-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                               "〜"                DELIMITED   BY  SIZE
                               WRK-NISSU-ED-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                       COMPUTE WRK-NISSU-ST
                           =   WRK-NISSU-ED      +   1
                   ELSE
      *
                       IF    ( WRK-NISSU-ST    <   10 )
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-9
                       ELSE
                           MOVE    WRK-NISSU-ST    TO  WRK-NISSU-ST-Z9
                       END-IF
      *
                       STRING  WRK-NISSU-ST-X      DELIMITED   BY  SPACE
                               "日"                DELIMITED   BY  SIZE
                       INTO    WRK-NISSU-MES
                       WITH    POINTER WRK-NISSU-PTR
                       END-STRING
                       COMPUTE WRK-NISSU-ST
                           =   WRK-NISSU-ST      +   1
                   END-IF
      *
               ELSE
                   COMPUTE WRK-NISSU-ST
                       =   WRK-NISSU-ST      +   1
               END-IF
           END-PERFORM
      *
           MOVE    WRK-NISSU-MES       TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  WRK-NISSU-MES
      *
           .
       8001-NISSU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       800-PRT-WRITE-SEC           SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
      *
      *    ヘッダ部編集
           MOVE    WKBQ01-HYODAI   TO  HC28-HYODAI
      *
           MOVE    WKBQ01-JYOKEN1  TO  HC28-JYOKEN (1)
      *
           MOVE    WKBQ01-JYOKEN2  TO  HC28-JYOKEN (2)
      *
           MOVE    WKBQ01-SYSYMD   TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG3    TO  HC28-SAKUSEIBI
      *
           MOVE    CNT-PAGE        TO  WRK-ZZZZ9C
           MOVE    WRK-ZZZZ9C      TO  HC28-PAGE
      *
      *
           MOVE    SPACE           TO  PRT-REC
           INITIALIZE                  PRT-REC
          MOVE    "HCM28B.red"    TO  PRT-PRTID
           MOVE    WRK-PRTNM       TO  PRT-PRTNM
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
      *
           IF    ( WKBQ01-SHELLID      =   "API" )
                   MOVE    "7"     TO  PRT-PRTKBN
           ELSE
               IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                   MOVE    "5"     TO  PRT-PRTKBN
               ELSE
                   MOVE    "6"     TO  PRT-PRTKBN
               END-IF
           END-IF
           MOVE    MCP-TEMPDIR     TO  PRT-OUT
           MOVE    WRK-PARA-TBL-KEY TO PRT-TBL-KEY
           MOVE    1               TO  PRT-RENNUM
           IF    ( WKBQ01-SHELLID      =   "API" )
               MOVE    WRK-PARA-TBL-GROUP
                                       TO  PRT-TBL-GROUP
           ELSE
               MOVE    SMCNDATE-YMD    TO  PRT-TBL-GROUP
               MOVE    SMCNDATE-HMS    TO  PRT-TBL-GROUP(9:)
           END-IF
           MOVE    1               TO  PRT-SHORI-RENNUM
           MOVE    CNT-PAGE        TO  PRT-PAGE
           MOVE    WRK-UUID        TO  PRT-UUID
      *
           MOVE    "1"             TO  PRT-SITEKBN
           INITIALIZE                  SFIXREDDATA-AREA
           MOVE    "1"             TO  SFIXREDDATA-SITEKBN
           MOVE    PRT-PRTID       TO  SFIXREDDATA-FORMID
           MOVE    HC28            TO  SFIXREDDATA-DATA
           CALL    "ORCSFIXREDDATA"    USING
                                       SFIXREDDATA-AREA
                                       SPA-AREA
           MOVE    SFIXREDDATA-DATA
                                   TO  PRT-PRTDATA
      *
           WRITE   PRT-REC
      *
           .
       800-PRT-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           INSPECT WRK-HENYMDG3   REPLACING  ALL "  "  BY  "　"
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE           TO  WRK-OUT-INPUT
           MOVE    ZERO            TO  WRK-OUT-MAX
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE 300                TO  KANACONV-LEN
           MOVE 1000               TO  KANACONV-STLEN
           MOVE 1                  TO  KANACONV-CONV-FLAG
           MOVE 15                 TO  KANACONV-CHAR-TYPE
           MOVE WRK-MAE-INPUT      TO  KANACONV-INDATA
           CALL "kanaconv"             USING ORCXKANACONVAREA
           IF    ( KANACONV-RETURN =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-OUT-INPUT
               MOVE    KANACONV-MAXLEN TO  WRK-OUT-MAX
           END-IF
      *
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    年齢編集処理
      *****************************************************************
       900-AGE-EDIT-SEC                SECTION.
      *
           IF  (   PTINF-BIRTHDAY
                                       <   WKBQ01-SYSYMD )
               MOVE    PTINF-BIRTHDAY
                                       TO  WRK-CALC-YMD1
               MOVE    WKBQ01-SYSYMD   TO  WRK-CALC-YMD2
               PERFORM 900-AGE-CALC-SEC
           ELSE
               MOVE    WKBQ01-SYSYMD   TO  WRK-CALC-YMD1
               MOVE    PTINF-BIRTHDAY
                                       TO  WRK-CALC-YMD2
               PERFORM 900-AGE-CALC-SEC
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       *   -1
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       *   -1
           END-IF
      *
           IF    ( WRK-AGE-YY          =   ZERO )
               MOVE    WRK-AGE-MM      TO  WRK-AGE-Z
               MOVE    "ヶ月"          TO  WRK-AGE-MEI
           ELSE
               MOVE    WRK-AGE-YY      TO  WRK-AGE-Z
               MOVE    "歳"            TO  WRK-AGE-MEI
           END-IF
      *
           .
       900-AGE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       900-AGE-CALC-SEC                SECTION.
      *
           COMPUTE WRK-AGE-YY          =   WRK-CALC-YY2
                                       -   WRK-CALC-YY1
           IF    ( WRK-CALC-YMD1(5:4)  >   WRK-CALC-YMD2(5:4) )
               COMPUTE WRK-AGE-YY      =   WRK-AGE-YY
                                       -   1
               COMPUTE WRK-AGE-MM      =  (WRK-CALC-MM2    +   12 )
                                       -   WRK-CALC-MM1
           ELSE
               COMPUTE WRK-AGE-MM      =   WRK-CALC-MM2
                                       -   WRK-CALC-MM1
           END-IF
           IF    ( WRK-CALC-DD1        >   WRK-CALC-DD2 )
               COMPUTE WRK-AGE-MM      =   WRK-AGE-MM
                                       -   1
           END-IF
      *
           .
       900-AGE-CALC-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得処理
      *****************************************************************
       900-STR-LEN-GET-SEC                 SECTION.
      *
           PERFORM VARYING IDX-LEN FROM    WRK-STR-LEN BY  -1
               UNTIL   ( IDX-LEN   <   1)
                OR     ( WRK-STR (IDX-LEN : 1) NOT =   SPACE)
      *
               CONTINUE
           END-PERFORM
      *
           MOVE    IDX-LEN     TO  WRK-STR-LEN
      *
           .
       900-STR-LEN-GET-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       910-PTSRH-ODR-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID          TO  PTSRH-TERMID
           MOVE    "2"                 TO  PTSRH-DATAKBN
           MOVE    "%"                 TO  PTSRH-UPYMD
                                           PTSRH-UPHMS
           MOVE    WKBQ01-SQL          TO  PTSRH-SQL
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       910-PTSRH-ODR-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会FETCH処理
      *****************************************************************
       910-PTSRH-ODR-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTSRH
               MOVE    MCPDATA-REC     TO  PTSRH-REC
               COMPUTE CNT-KNS =   CNT-KNS +   1
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       910-PTSRH-ODR-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＳＨ処理
      *****************************************************************
       900-PUSH-SEC                    SECTION.
      *
           OPEN    OUTPUT              PUSHFOUND-FILE
      *
           INITIALIZE                  PUSHFOUND-REC
      *
           MOVE    "found"         TO  PUSHFOUND-EVENT
           MOVE    WKBQ01-KARTE-UID
                                   TO  PUSHFOUND-KERTE-UID
           MOVE    WRK-PARA-TERMID TO  PUSHFOUND-ORCA-UID
      *
           IF    ( WKBQ01-REQUEST-CSV  =   "1" )
               MOVE    CNT-CSV     TO  WRK-NUM-Z
               MOVE    FUNCTION TRIM(WRK-NUM-Z)
                                   TO  PUSHFOUND-NUMBER-OF-RECORDS
           END-IF
      *
           IF    ( WKBQ01-REQUEST-PRINT  =   "1" )
               COMPUTE WRK-NUM     =   CNT-PAGE
                                   -   WKBQ01-STPAGE
                                   +   1
               MOVE    WRK-NUM     TO  WRK-NUM-Z
               MOVE    FUNCTION TRIM(WRK-NUM-Z)
                                   TO  PUSHFOUND-NUMBER-OF-SHEETS
           END-IF
      *
           WRITE   PUSHFOUND-REC
      *
           CLOSE   PUSHFOUND-FILE
      *
           .
       900-PUSH-EXT.
           EXIT.
      *****************************************************************
      *    患者照会削除処理
      *****************************************************************
       900-PTSRH-DEL1-SEC              SECTION.
      *
           INITIALIZE                  PTSRH-REC
           MOVE    SPA-HOSPNUM     TO  PTSRH-HOSPNUM
           MOVE    SPA-TERMID      TO  PTSRH-TERMID
           MOVE    "2"             TO  PTSRH-DATAKBN
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       900-PTSRH-DEL1-EXT.
           EXIT.
      *****************************************************************
      *    患者マスタ読み飛ばし処理
      *****************************************************************
       910-PTSRH-SKIP-SEC         SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1        >       WKBQ01-STKNS )
                 OR      ( FLG-PTSRH   NOT =   ZERO )
      *
      *        患者マスタ読込処理
               PERFORM 910-PTSRH-ODR-FET-SEC
      *
               IF    ( PTSRH-PTID  NOT =   KEY-CSV-PTID  )
                OR   ( PTSRH-SRYCD NOT =   KEY-CSV-SRYCD )
      *
                   COMPUTE CNT-CSV-RENNUM  =   CNT-CSV-RENNUM  +   1
                   MOVE    PTSRH-PTID  TO  KEY-CSV-PTID
                   MOVE    PTSRH-SRYCD TO  KEY-CSV-SRYCD
                   MOVE    1           TO  FLG-CSV-EDIT
               ELSE
                   MOVE    ZERO        TO  FLG-CSV-EDIT
               END-IF
      *
           END-PERFORM
      *
           .
       910-PTSRH-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               INITIALIZE              PTINF-REC
               MOVE    1           TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ステップ管理開始処理
      *****************************************************************
       900-JOB-STEP-START-SEC      SECTION.
      *
           IF    ( WKBQ01-SHELLID      =   "API" )
               CONTINUE
           ELSE
               MOVE    "STS"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    CONST-JOBID         TO  JOB-JOBID
               MOVE    CONST-JOB-SHELLID   TO  JOB-SHELLID
               MOVE    "患者一覧出力（診療行為）"
                                           TO  JOB-SHELLMSG
               MOVE    "ORCBQ01"           TO  JOB-PGID
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           .
       900-JOB-STEP-START-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       900-JOB-STEP-END-SEC            SECTION.
      *
           IF    ( WKBQ01-SHELLID      =   "API" )
               CONTINUE
           ELSE
               MOVE    "STE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    CONST-JOBID         TO  JOB-JOBID
               MOVE    CONST-JOB-SHELLID   TO  JOB-SHELLID
               IF    ( WKBQ01-REQUEST-CSV  =   "1" )
                   MOVE    CNT-CSV         TO  JOB-UPDCNT
               END-IF
               IF    ( WKBQ01-REQUEST-PRINT  =   "1" )
                   COMPUTE JOB-UPDCNT  =   CNT-PAGE
                                       -   WKBQ01-STPAGE
                                       +   1
               END-IF
      *
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           .
       900-JOB-STEP-END-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　接続処理
      *****************************************************************
       120-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       120-DBOPEN-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
