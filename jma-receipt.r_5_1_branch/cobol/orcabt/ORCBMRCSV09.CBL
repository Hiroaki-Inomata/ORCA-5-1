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
       PROGRAM-ID.             ORCBMRCSV09.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 診療情報データ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/09/04    NACL-森脇     新規作成
      *  13/11/05    NACL-伊藤     改修
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
      * TODO
      *   未編集レコード
      *      25 患者特殊公費レコード
      *      27 (案)労災保険レコード
      *     221 (案)医薬品単位変換レコード
      *     228 用法補足レコード
      *     231 処方特記事項レコード
      *     501 備考レコード
      *
      *   ver4.7とver4.8で修正する
      *     ver4.8で検索して対処する
      *
      *   保険情報出力は見直し（正しく条件から記録されているか）
      *   不均等のレコードの編集・・まだ最終形ではない
      *   処方せん備考欄のコメントの記録方法
      *
      *   病名レコード出力の条件が何も検討されていない
      *     例えば、診療開始日が初診算定日以降とか当月有効なものとか
      *
      *   213 医薬品補足レコードに粉砕区分の記録
      *     (粉砕) 099209901 システム予約コード
      *
      *   診療区分14在宅の薬剤と診療区分98のテスト
      *
      *   tbl_sryactでは診療種別区分が記録されないケースがある
      *
      *****************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    診療情報データ
           SELECT  MRCSV-FILE      ASSIGN  ASS-MRCSV
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-FILE.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    診療情報データ
       FD  MRCSV-FILE.
       01  MRCSV-REC               PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASS-MRCSV//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-FILE            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-IYKTOP          PIC 9(01).
      *
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTPUBLIC        PIC 9(01).
           03  FLG-PTKINKI         PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACTREAD      PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-GENERICNAME     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX-G               PIC 9(04).
           03  IDX-M               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX-J               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-S-SV            PIC 9(04).
      *
           03  IDX-JYU             PIC 9(04).
           03  IDX-ACT             PIC 9(02).
           03  IDX-SRY             PIC 9(02).
           03  IDX-KOH             PIC 9(04).
           03  IDX-REN             PIC 9(04).
      *
      *    件数領域
       01  CNT-AREA.
           03  CNT-OUTPUT          PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KEY.
               05  WRK-SRYKA           PIC X(02).
               05  WRK-DRCD            PIC X(05).
               05  WRK-HKNCOMBINUM     PIC X(04).
           03  WRK-TBL.
               05  WRK-ZAINUM          PIC 9(08).
               05  WRK-SRYKBN          PIC X(02).
               05  WRK-SRYSYUKBN       PIC X(03).
               05  WRK-JIHOKBN         PIC X(01).
               05  WRK-INGAIKBN        PIC 9(01).
               05  WRK-MEISAISU        PIC 9(03).
               05  WRK-SRYCD           PIC X(09).
               05  WRK-GENERICCD       PIC X(12).
               05  WRK-NAME            PIC X(120).
               05  WRK-TANICD          PIC X(03).
               05  WRK-TANINAME        PIC X(24).
               05  WRK-KZMCOMPSIKIBETU PIC 9(01).
               05  WRK-ZAIGATACD       PIC 9(02).
               05  WRK-TEN             PIC 9(09)V99.
               05  WRK-SRYSURYO        PIC 9(05)V9(05).
               05  WRK-SRYKAISU        PIC 9(03).
               05  WRK-KAISU           PIC 9(05).
               05  WRK-KANRENFLG       PIC X(01).
               05  WRK-YCOMMNTKBN      PIC X(01).
               05  WRK-BUIKBN          PIC 9(01).
           03  WRK-ZAINUM-SAVE     PIC 9(08).
           03  WRK-ZAISEQ          PIC 9(03).
      *
           03  WRK-SURYO           PIC 9(09)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1        PIC 9(09).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1        PIC ZZZZZZZZ9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-H         PIC X(15).
      *
           03  WRK-H2Z-BEFORE          PIC X(40).
           03  WRK-H2Z-AFTER           PIC X(40).
           03  WRK-H2Z-MAX             PIC 9(05).
           03  WRK-TANINAME-SAVE       PIC X(24).
      *
      *    CSV項目編集用
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(05).
               05  WRK-PO-ED           PIC 9(05).
               05  WRK-CNT             PIC 9(05).
           03  WRK-DATA            PIC X(140).
      *
      *    ファイル名
           03  WRK-CSV-FILE        PIC X(256).
           03  WRK-DRCD-E          PIC X(05).
      *
       01  PARA-AREA.
           03  PARA-VERSION-KBN    PIC X(01).
           03  PARA-ORCA-VERSION   PIC X(10).
           03  PARA-DOUJITU        PIC X(01).
           03  PARA-TEMP           PIC X(128).
      *
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-SYSYMD.
               05  SYS-SYSYY       PIC 9(04).
               05  SYS-SYSMM       PIC 9(02).
               05  SYS-SYSDD       PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
      *    プログラムオプションパラメタ
       01  POPT-AREA.
           03  POPT-NAME               PIC X(25).
           03  POPT-DATA               PIC X(100).
           03  POPT-BYOMEI             PIC X(01).
           03  POPT-SRYKBN-TBL     OCCURS  30  INDEXED   I-SKBN.
               05  POPT-SRYKBN             PIC X(02).
               05  POPT-FILLER             PIC X(01).
               05  POPT-SRYKBN-F           PIC X(01).
      *
       01  TBL-NAIYOU-AREA.
           03  TBL-NAIYOU-MAX      PIC 9(04).
           03  TBL-NAIYOU-REC.
               05  TBL-NAIYOU-DATA     OCCURS  20.
                   07  TBL-NAIYOU-KEY.
                       09  TBL-SRYKA           PIC X(02).
                       09  TBL-DRCD            PIC X(05).
                       09  TBL-HKNCOMBINUM     PIC X(04).
                   07  TBL-NAIYOU-TBL      OCCURS  400.
                       09  TBL-ZAINUM          PIC 9(08).
                       09  TBL-SRYKBN          PIC X(02).
                       09  TBL-SRYSYUKBN       PIC X(03).
                       09  TBL-JIHOKBN         PIC X(01).
                       09  TBL-INGAIKBN        PIC 9(01).
                       09  TBL-MEISAISU        PIC 9(03).
                       09  TBL-SRYCD           PIC X(09).
                       09  TBL-GENERICCD       PIC X(12).
                       09  TBL-NAME            PIC X(120).
                       09  TBL-TANICD          PIC X(03).
                       09  TBL-TANINAME        PIC X(24).
                       09  TBL-KZMCOMPSIKIBETU PIC 9(01).
                       09  TBL-ZAIGATACD       PIC 9(02).
                       09  TBL-TEN             PIC 9(09)V99.
                       09  TBL-SRYSURYO        PIC 9(05)V9(05).
                       09  TBL-SRYKAISU        PIC 9(03).
                       09  TBL-KAISU           PIC 9(05).
                       09  TBL-KANRENFLG       PIC X(01).
                       09  TBL-YCOMMNTKBN      PIC X(01).
                       09  TBL-BUIKBN          PIC 9(01).
      *
      *    診療情報出力テーブル
       01  TBL-MRCSV-AREA.
           03  TBL-MRCSV-MAX       PIC 9(04).
           03  TBL-MRCSV-REC.
               05  TBL-MRCSV-DATA  PIC X(140)  OCCURS  16.
      *
       01  CONST-AREA.
           03  CONST-SRYKBN10      PIC X(19)   VALUE
                       "11:0;12:0;13:0;14:1".
           03  CONST-SRYKBN20      PIC X(44)   VALUE
                       "21:1;22:1;23:1;24:0;25:0;26:0;27:0;28:0;39:0".
           03  CONST-SRYKBN30      PIC X(14)   VALUE
                       "31:1;32:1;33:1".
           03  CONST-SRYKBN40      PIC X(39)   VALUE
                       "40:0;50:0;54:0;60:1;64:1;70:1;80:1".
           03  CONST-SRYKBN90      PIC X(14)   VALUE
                       "90:0;92:0;97:0".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *    医療機関住所情報
           COPY    "CPSK1002.INC".
      *    診療科情報
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    帳票編集情報
           COPY    "CPSK1030.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者地域連携
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *    患者医薬品禁忌
       01  PTKINKI-REC.
           COPY    "CPPTKINKI.INC".
      *    患者病名マスタ
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    患者保険
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    患者公費
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    患者労災
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *    受診履歴マスタ
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCXKANACONV.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *    院外処方一般名編集サブ
           COPY    "CPORCSGETGENERIC.INC".
      *    お薬手帳データ制御サブパラメタ
           COPY    "CPORCSMNOTE.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
       PROCEDURE                   DIVISION
                                   USING
                                       COMMAND-PARAM.
      *
      ****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  ASS-MRCSV
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    SPA-HOSPNUM
                                               SPA-PTID
                                               SPA-NYUGAIKBN
                                               SPA-SRYYMD
                                               PARA-VERSION-KBN
                                               SPA-DRCD
                                               SPA-OPID
                                               PARA-DOUJITU
                                               PARA-ORCA-VERSION
                                               PARA-TEMP
           END-UNSTRING
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE SYS-SYSYY           =   SYS-YY  +   2000
           MOVE    SYS-MM              TO  SYS-SYSMM
           MOVE    SYS-DD              TO  SYS-SYSDD
           ACCEPT  SYS-TIME        FROM    TIME
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE  SGETTEMP-AREA
           MOVE    PARA-TEMP           TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASS-MRCSV
      *
      *    ファイルＯＰＥＮ
           OPEN    OUTPUT  MRCSV-FILE
      *
           INITIALIZE  TBL-NAIYOU-REC
           MOVE    ZERO                TO  TBL-NAIYOU-MAX
           MOVE    SPACE               TO  WRK-KEY
      *
      *    帳票編集情報
           INITIALIZE  SYSKANRI-REC
           MOVE    "1030"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1030-REC
           END-IF
      *
      *    プログラムオプション取込み
           PERFORM 900-OPTION-READ-SEC
      *
           PERFORM 100-JYURRK-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      ****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM UNTIL FLG-END   =   1
      *        受診履歴チェック処理
               PERFORM 210-JYURRK-CHK-SEC
      *        受診履歴検索
               PERFORM 900-JYURRK-FETCH-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      ****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      TBL-NAIYOU-MAX      >   ZERO
      *        診療情報出力処理
               PERFORM 310-MR-OUTPUT-SEC
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           CLOSE   MRCSV-FILE
      *
           IF      TBL-NAIYOU-MAX      >   ZERO
               PERFORM 3001-MNOTE-SEC
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      ****************************************************************
      *    パソリ登録処理
      *****************************************************************
       3001-MNOTE-SEC              SECTION.
      *
      *    CSVデータ
           IF      SPA-DRCD            =   SPACE
               MOVE    "XXXXX"             TO  WRK-DRCD-E
           ELSE
               MOVE    SPA-DRCD            TO  WRK-DRCD-E
           END-IF
      *
      *    ファイル名作成
           MOVE    SPACE               TO  WRK-CSV-FILE
           STRING  "jmamr_"
                   SPA-SRYYMD          DELIMITED   BY  SIZE
                   "_"
                   WRK-DRCD-E          DELIMITED   BY  SIZE
                   "_"
                   SYS-SYSYMD          DELIMITED   BY  SIZE
                   "_"
                   SYS-TIME            DELIMITED   BY  SIZE
                   ".csv"
                                       INTO    WRK-CSV-FILE
           END-STRING
      *
           INITIALIZE                  ORCSMNOTEAREA
           MOVE    "INS"               TO  SMNOTE-MODE
           MOVE    SPA-PTID            TO  SMNOTE-PTID
           MOVE    SPA-NYUGAIKBN       TO  SMNOTE-NYUGAIKBN
           MOVE    "/var/tmp"          TO  SMNOTE-TO-FOLDER
           MOVE    WRK-CSV-FILE        TO  SMNOTE-TO-DATA
           MOVE    "2"                 TO  SMNOTE-CODE-TYPE
           MOVE    "日医版診療情報"    TO  SMNOTE-TITLE
           MOVE    "JMAMR"             TO  SMNOTE-TBL-KEY
           MOVE    PARA-TEMP           TO  SMNOTE-CSVFILE
           MOVE    1                   TO  SMNOTE-RENNUM
           MOVE    SPA-DRCD            TO  SMNOTE-DRCD
      *    データバージョン
           MOVE    PARA-VERSION-KBN    TO  SMNOTE-DATA-VERSION
      *    出力場所コード
      *    請求確認
           MOVE    "01"                TO  SMNOTE-LOCATION-CD
           MOVE    "K03"               TO  SPA-SAKIPG
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
           MOVE    "81"                TO  SPA-PRT-FLG
           CALL    "ORCSMNOTE"         USING
                                       ORCSMNOTEAREA
                                       SPA-AREA
           .
       3001-MNOTE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込処理
      *****************************************************************
       100-JYURRK-SEC              SECTION.
      *
           INITIALIZE  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    SPA-SRYYMD          TO  JYURRK-UPSRYYMD
           MOVE    JYURRK-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               PERFORM 900-JYURRK-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-JYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       210-JYURRK-CHK-SEC          SECTION.
      *
      *    *** 包括保険分を読み飛ばし ***
           IF      JYURRK-HKNCOMBINUM  =   "9999"
               GO  TO  210-JYURRK-CHK-EXT
           END-IF
      *
           MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
           MOVE    JYURRK-DRCD         TO  WRK-DRCD
           MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBINUM
      *
           COMPUTE TBL-NAIYOU-MAX      =   TBL-NAIYOU-MAX  +   1
      *
           PERFORM VARYING IDX-JYU     FROM    1   BY  1
                   UNTIL ( IDX-JYU     >   15  )
                   OR    ( JYURRK-ZAINUM(IDX-JYU)  =   ZERO )
               MOVE    JYURRK-ZAINUM(IDX-JYU)  TO  WRK-ZAINUM
               COMPUTE IDX-REN             =   JYURRK-RENNUM
                                           +   1
      *ver4.8
      *        IF      IDX-REN             >   4
      *            MOVE    4                   TO  IDX-REN
      *****    END-IF
               PERFORM 220-SRYACT-SEC
           END-PERFORM
           .
       210-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報出力処理
      *****************************************************************
       220-SRYACT-SEC              SECTION.
      *
           INITIALIZE  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    WRK-SRYKA           TO  SRY-SRYKA
           MOVE    SPA-SRYYMD(1:6)     TO  SRY-SRYYM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
      *        診療行為検索
               PERFORM 2201-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
               INITIALIZE  SRYACT-REC
           END-IF
      *
           IF     (FLG-SRYACT          =   ZERO) AND
                  (SPA-NYUGAIKBN       =   "2")  AND
                  (SRY-SRYKBN          =   "14"  OR
                                           "21"  OR  "22"  OR  "23")
      *            一般名処方編集のための処理
               PERFORM 2201-GENERIC-SEC
           END-IF
      *
           MOVE    ZERO                TO  IDX-G
           PERFORM UNTIL   FLG-SRYACT  =   1
      *        診療行為チェック処理
               PERFORM 230-SRYACT-CHK-SEC
      *        診療行為検索
               PERFORM 2201-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       220-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為情報出力選択処理
      *****************************************************************
       2201-SRYACT-READ-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACTREAD
           PERFORM UNTIL   FLG-SRYACTREAD  =   1
      *        診療行為検索
               PERFORM 900-SRYACT-FETCH-SEC
               IF      FLG-SRYACT          =   1
                   MOVE    1                   TO  FLG-SRYACTREAD
               ELSE
                   SET     I-SKBN              TO  1
                   SEARCH      POPT-SRYKBN-TBL     VARYING   I-SKBN
                       AT  END
                           MOVE    ZERO            TO  FLG-SRYACTREAD
                       WHEN    SRY-SRYKBN      =   POPT-SRYKBN (I-SKBN)
                           MOVE    POPT-SRYKBN-F (I-SKBN)
                                                   TO  FLG-SRYACTREAD
                   END-SEARCH
               END-IF    
           END-PERFORM
           .
       2201-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般名記載の編集のための処理
      *****************************************************************
       2201-GENERIC-SEC            SECTION.
      *
           INITIALIZE  ORCSGETGENERICAREA
           MOVE    SPA-HOSPNUM         TO  ORCSGETGE-HOSPNUM
           MOVE    SPA-PTID            TO  ORCSGETGE-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ORCSGETGE-SRYYM
           MOVE    "2"                 TO  ORCSGETGE-NYUGAIKBN
           MOVE    WRK-ZAINUM          TO  ORCSGETGE-ZAINUM
           MOVE    SYS-1030-KOUHATUKA  TO  ORCSGETGE-KOUHATUKA
           CALL    "ORCSGETGENERIC"    USING
                                       ORCSGETGENERICAREA
                                       SPA-AREA
           .
       2201-GENERIC-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為チェック処理
      *****************************************************************
       230-SRYACT-CHK-SEC          SECTION.
      *
      *    診療会計読込処理
           PERFORM 900-SRYACCT-READ-SEC
           MOVE    ZERO                TO  IDX-S-SV
      *    *** 薬評分は対象外 ***
           IF      ACCT-ZAIKBN         =   1
               GO  TO  230-SRYACT-CHK-EXT
           ELSE
               MOVE    ACCT-ZAITEN         TO  WRK-TEN
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-ACT
               MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                           TO  WRK-KAISU
           END-IF
           MOVE    ACCT-MEISAISU       TO  WRK-MEISAISU
      *
           PERFORM VARYING IDX-J   FROM    1   BY  1
                   UNTIL ( TBL-NAIYOU-KEY(IDX-J)   =   SPACE )
                   OR    ( TBL-NAIYOU-KEY(IDX-J)   =   WRK-KEY )
               CONTINUE
           END-PERFORM
      
           MOVE    WRK-SRYKA           TO  TBL-SRYKA(IDX-J)
           MOVE    WRK-DRCD            TO  TBL-DRCD(IDX-J)
           MOVE    WRK-HKNCOMBINUM     TO  TBL-HKNCOMBINUM(IDX-J)
      
           PERFORM VARYING IDX-S   FROM    1   BY  1
                   UNTIL ( TBL-SRYCD(IDX-J IDX-S)  =   SPACE )
               CONTINUE
           END-PERFORM
      *
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   5 )
                   OR    ( SRY-SRYCD(IDX-SRY)  =   SPACE )
               COMPUTE IDX-G               =   IDX-G   +   1
               MOVE    SRY-ZAINUM          TO  TBL-ZAINUM(IDX-J IDX-S)
               MOVE    SRY-SRYKBN          TO  TBL-SRYKBN(IDX-J IDX-S)
               MOVE    SRY-SRYSYUKBN       TO
                                           TBL-SRYSYUKBN(IDX-J IDX-S)
               IF      ACCT-JIHOKBN        =   "1"
      *            包括算定された剤
                   MOVE    "1"                 TO
                                               TBL-JIHOKBN(IDX-J IDX-S)
               END-IF
               IF      SRY-SRYSYUKBN       =   "213"
                                           OR  "223"
                                           OR  "233"
      *            包括算定された院内処方
                   MOVE    "1"                 TO
                                               TBL-JIHOKBN(IDX-J IDX-S)
               END-IF
               IF     (SRY-SRYKBN          =   "21"
                                           OR  "22"
                                           OR  "23")   OR
                      (SRY-SRYSYUKBN       =   "148")
                   IF     (ACCT-ZAITEN         =   ZERO)   AND
                          (TBL-JIHOKBN(IDX-J IDX-S)
                                               =   SPACE)
      *                院外処方
                       MOVE    1               TO
                                               TBL-INGAIKBN(IDX-J IDX-S)
                   END-IF
               END-IF
               MOVE    WRK-MEISAISU        TO  TBL-MEISAISU(IDX-J IDX-S)
               MOVE    SRY-SRYCD(IDX-SRY)  TO  TBL-SRYCD(IDX-J IDX-S)
               MOVE    SRY-SRYSURYO(IDX-SRY)
                                           TO  TBL-SRYSURYO(IDX-J IDX-S)
               MOVE    SRY-SRYKAISU(IDX-SRY)
                                           TO  TBL-SRYKAISU(IDX-J IDX-S)
      *        剤点数・回数は剤の最後尾に近いレコードのみに記録
               IF     (SRY-SRYCD(IDX-SRY)(1:1)
                                           =   "1" OR  "6" OR  "7")  OR
                      (SRY-SRYCD(IDX-SRY)(1:3)
                                           =   "001")
                   MOVE    WRK-TEN             TO  TBL-TEN(IDX-J IDX-S)
                   MOVE    WRK-KAISU           TO
                                                 TBL-KAISU(IDX-J IDX-S)
                   IF      IDX-S-SV        NOT =   ZERO
                       MOVE    ZERO            TO
                                               TBL-TEN(IDX-J IDX-S-SV)
                                               TBL-KAISU(IDX-J IDX-S-SV)
                   END-IF
                   MOVE    IDX-S               TO  IDX-S-SV
               END-IF
      *
      *        点数マスタの内容セット
               MOVE    SRY-SRYCD(IDX-SRY)  TO  WRK-SRYCD
               PERFORM 900-TENSU-READ-SEC
      *        点数付加情報
               IF     (WRK-SRYCD(1:1)      =   "6" )  AND
                      (FLG-TENSU           =   ZERO)
                   PERFORM 900-TENSUPLUS-READ-SEC
               ELSE
                   INITIALIZE  TENSUPLUS-REC
               END-IF
               MOVE    TNS-NAME            TO  TBL-NAME(IDX-J IDX-S)
               MOVE    TNS-TANICD          TO  TBL-TANICD(IDX-J IDX-S)
               MOVE    TNS-TANINAME        TO  TBL-TANINAME(IDX-J IDX-S)
               MOVE    TNS-KZMCOMPSIKIBETU TO  TBL-KZMCOMPSIKIBETU
                                                   (IDX-J IDX-S)
               MOVE    TNS-KENTAICOMMENT   TO  TBL-YCOMMNTKBN
                                               (IDX-J IDX-S)
               IF      WRK-SRYCD           =   "099200011" OR
                                               "099200012" OR
                                               "099200013" OR
                                               "099200014" OR
                                               "099200015"
                   CONTINUE
               ELSE
                   MOVE    TNS-TANINAME        TO  WRK-TANINAME-SAVE
               END-IF
               IF     (WRK-SRYCD          >=   "002000000")  AND
                      (WRK-SRYCD          <=   "002999999")
                   MOVE    TNS-BUIKBN          TO
                                               TBL-BUIKBN(IDX-J IDX-S)
               END-IF
      *
               PERFORM 2301-ZAIGATACD-SET-SEC
      *
               IF     (ORCSGETGE-ERRCD     =   ZERO) AND
                      (SPA-NYUGAIKBN       =   "2")  AND
                      (SRY-SRYKBN          =   "14"  OR
                                               "21"  OR  "22"  OR  "23")
      *            一般名処方セット
                   PERFORM 2302-GENERIC-HEN-SEC
               END-IF
      *
               IF      SRY-INPUTNUM(IDX-SRY)   >   ZERO
      *            患者コメント検索
                   PERFORM 900-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT   TO  TBL-NAME(IDX-J IDX-S)
               END-IF
      *
               IF      SRY-MEISKYFLG(IDX-SRY)  =   "2"
                   MOVE    "1"                 TO  TBL-KANRENFLG
                                                   (IDX-J IDX-S)
               END-IF
      *
      *        不均等投与の数量コード
               IF      SRY-SRYCD(IDX-SRY)  =   "099200011" OR
                                               "099200012" OR
                                               "099200013" OR
                                               "099200014" OR
                                               "099200015"
                   MOVE    SRY-SRYSURYO(IDX-SRY)
                                               TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  WRK-H2Z-BEFORE
                   PERFORM 800-ZENKAKU-HEN-SEC
                   STRING  TNS-NAME(3:2)       DELIMITED BY SPACE
                           WRK-H2Z-AFTER       DELIMITED BY SPACE
                           WRK-TANINAME-SAVE   DELIMITED BY SPACE
                           INTO    TBL-NAME(IDX-J IDX-S)
                   END-STRING
               END-IF
      *
               COMPUTE IDX-S               =   IDX-S   +   1
           END-PERFORM
           .
       230-SRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤型コード種別をセット
      *****************************************************************
       2301-ZAIGATACD-SET-SEC      SECTION.
      *
           EVALUATE    SRY-SRYKBN
      *                内服（浸煎、湯薬）
               WHEN    "21"
                   IF      TNS-TANICD          =   "036"
      *                内服滴剤
                       MOVE    2       TO  TBL-ZAIGATACD(IDX-J IDX-S)
                   ELSE
                       MOVE    1       TO  TBL-ZAIGATACD(IDX-J IDX-S)
                   END-IF
      *                頓服
               WHEN    "22"
                   MOVE    3           TO  TBL-ZAIGATACD(IDX-J IDX-S)
      *                注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   MOVE    4           TO  TBL-ZAIGATACD(IDX-J IDX-S)
      *                外用
               WHEN    "23"
                   MOVE    5           TO  TBL-ZAIGATACD(IDX-J IDX-S)
               WHEN    OTHER
      *                材料
                   IF      SRY-SRYCD(IDX-SRY)(1:1) =   "7"
                       MOVE    5       TO  TBL-ZAIGATACD(IDX-J IDX-S)
                   ELSE
                       MOVE    ZERO    TO  TBL-ZAIGATACD(IDX-J IDX-S)
                   END-IF
           END-EVALUATE
           .
       2301-ZAIGATACD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般名編集処理
      *****************************************************************
       2302-GENERIC-HEN-SEC        SECTION.
      *
           IF      TBL-INGAIKBN(IDX-J IDX-S)
                                       =   ZERO
      *        院内処方は銘柄名である
               GO  TO  2302-GENERIC-HEN-EXT
           END-IF
      *
           IF      ORCSGETGE-GENERICNAME(IDX-G)
                                   NOT =   SPACE
               MOVE  ORCSGETGE-YAKKAKJNCD(IDX-G)
                                       TO  TBL-GENERICCD(IDX-J IDX-S)
               MOVE  ORCSGETGE-GENERICNAME(IDX-G)
                                       TO  TBL-NAME(IDX-J IDX-S)
           END-IF
           .
       2302-GENERIC-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報出力処理
      *****************************************************************
       310-MR-OUTPUT-SEC           SECTION.
      *
           PERFORM 3101-OUTPUT-HP-KIHON-SEC
           PERFORM 3102-OUTPUT-PT-KIHON-SEC
      *
      *    明細情報出力処理
           PERFORM 3103-OUTPUT-DT-SEC
           .
       310-MR-OUTPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報出力処理
      *      バージョン情報
      *      医療機関情報
      *****************************************************************
       3101-OUTPUT-HP-KIHON-SEC    SECTION.
      *
      *    バージョンレコード
           MOVE    SPACE               TO  TBL-MRCSV-REC
           MOVE    "JMAMR01"           TO  TBL-MRCSV-DATA (1)
           MOVE    1                   TO  TBL-MRCSV-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    医療機関基本情報
           INITIALIZE  SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           END-IF
      *
      *    医療機関住所情報
           INITIALIZE  SYSKANRI-REC
           MOVE    "1002"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           END-IF
      *
      *    医療機関レコード
           MOVE    SPACE               TO  TBL-MRCSV-REC
           MOVE    "1"                 TO  TBL-MRCSV-DATA (1)
           MOVE    SYS-1001-HOSPNAME   TO  TBL-MRCSV-DATA (2)
           MOVE    SYS-1001-PREFNUM    TO  TBL-MRCSV-DATA (3)
           MOVE    SYS-1001-TENHYOKBN  TO  TBL-MRCSV-DATA (4)
           MOVE    SYS-1001-HOSPCD     TO  TBL-MRCSV-DATA (5)
           MOVE    SYS-1001-HOSPID     TO  TBL-MRCSV-DATA (6)
           MOVE    6                   TO  TBL-MRCSV-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    医療機関所在地レコード
           IF     (SYS-1002-POST       =   SPACE)  AND
                  (SYS-1002-ADRS       =   SPACE)
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "2"                 TO  TBL-MRCSV-DATA (1)
               MOVE    SYS-1002-POST       TO  TBL-MRCSV-DATA (2)
               MOVE    SYS-1002-ADRS       TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    医療機関電話レコード
           IF     (SYS-1002-TEL        =   SPACE)
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "3"                 TO  TBL-MRCSV-DATA (1)
               MOVE    SYS-1002-TEL        TO  TBL-MRCSV-DATA (2)
               MOVE    2                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           .
       3101-OUTPUT-HP-KIHON-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報出力処理
      *      患者基本情報
      *      患者病名情報
      *      診療日情報
      *****************************************************************
       3102-OUTPUT-PT-KIHON-SEC    SECTION.
      *
      *    患者情報検索
           PERFORM 900-PTINF-READ-SEC
      *    患者番号検索
           PERFORM 900-PTNUM-READ-SEC
      *    患者地域連携検索
           PERFORM 900-PTPUBLIC-READ-SEC
      *
      *    患者情報レコード
           MOVE    SPACE               TO  TBL-MRCSV-REC
           MOVE    "11"                TO  TBL-MRCSV-DATA (1)
           INITIALIZE  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    PTINF-NAME          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-OUT-INPUT(1:40)
                                       =   PTINF-NAME
               MOVE    PTINF-NAME          TO  TBL-MRCSV-DATA (2)
           ELSE
               MOVE    PTINF-KANANAME      TO  TBL-MRCSV-DATA (2)
           END-IF
           MOVE    PTINF-KANANAME      TO  TBL-MRCSV-DATA (3)
           MOVE    PTINF-SEX           TO  TBL-MRCSV-DATA (4)
           MOVE    PTINF-BIRTHDAY      TO  TBL-MRCSV-DATA (5)
           MOVE    PTNUM-PTNUM         TO  TBL-MRCSV-DATA (6)
           MOVE    6                   TO  TBL-MRCSV-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    患者住所レコード
           IF     (PTINF-HOME-POST     =   SPACE)  AND
                  (PTINF-HOME-ADRS     =   SPACE)  AND
                  (PTINF-HOME-BANTI    =   SPACE)
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "12"                TO  TBL-MRCSV-DATA (1)
               MOVE    PTINF-HOME-POST     TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-HOME-ADRS     TO  TBL-MRCSV-DATA (3)
               MOVE    PTINF-HOME-BANTI    TO  TBL-MRCSV-DATA (4)
               MOVE    4                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    患者連絡先レコード
           IF     (PTINF-HOME-TEL1     =   SPACE)  AND
                  (PTINF-KEITAI-TEL    =   SPACE)
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "13"                TO  TBL-MRCSV-DATA (1)
               MOVE    PTINF-HOME-TEL1     TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-KEITAI-TEL    TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    患者勤務先レコード
           IF     (PTINF-OFFICE-NAME   =   SPACE)  AND
                  (PTINF-OFFICE-TEL    =   SPACE)
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "14"                TO  TBL-MRCSV-DATA (1)
               MOVE    PTINF-OFFICE-NAME   TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-OFFICE-TEL    TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    患者地域連携情報レコード
           IF     (PTPUBLIC-PATIENT-ID1
                                       =   SPACE)  AND
                  (PTPUBLIC-AGREEMENT  =   SPACE)
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "15"                TO  TBL-MRCSV-DATA (1)
      *        将来のマイナンバーのための項目
               MOVE    SPACE               TO  TBL-MRCSV-DATA (2)
               MOVE    PTPUBLIC-PATIENT-ID1
                                           TO  TBL-MRCSV-DATA (3)
               MOVE    PTPUBLIC-AGREEMENT  TO  TBL-MRCSV-DATA (4)
               MOVE    4                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    患者コメントレコード
           IF      PTINF-ALLERGY-1     =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "18"                TO  TBL-MRCSV-DATA (1)
               MOVE    "1"                 TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-ALLERGY-1     TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           IF      PTINF-ALLERGY-2     =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "18"                TO  TBL-MRCSV-DATA (1)
               MOVE    "1"                 TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-ALLERGY-2     TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           IF      PTINF-TABOO-1       =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "18"                TO  TBL-MRCSV-DATA (1)
               MOVE    "2"                 TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-TABOO-1       TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           IF      PTINF-TABOO-2       =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "18"                TO  TBL-MRCSV-DATA (1)
               MOVE    "2"                 TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-TABOO-2       TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           IF      PTINF-KANSENSYO-1   =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "18"                TO  TBL-MRCSV-DATA (1)
               MOVE    "3"                 TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-KANSENSYO-1   TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           IF      PTINF-KANSENSYO-2   =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "18"                TO  TBL-MRCSV-DATA (1)
               MOVE    "3"                 TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-KANSENSYO-2   TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           IF      PTINF-COMMENT-1     =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "18"                TO  TBL-MRCSV-DATA (1)
               MOVE    "4"                 TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-COMMENT-1     TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           IF      PTINF-COMMENT-2     =   SPACE
               CONTINUE
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "18"                TO  TBL-MRCSV-DATA (1)
               MOVE    "4"                 TO  TBL-MRCSV-DATA (2)
               MOVE    PTINF-COMMENT-2     TO  TBL-MRCSV-DATA (3)
               MOVE    3                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
      *    患者医薬品禁忌情報出力処理
           PERFORM 31021-OUTPUT-PTKINKI-SEC
      *
      *    患者病名情報出力処理
      *    2013.5 松江会議にてデフォルトは出力しないとする
      *    オプションにより出力可能
           IF      POPT-BYOMEI         =   "1"
               PERFORM 31022-OUTPUT-PTBYOMEI-SEC
           END-IF
      *
      *    診療年月日レコード
           MOVE    SPACE               TO  TBL-MRCSV-REC
           MOVE    "101"               TO  TBL-MRCSV-DATA (1)
           MOVE    SPA-SRYYMD          TO  TBL-MRCSV-DATA (2)
           MOVE    TBL-NAIYOU-MAX      TO  WRK-SURYO
           PERFORM 3109-SURYO-HEN-SEC
           MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (3)
           MOVE    3                   TO  TBL-MRCSV-MAX
           PERFORM 400-DATAOUT-SEC
           .
       3102-OUTPUT-PT-KIHON-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者医薬品禁忌情報出力処理
      *****************************************************************
       31021-OUTPUT-PTKINKI-SEC    SECTION.
      *
           INITIALIZE  PTKINKI-REC
           MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
           MOVE    SPA-PTID            TO  PTKINKI-PTID
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
      *        患者医薬品禁忌検索
               PERFORM 900-PTKINKI-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-PTKINKI
               INITIALIZE  PTKINKI-REC
           END-IF
      *
           PERFORM UNTIL FLG-PTKINKI   =   1
      *        点数マスタ検索
               MOVE    PTKINKI-SRYCD       TO  WRK-SRYCD
               PERFORM 900-TENSU-READ-SEC
      *
      *        患者医薬品禁忌レコード
               IF     (PTKINKI-SRYCD       =   SPACE)  AND
                      (TNS-NAME            =   SPACE)
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-REC
                   MOVE    "19"                TO  TBL-MRCSV-DATA (1)
                   MOVE    "1"                 TO  TBL-MRCSV-DATA (2)
                   MOVE    PTKINKI-SRYCD       TO  TBL-MRCSV-DATA (3)
                   MOVE    TNS-NAME            TO  TBL-MRCSV-DATA (4)
                   MOVE    4                   TO  TBL-MRCSV-MAX
                   PERFORM 400-DATAOUT-SEC
               END-IF
      *        患者医薬品禁忌検索
               PERFORM 900-PTKINKI-FETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31021-OUTPUT-PTKINKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名情報出力処理
      *****************************************************************
       31022-OUTPUT-PTBYOMEI-SEC   SECTION.
      *
           INITIALIZE  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-SRYYMD          TO  PTBYO-SRYYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
      *        患者病名検索
               PERFORM 900-PTBYOMEI-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE  PTBYOMEI-REC
           END-IF
      *
           PERFORM UNTIL FLG-PTBYOMEI  =   1
      *        患者病名レコード
               IF     (PTBYO-BYOMEI        =   SPACE)  AND
                      (PTBYO-HOSOKU-COMMENT
                                           =   SPACE)  AND
                      (PTBYO-SRYYMD        =   SPACE)  AND
                      (PTBYO-TENKIKBN      =   SPACE)  AND
                      (PTBYO-SYUBYOFLG     =   SPACE)
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-REC
                   MOVE    "51"                TO  TBL-MRCSV-DATA (1)
                   MOVE    PTBYO-BYOMEI        TO  TBL-MRCSV-DATA (2)
                   MOVE    PTBYO-HOSOKU-COMMENT
                                               TO  TBL-MRCSV-DATA (3)
                   MOVE    PTBYO-SRYYMD        TO  TBL-MRCSV-DATA (4)
                   MOVE    PTBYO-TENKIKBN      TO  TBL-MRCSV-DATA (5)
                   MOVE    PTBYO-SYUBYOFLG     TO  TBL-MRCSV-DATA (6)
                   MOVE    6                   TO  TBL-MRCSV-MAX
                   PERFORM 400-DATAOUT-SEC
               END-IF
      *        患者病名検索
               PERFORM 900-PTBYOMEI-FETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31022-OUTPUT-PTBYOMEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細情報出力処理
      *****************************************************************
       3103-OUTPUT-DT-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-KEY
      *
           PERFORM VARYING IDX-J   FROM    1   BY  1
                   UNTIL ( TBL-NAIYOU-KEY(IDX-J)   =   SPACE )
               IF    ( TBL-SRYKA(IDX-J)    NOT =   WRK-SRYKA ) OR
                     ( TBL-DRCD(IDX-J)     NOT =   WRK-DRCD )
                   MOVE    TBL-SRYKA(IDX-J)    TO  WRK-SRYKA
                   MOVE    TBL-DRCD(IDX-J)     TO  WRK-DRCD
                   MOVE    SPACE               TO  WRK-HKNCOMBINUM
                   MOVE    ZERO                TO  WRK-ZAINUM-SAVE
                   MOVE    ZERO                TO  WRK-ZAISEQ
      *            診療科・医師情報出力処理
                   PERFORM 31031-OUTPUT-SRYKA-SEC
               END-IF
               IF    ( TBL-HKNCOMBINUM(IDX-J)  
                                           NOT =   WRK-HKNCOMBINUM )
                   MOVE    TBL-HKNCOMBINUM(IDX-J)
                                               TO  WRK-HKNCOMBINUM
                   MOVE    ZERO                TO  WRK-ZAINUM-SAVE
                   MOVE    ZERO                TO  WRK-ZAISEQ
      *            保険情報出力処理
                   PERFORM 31032-OUTPUT-HKNKBN-SEC
               END-IF
               PERFORM VARYING IDX-S   FROM    1   BY  1
                       UNTIL ( TBL-SRYCD(IDX-J IDX-S)  =   SPACE )
                   MOVE    TBL-NAIYOU-TBL(IDX-J IDX-S)  TO  WRK-TBL
                   IF      WRK-ZAINUM      NOT =   WRK-ZAINUM-SAVE
                       MOVE    WRK-ZAINUM          TO  WRK-ZAINUM-SAVE
                       COMPUTE WRK-ZAISEQ          =   WRK-ZAISEQ  +   1
                       MOVE    1                   TO  FLG-SRYKBN
                       MOVE    ZERO                TO  FLG-IYKTOP
                   ELSE
                       MOVE    ZERO                TO  FLG-SRYKBN
                   END-IF
      *            在宅・投薬
                   IF     (WRK-SRYKBN          =   "14"
                                               OR  "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "98" )  AND
                         ((WRK-SRYCD(1:1)      =   "6"  OR  "7")   OR
                         ((FLG-IYKTOP          =   1)   AND
                          (WRK-SRYCD(1:1)      =   "8"  OR  "0")))
      *                医薬品情報出力処理
                       PERFORM 31034-OUTPUT-IYKDATA-SEC
                   ELSE
      *                診療情報出力処理
                       PERFORM 31033-OUTPUT-SRYDATA-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       3103-OUTPUT-DT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科・医師情報出力処理
      *****************************************************************
       31031-OUTPUT-SRYKA-SEC      SECTION.
      *
      *    診療科情報
           INITIALIZE  SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           END-IF
      *
      *    職員情報
           INITIALIZE  SYSKANRI-REC
           MOVE    "1010"              TO  SYS-KANRICD
           MOVE    WRK-DRCD            TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
           END-IF
      *
      *    診療科レコード
           MOVE    SPACE               TO  TBL-MRCSV-REC
           MOVE    "105"               TO  TBL-MRCSV-DATA (1)
           IF      SYS-1005-KBNCD      =   SPACE
               MOVE    "1"                 TO  TBL-MRCSV-DATA (2)
           ELSE
               MOVE    "2"                 TO  TBL-MRCSV-DATA (2)
           END-IF
           MOVE    SYS-1005-KBNCD      TO  TBL-MRCSV-DATA (3)
           MOVE    SYS-1005-SRYKANAME  TO  TBL-MRCSV-DATA (4)
           MOVE    4                   TO  TBL-MRCSV-MAX
           PERFORM 400-DATAOUT-SEC
      *
      *    医師レコード
           MOVE    SPACE               TO  TBL-MRCSV-REC
           MOVE    "107"               TO  TBL-MRCSV-DATA (1)
           MOVE    SPACE               TO  TBL-MRCSV-DATA (2)
           MOVE    SYS-1010-NAME       TO  TBL-MRCSV-DATA (3)
           MOVE    SYS-1010-KANANAME   TO  TBL-MRCSV-DATA (4)
           MOVE    4                   TO  TBL-MRCSV-MAX
           PERFORM 400-DATAOUT-SEC
           .
       31031-OUTPUT-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報出力処理
      *****************************************************************
       31032-OUTPUT-HKNKBN-SEC     SECTION.
      *
      *    保険組合せ検索
           PERFORM 900-HKNCOMBI-READ-SEC
      *    患者保険検索
           PERFORM 900-PTHKNINF-READ-SEC
      *
      *    患者保険レコード
           MOVE    SPACE               TO  TBL-MRCSV-REC
           MOVE    "21"                TO  TBL-MRCSV-DATA (1)
           MOVE    PTHKN-HKNJANUM      TO  TBL-MRCSV-DATA (2)
           MOVE    PTHKN-KIGO          TO  TBL-MRCSV-DATA (3)
           MOVE    PTHKN-NUM           TO  TBL-MRCSV-DATA (4)
           MOVE    PTHKN-HONKZKKBN     TO  TBL-MRCSV-DATA (5)
           MOVE    PTHKN-SKKGETYMD     TO  TBL-MRCSV-DATA (6)
           MOVE    "30"                TO  TBL-MRCSV-DATA (7)
           MOVE    "30"                TO  TBL-MRCSV-DATA (8)
           MOVE    PTHKN-CONTKBN       TO  TBL-MRCSV-DATA (10)
      *
           EVALUATE    PTHKN-HOJOKBN
               WHEN    "0"
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "060"
                       WHEN    "069"
                           MOVE    "0"     TO  TBL-MRCSV-DATA (7)
                           MOVE    "0"     TO  TBL-MRCSV-DATA (8)
                   END-EVALUATE
               WHEN    "1"
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002"
                           MOVE    "1"     TO  TBL-MRCSV-DATA (9)
                       WHEN    "980"
                           MOVE   "JIHI1"  TO  TBL-MRCSV-DATA (2)
                       WHEN    "060"
                       WHEN    "069"
                           MOVE    "10"    TO  TBL-MRCSV-DATA (7)
                           MOVE    "10"    TO  TBL-MRCSV-DATA (8)
                   END-EVALUATE
               WHEN    "2"
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002"
                           MOVE    "2"     TO  TBL-MRCSV-DATA (9)
                       WHEN    "980"
                           MOVE   "JIHI2"  TO  TBL-MRCSV-DATA (2)
                       WHEN    "060"
                       WHEN    "069"
                           MOVE    "20"    TO  TBL-MRCSV-DATA (7)
                           MOVE    "20"    TO  TBL-MRCSV-DATA (8)
                   END-EVALUATE
               WHEN    "3"
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002"
                           MOVE    "3"     TO  TBL-MRCSV-DATA (9)
                       WHEN    "060"
                       WHEN    "069"
                           MOVE    "30"    TO  TBL-MRCSV-DATA (7)
                           MOVE    "30"    TO  TBL-MRCSV-DATA (8)
                   END-EVALUATE
               WHEN    "4"
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "060"
                       WHEN    "069"
                           MOVE    "10"    TO  TBL-MRCSV-DATA (7)
                           MOVE    "0"     TO  TBL-MRCSV-DATA (8)
                   END-EVALUATE
               WHEN    "5"
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "060"
                       WHEN    "069"
                           MOVE    "20"    TO  TBL-MRCSV-DATA (7)
                           MOVE    "10"    TO  TBL-MRCSV-DATA (8)
                   END-EVALUATE
               WHEN    "6"
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "060"
                       WHEN    "069"
                           MOVE    "30"    TO  TBL-MRCSV-DATA (7)
                           MOVE    "20"    TO  TBL-MRCSV-DATA (8)
                   END-EVALUATE
               WHEN    "7"
                   MOVE    "30"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "30"        TO  TBL-MRCSV-DATA (8)
               WHEN    "8"
                   MOVE    "20"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "20"        TO  TBL-MRCSV-DATA (8)
               WHEN    "9"
                   MOVE    "10"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "10"        TO  TBL-MRCSV-DATA (8)
               WHEN    "A"
                   MOVE    "10"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "10"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "1"         TO  TBL-MRCSV-DATA (9)
               WHEN    "B"
                   MOVE    "10"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "10"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "2"         TO  TBL-MRCSV-DATA (9)
               WHEN    "C"
                   MOVE    "10"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "10"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "3"         TO  TBL-MRCSV-DATA (9)
               WHEN    "D"
                   MOVE    "20"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "20"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "1"         TO  TBL-MRCSV-DATA (9)
               WHEN    "E"
                   MOVE    "20"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "20"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "2"         TO  TBL-MRCSV-DATA (9)
               WHEN    "F"
                   MOVE    "20"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "20"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "3"         TO  TBL-MRCSV-DATA (9)
               WHEN    "G"
                   MOVE    "30"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "30"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "1"         TO  TBL-MRCSV-DATA (9)
               WHEN    "H"
                   MOVE    "30"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "30"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "2"         TO  TBL-MRCSV-DATA (9)
               WHEN    "I"
                   MOVE    "30"        TO  TBL-MRCSV-DATA (7)
                   MOVE    "30"        TO  TBL-MRCSV-DATA (8)
                   MOVE    "3"         TO  TBL-MRCSV-DATA (9)
           END-EVALUATE
      *
           MOVE    10                  TO  TBL-MRCSV-MAX
           IF     (TBL-MRCSV-DATA(2)   =   SPACE)  AND
                  (TBL-MRCSV-DATA(3)   =   SPACE)  AND
                  (TBL-MRCSV-DATA(4)   =   SPACE)  AND
                  (TBL-MRCSV-DATA(5)   =   SPACE)  AND
                  (TBL-MRCSV-DATA(6)   =   SPACE)
               CONTINUE
           ELSE
               PERFORM 400-DATAOUT-SEC
           END-IF
      *
           PERFORM VARYING IDX-KOH     FROM    1   BY 1
                   UNTIL ( IDX-KOH     >   4 )
                   OR    ( COMB-KOHID(IDX-KOH) =   ZERO )
      *        患者公費検索
               PERFORM 900-PTKOHINF-READ-SEC
      *
      *        患者公費レコード
               IF     (PTKOH-FTNJANUM      =   SPACE)  AND
                      (PTKOH-JKYSNUM       =   SPACE)
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-REC
                   MOVE    "24"                TO  TBL-MRCSV-DATA (1)
                   MOVE    PTKOH-FTNJANUM      TO  TBL-MRCSV-DATA (2)
                   MOVE    PTKOH-JKYSNUM       TO  TBL-MRCSV-DATA (3)
                   MOVE    3                   TO  TBL-MRCSV-MAX
                   PERFORM 400-DATAOUT-SEC
               END-IF
           END-PERFORM
           .
       31032-OUTPUT-HKNKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報出力処理
      *****************************************************************
       31033-OUTPUT-SRYDATA-SEC    SECTION.
      *
      *    診療行為レコード
           IF    ( WRK-SRYCD       >=  "100000000" )   AND
                 ( WRK-SRYCD       <=  "199999999" )
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "121"               TO  TBL-MRCSV-DATA (1)
               MOVE    WRK-ZAISEQ          TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (2)
               PERFORM 31035-SRYKBN-HEN-SEC
               MOVE    SPACE               TO  TBL-MRCSV-DATA (5)
               MOVE    WRK-SRYCD           TO  TBL-MRCSV-DATA (6)
               MOVE    WRK-NAME            TO  TBL-MRCSV-DATA (7)
               IF      WRK-KZMCOMPSIKIBETU =   1
                   MOVE    WRK-SRYSURYO        TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (8)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (8)
               END-IF
               MOVE    WRK-TANINAME        TO  TBL-MRCSV-DATA (9)
               IF      WRK-TEN             >   ZERO
                   MOVE    WRK-TEN             TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (10)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (10)
               END-IF
               IF      WRK-KAISU           >   ZERO
                   MOVE    WRK-KAISU           TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (11)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (11)
               END-IF
               MOVE    11                  TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    医薬品レコード
           IF    ( WRK-SRYCD       >=  "600000000" )   AND
                 ( WRK-SRYCD       <=  "699999999" )
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "141"               TO  TBL-MRCSV-DATA (1)
               MOVE    WRK-ZAISEQ          TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (2)
               PERFORM 31035-SRYKBN-HEN-SEC
               MOVE    SPACE               TO  TBL-MRCSV-DATA (5)
               MOVE    WRK-SRYCD           TO  TBL-MRCSV-DATA (6)
               MOVE    WRK-NAME            TO  TBL-MRCSV-DATA (7)
               MOVE    WRK-SRYSURYO        TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (8)
               MOVE    WRK-TANINAME        TO  TBL-MRCSV-DATA (9)
               IF      WRK-TEN             >   ZERO
                   MOVE    WRK-TEN             TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (10)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (10)
               END-IF
               IF      WRK-KAISU           >   ZERO
                   MOVE    WRK-KAISU           TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (11)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (11)
               END-IF
               MOVE    11                  TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    特定器材レコード
           IF    ( WRK-SRYCD       >=  "700000000" )   AND
                 ( WRK-SRYCD       <=  "799999999" )
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "161"               TO  TBL-MRCSV-DATA (1)
               MOVE    WRK-ZAISEQ          TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (2)
               PERFORM 31035-SRYKBN-HEN-SEC
               MOVE    WRK-SRYCD           TO  TBL-MRCSV-DATA (5)
               MOVE    WRK-NAME            TO  TBL-MRCSV-DATA (6)
               MOVE    WRK-SRYSURYO        TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (7)
               MOVE    WRK-TANICD          TO  TBL-MRCSV-DATA (8)
               MOVE    WRK-TANINAME        TO  TBL-MRCSV-DATA (9)
               MOVE    WRK-SRYKAISU        TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (10)
               IF      WRK-TEN             >   ZERO
                   MOVE    WRK-TEN             TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (11)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (11)
               END-IF
               IF      WRK-KAISU           >   ZERO
                   MOVE    WRK-KAISU           TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (12)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (12)
               END-IF
               MOVE    12                  TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
      *    コメントコード
           IF   (( WRK-SRYCD       >=  "002000000" )   AND
                 ( WRK-SRYCD       <=  "002999999" ))  OR
                (( WRK-SRYCD       >=  "008200000" )   AND
                 ( WRK-SRYCD       <=  "008699999" ))  OR
                (( WRK-SRYCD       >=  "800000000" )   AND
                 ( WRK-SRYCD       <=  "899999999" ))
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "181"               TO  TBL-MRCSV-DATA (1)
               MOVE    WRK-ZAISEQ          TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (2)
               PERFORM 31035-SRYKBN-HEN-SEC
               IF     (WRK-SRYCD          >=   "002000000")  AND
                      (WRK-SRYCD          <=   "002999999")
                   MOVE    "2"                 TO  TBL-MRCSV-DATA (5)
                   EVALUATE    WRK-BUIKBN
                       WHEN    1
                           MOVE    "002000001"     TO  TBL-MRCSV-DATA(6)
                       WHEN    2
                           MOVE    "002000002"     TO  TBL-MRCSV-DATA(6)
                       WHEN    3
                           MOVE    "002000003"     TO  TBL-MRCSV-DATA(6)
                       WHEN    5
                           MOVE    "002000005"     TO  TBL-MRCSV-DATA(6)
                       WHEN    6
                           MOVE    "002000006"     TO  TBL-MRCSV-DATA(6)
                       WHEN    7
                           MOVE    "002000007"     TO  TBL-MRCSV-DATA(6)
                       WHEN    8
                           MOVE    "002000008"     TO  TBL-MRCSV-DATA(6)
                       WHEN    OTHER
                           MOVE    "002000000"     TO  TBL-MRCSV-DATA(6)
                   END-EVALUATE
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (5)
                   MOVE    WRK-SRYCD           TO  TBL-MRCSV-DATA (6)
               END-IF
               MOVE    WRK-NAME            TO  TBL-MRCSV-DATA (7)
               MOVE    WRK-KANRENFLG       TO  TBL-MRCSV-DATA (8)
               MOVE    SPACE               TO  TBL-MRCSV-DATA (9)
               MOVE    9                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           .
       31033-OUTPUT-SRYDATA-EXT.
           EXIT.
      *
      *****************************************************************
      *    医薬品情報出力処理
      *****************************************************************
       31034-OUTPUT-IYKDATA-SEC    SECTION.
      *
      *    医薬品レコード
      *    サンプル薬レコード（サ）は対象
           IF    ( WRK-SRYCD(1:1)  =   "6" )   OR
                 ( WRK-SRYCD(1:1)  =   "7" )   OR
                 ( WRK-SRYCD(1:3)  =   "059" ) OR
                 ( WRK-SRYCD       =   "820000046" ) OR
                 ( WRK-SRYCD       =   "820000047" )
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "141"               TO  TBL-MRCSV-DATA (1)
               MOVE    WRK-ZAISEQ          TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (2)
               PERFORM 31035-SRYKBN-HEN-SEC
               IF      WRK-GENERICCD   NOT =   SPACE
                   MOVE    "2"                 TO  TBL-MRCSV-DATA (5)
                   MOVE    WRK-GENERICCD       TO  TBL-MRCSV-DATA (6)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (5)
                   MOVE    WRK-SRYCD           TO  TBL-MRCSV-DATA (6)
               END-IF
               MOVE    WRK-NAME            TO  TBL-MRCSV-DATA (7)
               MOVE    WRK-SRYSURYO        TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (8)
               MOVE    WRK-TANINAME        TO  TBL-MRCSV-DATA (9)
               IF      WRK-TEN             >   ZERO
                   MOVE    WRK-TEN             TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (10)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (10)
               END-IF
               IF      WRK-KAISU           >   ZERO
                   MOVE    WRK-KAISU           TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (11)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (11)
               END-IF
               MOVE    11                  TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
               MOVE    1                   TO  FLG-IYKTOP
           END-IF
      *
      *    医薬品補足レコード
      *    サンプル薬レコード（サ）は対象外
           IF   (( WRK-SRYCD(1:1)  =   "8" )   AND
                 ( WRK-SRYCD   NOT =   "820000046" )   AND
                 ( WRK-SRYCD   NOT =   "820000047" ))  OR
                 ( WRK-SRYCD(1:3)  =   "008" ) OR
                (( WRK-SRYCD(1:3)  =   "001" ) AND
                 ( WRK-YCOMMNTKBN  =   "2" )) OR
                 ( WRK-SRYCD       =   "099200011" ) OR
                 ( WRK-SRYCD       =   "099200012" ) OR
                 ( WRK-SRYCD       =   "099200013" ) OR
                 ( WRK-SRYCD       =   "099200014" ) OR
                 ( WRK-SRYCD       =   "099200015" ) OR
                 ( WRK-SRYCD       =   "099209903" ) OR
                 ( WRK-SRYCD       =   "099209905" ) OR
                 ( WRK-SRYCD       =   "099209906" )
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "213"               TO  TBL-MRCSV-DATA (1)
               MOVE    WRK-ZAISEQ          TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (2)
               EVALUATE    WRK-SRYCD
                   WHEN    "099209903"
                       MOVE    "3"         TO  TBL-MRCSV-DATA (3)
                       MOVE    "後発品変更不可"
                                           TO  TBL-MRCSV-DATA (4)
                   WHEN    "099209905"
                       MOVE    "5"         TO  TBL-MRCSV-DATA (3)
                       MOVE    "含量規格変更不可"
                                           TO  TBL-MRCSV-DATA (4)
                   WHEN    "099209906"
                       MOVE    "4"         TO  TBL-MRCSV-DATA (3)
                       MOVE    "剤形変更不可"
                                           TO  TBL-MRCSV-DATA (4)
                   WHEN    OTHER
                       MOVE    SPACE       TO  TBL-MRCSV-DATA (3)
                       MOVE    WRK-NAME    TO  TBL-MRCSV-DATA (4)
               END-EVALUATE
               MOVE    4                   TO  TBL-MRCSV-MAX
               IF     (TBL-MRCSV-DATA(3)   =   SPACE)  AND
                      (TBL-MRCSV-DATA(4)   =   SPACE)
                   CONTINUE
               ELSE
                   PERFORM 400-DATAOUT-SEC
               END-IF
           END-IF
      *
           IF    ( WRK-SRYCD       >=  "001000000" )   AND
                 ( WRK-SRYCD       <=  "001999999" )
      *        用法レコード
               MOVE    SPACE               TO  TBL-MRCSV-REC
               MOVE    "221"               TO  TBL-MRCSV-DATA (1)
               MOVE    WRK-ZAISEQ          TO  WRK-SURYO
               PERFORM 3109-SURYO-HEN-SEC
               MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (2)
               MOVE    WRK-NAME            TO  TBL-MRCSV-DATA (3)
               IF      WRK-TEN             >   ZERO
                   MOVE    WRK-TEN             TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (4)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (4)
               END-IF
               IF      WRK-KAISU           >   ZERO
                   MOVE    WRK-KAISU           TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (5)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (5)
               END-IF
               PERFORM 31036-CYOZAITANI-HEN-SEC
      *        剤型コード
               IF      WRK-ZAIGATACD   NOT =   ZERO
                   MOVE    WRK-ZAIGATACD       TO  WRK-SURYO
                   PERFORM 3109-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-H         TO  TBL-MRCSV-DATA (7)
               ELSE
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (7)
               END-IF
      *        用法コード種別（コードなし）
               MOVE    "1"                 TO  TBL-MRCSV-DATA (8)
      *        用法コード（用法コード種別がコードなしのため省略）
               MOVE    SPACE               TO  TBL-MRCSV-DATA (9)
               MOVE    9                   TO  TBL-MRCSV-MAX
               PERFORM 400-DATAOUT-SEC
           END-IF
           .
       31034-OUTPUT-IYKDATA-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分編集処理
      *****************************************************************
       31035-SRYKBN-HEN-SEC        SECTION.
      *
           IF      FLG-SRYKBN          =   1
               MOVE    WRK-SRYKBN          TO  TBL-MRCSV-DATA (3)
               IF      WRK-JIHOKBN         =   "1"
                   MOVE    "1"                 TO  TBL-MRCSV-DATA (4)
               ELSE
                   IF      WRK-INGAIKBN        =   1
                       MOVE    "2"                 TO
                                                   TBL-MRCSV-DATA (4)
                   ELSE
                       MOVE    SPACE               TO
                                                   TBL-MRCSV-DATA (4)
                   END-IF
               END-IF
               MOVE    ZERO                TO  FLG-SRYKBN
           ELSE
               MOVE    SPACE               TO  TBL-MRCSV-DATA (3)
               MOVE    SPACE               TO  TBL-MRCSV-DATA (4)
           END-IF
           .
       31035-SRYKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    調剤単位編集処理
      *****************************************************************
       31036-CYOZAITANI-HEN-SEC    SECTION.
      *
           EVALUATE    WRK-ZAIGATACD
               WHEN    1
               WHEN    6
               WHEN    7
                   MOVE    "日分"              TO  TBL-MRCSV-DATA (6)
               WHEN    2
               WHEN    4
               WHEN    5
               WHEN    9
               WHEN    10
                   MOVE    "調剤"              TO  TBL-MRCSV-DATA (6)
               WHEN    3
                   MOVE    "回分"              TO  TBL-MRCSV-DATA (6)
               WHEN    OTHER
                   MOVE    SPACE               TO  TBL-MRCSV-DATA (6)
           END-EVALUATE
           .
       31036-CYOZAITANI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       3109-SURYO-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING IDX-M   FROM    5   BY  -1
                   UNTIL   IDX-M   <   1
               IF      WRK-SURYO-S2(IDX-M:1)
                                       NOT =   ZERO
                   MOVE    1                   TO  FLG-SP
               END-IF
               IF      FLG-SP              =   1
                   MOVE    WRK-SURYO-S2(IDX-M:1)
                                               TO  WRK-SURYO-Z3
                                                   (IDX-M:1)
               END-IF
           END-PERFORM
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *
           MOVE    SPACE               TO  WRK-SURYO-H
           PERFORM VARYING IDX-M   FROM    1   BY  1
                   UNTIL   IDX-M   >   15
               IF      WRK-SURYO-X(IDX-M:1)
                                           NOT =   SPACE
                   MOVE    WRK-SURYO-X(IDX-M:) TO  WRK-SURYO-H
                   MOVE    15                  TO  IDX-M
               END-IF
           END-PERFORM
           .
       3109-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ出力処理
      *****************************************************************
       400-DATAOUT-SEC             SECTION.
      *
           INITIALIZE  WRK-POINT-AREA
           MOVE    SPACE               TO  MRCSV-REC
           INITIALIZE  MRCSV-REC
      *
           PERFORM VARYING IDX-K   FROM    1   BY  1
                   UNTIL ( IDX-K   >   TBL-MRCSV-MAX )
                   OR    ( WRK-PO-ED   >=  1000 )
      *
               COMPUTE WRK-PO-ST           =   WRK-PO-ED   +   1
      *
               MOVE    TBL-MRCSV-DATA (IDX-K)
                                           TO  WRK-DATA
      *        項目別編集処理
               PERFORM 4001-KOUMOKU-HEN-SEC
               MOVE    WRK-PO-ST           TO  WRK-PO-ED
           END-PERFORM
      *
      *    診療情報データ出力
           WRITE   MRCSV-REC
           COMPUTE CNT-OUTPUT          =   CNT-OUTPUT  +   1
           .
       400-DATAOUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目別編集処理
      *****************************************************************
       4001-KOUMOKU-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT
           PERFORM VARYING IDX-D   FROM    140 BY  -1
                   UNTIL ( IDX-D   <   1 )
      *        半角空白以外
               IF      WRK-DATA (IDX-D:1)
                                       NOT =   SPACE
                   MOVE    IDX-D               TO  WRK-CNT
                   MOVE    1                   TO  IDX-D
               END-IF
           END-PERFORM
      *
           IF      WRK-CNT             >   0
               MOVE    WRK-DATA (1:WRK-CNT)
                                       TO  MRCSV-REC(WRK-PO-ST:WRK-CNT)
               COMPUTE WRK-PO-ST           =   WRK-PO-ST   +   WRK-CNT
           END-IF
      *
      *    最終項目以外に , 追加
           IF      IDX-K               <   TBL-MRCSV-MAX
               MOVE    ","                 TO  MRCSV-REC(WRK-PO-ST:1)
           END-IF
           .
       4001-KOUMOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-H2Z-AFTER
           MOVE    ZERO                TO  WRK-H2Z-MAX
      *
           INITIALIZE  ORCXKANACONVAREA
           MOVE    40                  TO  KANACONV-LEN
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    1                   TO  KANACONV-CONV-FLAG
           MOVE    15                  TO  KANACONV-CHAR-TYPE
           MOVE    WRK-H2Z-BEFORE      TO  KANACONV-INDATA
           CALL    "kanaconv"      USING   ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
               MOVE    KANACONV-OUTDATA
                                       TO  WRK-H2Z-AFTER
               MOVE    KANACONV-MAXLEN TO  WRK-H2Z-MAX
           END-IF
           .
       800-ZENKAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SPA-SRYYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   INITIALIZE  SYSKANRI-REC
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   INITIALIZE  PTINF-REC
                   MOVE    1                   TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE  PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号読込
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           INITIALIZE  PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTNUM
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
               ELSE
                   INITIALIZE  PTNUM-REC
                   MOVE    1                   TO  FLG-PTNUM
               END-IF
           ELSE
               INITIALIZE  PTNUM-REC
               MOVE    1                   TO  FLG-PTNUM
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者地域連携読込
      *****************************************************************
       900-PTPUBLIC-READ-SEC       SECTION.
      *
           INITIALIZE  PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    SPA-PTID            TO  PTPUBLIC-PTID
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTPUBLIC
                   MOVE    MCPDATA-REC         TO  PTPUBLIC-REC
               ELSE
                   INITIALIZE  PTPUBLIC-REC
                   MOVE    1               TO  FLG-PTPUBLIC
               END-IF
           ELSE
               INITIALIZE  PTPUBLIC-REC
               MOVE    1                   TO  FLG-PTPUBLIC
           END-IF
      *
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTPUBLIC-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC       SECTION.
      *
           INITIALIZE  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
               ELSE
                   INITIALIZE  HKNCOMBI-REC
                   MOVE    1                   TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE  HKNCOMBI-REC
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報読込
      *****************************************************************
       900-PTHKNINF-READ-SEC       SECTION.
      *
           INITIALIZE  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
               ELSE
                   INITIALIZE  PTHKNINF-REC
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               INITIALIZE  PTHKNINF-REC
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-SEC       SECTION.
      *
           INITIALIZE  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    COMB-KOHID(IDX-KOH) TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               ELSE
                   INITIALIZE  PTKOHINF-REC
                   MOVE    1                   TO  FLG-PTKOHINF
               END-IF
           ELSE
               INITIALIZE  PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災読込
      *****************************************************************
       900-PTRSIINF-READ-SEC       SECTION.
      *
           INITIALIZE  PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-PTID            TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
               ELSE
                   INITIALIZE  PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
           ELSE
               INITIALIZE  PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者医薬品禁忌読込
      *****************************************************************
       900-PTKINKI-FETCH-SEC       SECTION.
      *
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTKINKI
               MOVE    MCPDATA-REC         TO  PTKINKI-REC
           ELSE
               MOVE    1                   TO  FLG-PTKINKI
               INITIALIZE  PTKINKI-REC
           END-IF
           .
       900-PTKINKI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名読込
      *****************************************************************
       900-PTBYOMEI-FETCH-SEC      SECTION.
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC         TO  PTBYOMEI-REC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               INITIALIZE  PTBYOMEI-REC
           END-IF
           .
       900-PTBYOMEI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴読込
      *****************************************************************
       900-JYURRK-FETCH-SEC        SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  JYURRK-REC
           ELSE
               MOVE    1                   TO  FLG-END
               INITIALIZE  JYURRK-REC
           END-IF
           .
       900-JYURRK-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-FETCH-SEC        SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACT
               MOVE    MCPDATA-REC         TO  SRYACT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
               INITIALIZE  SRYACT-REC
           END-IF
           .
       900-SRYACT-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読込処理
      *****************************************************************
       900-SRYACCT-READ-SEC        SECTION.
      *
           INITIALIZE  SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SRYACCT
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE  SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       900-PTCOM-READ-SEC          SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD(IDX-SRY)  TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX-SRY)
                                       TO  PTCOM-RENNUM
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-TENSU
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   IF      TNS-SRYCD(1:1)      =   "6" OR  "7"
                       INITIALIZE  TNSUP-KEY
                       MOVE    SPA-SRYYMD          TO  TNSUP-YUKOSTYMD
                                                       TNSUP-YUKOEDYMD
                       CALL    "ORCSSETTANI"   USING   TNS-TENSU-REC
                                                       SPA-AREA
                   END-IF
               ELSE
                   INITIALIZE  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       900-TENSUPLUS-READ-SEC      SECTION.
      *
           INITIALIZE  TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-TENSUPLUS
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
               ELSE
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE  TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE  TENSUPLUS-REC
           END-IF
      *
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       900-OPTION-READ-SEC         SECTION.
      *
           INITIALIZE  POPT-AREA
      *    病名出力
           MOVE    "BYOMEI"            TO  POPT-NAME
           PERFORM 9001-OPTION-CALL-SEC
           MOVE    POPT-DATA(1:1)      TO  POPT-BYOMEI
      *
      *    診療区分出力
           MOVE    "SRYKBN10"          TO  POPT-NAME
           PERFORM 9001-OPTION-CALL-SEC
           IF      POPT-DATA           =   SPACE
               MOVE    CONST-SRYKBN10      TO  POPT-DATA
           END-IF
           MOVE    POPT-DATA(1:4)      TO  POPT-SRYKBN-TBL (1)
           MOVE    POPT-DATA(6:4)      TO  POPT-SRYKBN-TBL (2)
           MOVE    POPT-DATA(11:4)     TO  POPT-SRYKBN-TBL (3)
           MOVE    POPT-DATA(16:4)     TO  POPT-SRYKBN-TBL (4)
      *
           MOVE    "SRYKBN20"          TO  POPT-NAME
           PERFORM 9001-OPTION-CALL-SEC
           IF      POPT-DATA           =   SPACE
               MOVE    CONST-SRYKBN20      TO  POPT-DATA
           END-IF
           MOVE    POPT-DATA(1:4)      TO  POPT-SRYKBN-TBL (5)
           MOVE    POPT-DATA(6:4)      TO  POPT-SRYKBN-TBL (6)
           MOVE    POPT-DATA(11:4)     TO  POPT-SRYKBN-TBL (7)
           MOVE    POPT-DATA(16:4)     TO  POPT-SRYKBN-TBL (8)
           MOVE    POPT-DATA(21:4)     TO  POPT-SRYKBN-TBL (9)
           MOVE    POPT-DATA(26:4)     TO  POPT-SRYKBN-TBL (10)
           MOVE    POPT-DATA(31:4)     TO  POPT-SRYKBN-TBL (11)
           MOVE    POPT-DATA(36:4)     TO  POPT-SRYKBN-TBL (12)
           MOVE    POPT-DATA(41:4)     TO  POPT-SRYKBN-TBL (13)
      *
           MOVE    "SRYKBN30"          TO  POPT-NAME
           PERFORM 9001-OPTION-CALL-SEC
           IF      POPT-DATA           =   SPACE
               MOVE    CONST-SRYKBN30      TO  POPT-DATA
           END-IF
           MOVE    POPT-DATA(1:4)      TO  POPT-SRYKBN-TBL (14)
           MOVE    POPT-DATA(6:4)      TO  POPT-SRYKBN-TBL (15)
           MOVE    POPT-DATA(11:4)     TO  POPT-SRYKBN-TBL (16)
      *
           MOVE    "SRYKBN40"          TO  POPT-NAME
           PERFORM 9001-OPTION-CALL-SEC
           IF      POPT-DATA           =   SPACE
               MOVE    CONST-SRYKBN40      TO  POPT-DATA
           END-IF
           MOVE    POPT-DATA(1:4)      TO  POPT-SRYKBN-TBL (17)
           MOVE    POPT-DATA(6:4)      TO  POPT-SRYKBN-TBL (18)
           MOVE    POPT-DATA(11:4)     TO  POPT-SRYKBN-TBL (19)
           MOVE    POPT-DATA(16:4)     TO  POPT-SRYKBN-TBL (20)
           MOVE    POPT-DATA(21:4)     TO  POPT-SRYKBN-TBL (21)
           MOVE    POPT-DATA(26:4)     TO  POPT-SRYKBN-TBL (22)
           MOVE    POPT-DATA(31:4)     TO  POPT-SRYKBN-TBL (23)
      *
           MOVE    "SRYKBN90"          TO  POPT-NAME
           PERFORM 9001-OPTION-CALL-SEC
           IF      POPT-DATA           =   SPACE
               MOVE    CONST-SRYKBN90      TO  POPT-DATA
           END-IF
           MOVE    POPT-DATA(1:4)      TO  POPT-SRYKBN-TBL (24)
           MOVE    POPT-DATA(6:4)      TO  POPT-SRYKBN-TBL (25)
           MOVE    POPT-DATA(11:4)     TO  POPT-SRYKBN-TBL (26)
           .
       900-OPTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       9001-OPTION-CALL-SEC        SECTION.
      *
           INITIALIZE  ORCSPRGOPTIONAREA
           MOVE    "ORCBMRCSV"         TO  ORCSPRGOPTION-PRGID
           MOVE    POPT-NAME           TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"  USING  SPA-AREA
                                           ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  POPT-DATA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
               MOVE    ORCSPRGOPTION-OT-PARA
                                           TO  POPT-DATA
           END-IF
           .
       9001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル読み込み処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
