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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPI052BR5V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者病名マスタデータ移行
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/09/12    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(10).
           03  CNT-OUT1            PIC 9(10).
           03  CNT-XMLCOUNT        PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC X(02).
           03  WRK-PARA-TERMID     PIC X(64).
           03  WRK-PARA-BASE-MONTH PIC X(06).
           03  WRK-PARA-ERRFILE    PIC X(64).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
      *
           03  WRK-PTID            PIC 9(10).
           03  WRK-PTBYO-PTID-X.
               05  WRK-PTBYO-PTID  PIC 9(10).
      *
           03  WRK-YM              PIC 9(06).
           03  WRK-SRYYM           PIC X(06).
           03  WRK-END-SRYYM       PIC X(06).
      *
           03  WRK-DISEASE-ERRCD  PIC X(03).
      *
           03  WRK-LENGTH          PIC 9(02).
      *
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-MCP-PTBYO-PATHNAME
                                   PIC X(64).
      *
           03  WRK-BASE-MONTH      PIC X(07).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC X(04).
               05  WRK-SMM         PIC X(02).
               05  WRK-SDD         PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *
           03  WRK-THMS.
               05  WRK-THH         PIC X(02).
               05  WRK-TMM         PIC X(02).
               05  WRK-TSS         PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH     PIC X(02).
               05  WRK-HEN-T1      PIC X(01).
               05  WRK-HEN-TMM     PIC X(02).
               05  WRK-HEN-T2      PIC X(01).
               05  WRK-HEN-TSS     PIC X(02).
      *
      *    患者病名データ
       01  PTBYOMEILST-REC.
           03  PTBYOMEILST-OCC          OCCURS   28.
      *        患者番号
               05  PTBYOLST-PATIENTID   PIC X(20).
      *        患者氏名
               05  PTBYOLST-NAME        PIC X(100).
      *        患者カナ氏名
               05  PTBYOLST-KANANAME    PIC X(100).
      *        誕生日
               05  PTBYOLST-BIRTHDAY    PIC X(10).
      *        性別
               05  PTBYOLST-SEX         PIC X(1).
      *
               05  PTBYOLST-ERRCD       PIC X(03).
               05  PTBYOLST-BASE-MONTH  PIC X(07).
               05  PTBYOLST-INFORMATION-OVERFLOW
                                        PIC X(5).
      *        病名情報
               05  PTBYOLST-DISEASE-INF-G   OCCURS   50.
      *
                   07  PTBYOLST-DISEASE-ERRCD  PIC X(03).
      *            入外区分
                   07  PTBYOLST-NYUGAIKBN      PIC X(1).
      *            診療科
                   07  PTBYOLST-SRYKA          PIC X(02).
      *            保険組合せ番号
                   07  PTBYOLST-HKNCOMBI       PIC X(04).
      *            病名
                   07  PTBYOLST-DISEASE-NAME   PIC X(80).
      *            病名コード
                   07  PTBYOLST-DISEASE-SINGLE-G.
                       09  PTBYOLST-DISEASE-SINGLE OCCURS   21.
      *                    病名コード
                           11  PTBYOLST-SINGLE-CODE    PIC X(07).
      *                    病名
                           11  PTBYOLST-SINGLE-NAME    PIC X(80).
      *                    状態コード
                           11  PTBYOLST-SINGLE-FLG     PIC X(01).
      *            主病名フラグ
                   07  PTBYOLST-CATEGORY       PIC X(02).
      *            疑いフラグ
                   07  PTBYOLST-SUSPECTEDFLAG  PIC X(02).
      *        診療開始日
                   07  PTBYOLST-START-DATE     PIC X(10).
      *        転帰日
                   07  PTBYOLST-END-DATE       PIC X(10).
      *        転帰区分
                   07  PTBYOLST-OUTCOME        PIC X(02).
      *        補足コメント
                   07  PTBYOLST-SUPPLEMENT-NAME
                                               PIC X(40).
      *        補足コメントコード
                   07  PTBYOLST-SUPPLEMENT-SINGLE-G.
                       09  PTBYOLST-SUPPLEMENT-SINGLE   OCCURS   3.
      *                    補足コメントコード
                           11  PTBYOLST-SUPPLEMENT-SINGLE-CODE
                                               PIC X(10).
      *        カルテ病名
                   07  PTBYOLST-KARTE-NAME     PIC X(80).
      *        疾患区分
                   07  PTBYOLST-DISEASE-CLASS  PIC X(02).
      *        レセプト表示フラグ
                   07  PTBYOLST-RECEIPT-PRINT  PIC X(01).
      *        レセプト表示月数
                   07  PTBYOLST-RECEIPT-PERIOD PIC X(02).
      *        保険病名フラグ
                    07  PTBYOLST-INSURANCE-DISEASE PIC X(05).
      *        原疾患区分
                   07  PTBYOLST-NUMBER-MASTER  PIC X(02).
      *        合併症区分
                   07  PTBYOLST-NUMBER-SERVANT PIC X(02).
      *        退院証明書
                   07   PTBYOLST-DISCHARGEFLG  PIC X(5).
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者番号構成情報
           COPY    "CPSK1009.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "MONBLOB".
           COPY    "XMLDATA.INC".
           COPY    "ORCA-BLOB".
      *
           COPY    "CPORCSAPIFILE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ******************************************************************
      *
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
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
                                       CNT-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
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
                                               RECEERR
                                               WRK-PARA-BASE-MONTH
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORAPI052BR5V3"     TO  JOB-PGID
           MOVE    "患者病名情報返却"  TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    患者番号構成管理情報
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-PARA-BASE-MONTH TO  SYS-1009-STYUKYMD  (1:6)
           MOVE    "01"                TO  SYS-1009-STYUKYMD  (7:2)
           MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD  
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1009-REC
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
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    システム月取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD (1:6)  TO  WRK-END-SRYYM
      *
           MOVE    WRK-PARA-BASE-MONTH TO  WRK-SRYYM
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  CNT-OUT1
           MOVE    SPACE               TO  PTBYOMEILST-REC
           INITIALIZE                      PTBYOMEILST-REC
      *
           PERFORM         UNTIL   WRK-SRYYM   >   WRK-END-SRYYM
               MOVE    WRK-SRYYM           TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2)
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  WRK-BASE-MONTH
      *
               DISPLAY "SRYYM=" WRK-SRYYM
      *
               INITIALIZE                      PTBYOMEI-REC
               MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
               MOVE    WRK-SRYYM           TO  PTBYO-SRYYMD   (1:6)
               MOVE    "31"                TO  PTBYO-SRYYMD   (7:2)
               MOVE    WRK-SRYYM           TO  PTBYO-TENKIYMD (1:6)
               MOVE    "01"                TO  PTBYO-TENKIYMD (7:2)
               MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               IF      WRK-SRYYM           =   WRK-END-SRYYM
                   MOVE    "key52"         TO  WRK-MCP-PTBYO-PATHNAME
               ELSE
                   MOVE    "key51"         TO  WRK-MCP-PTBYO-PATHNAME
               END-IF
               MOVE    WRK-MCP-PTBYO-PATHNAME
                                           TO  MCP-PATHNAME
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   PERFORM 900-PTBYOMEI-READ-N-SEC
               ELSE
                   INITIALIZE                      PTBYOMEI-REC
                   MOVE    1                   TO  FLG-PTBYOMEI
               END-IF
      *
      *        病名マスタ編集処理
               PERFORM     UNTIL   FLG-PTBYOMEI  =   1
                   PERFORM     UNTIL   FLG-PTBYOMEI    =   1
                               OR      CNT-OUT1        >=  28
                       PERFORM 2001-PTBYOMEILST-HENSYU-SEC
                   END-PERFORM
      *
                   IF      CNT-OUT1            >=  28
                       ADD     1                   TO  CNT-XMLCOUNT
                       PERFORM 910-XMLEXPORT-SEC
      *
                       MOVE    ZERO                TO  CNT-OUT1
                       MOVE    SPACE               TO  PTBYOMEILST-REC
                       INITIALIZE                      PTBYOMEILST-REC
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    WRK-MCP-PTBYO-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE   "61"                 TO  LNK-DAY6-IRAI
               MOVE   WRK-SRYYM            TO  LNK-DAY6-KIJUN (1:6)
               MOVE   "01"                 TO  LNK-DAY6-KIJUN (7:2)
               MOVE   "2"                  TO  LNK-DAY6-ZOGENPTN
               MOVE   1                    TO  LNK-DAY6-ZOGEN
               CALL   "ORCSDAY"            USING   STS-AREA-DAY
                                                  LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN (1:6)
                                           TO  WRK-SRYYM
           END-PERFORM
      *
           IF      CNT-OUT1            >   ZERO
               ADD     1                   TO  CNT-XMLCOUNT
               PERFORM 910-XMLEXPORT-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスタ編集処理
      *****************************************************************
       2001-PTBYOMEILST-HENSYU-SEC          SECTION.
      *
           ADD     1                   TO  CNT-OUT1
                                           CNT-OUT
           MOVE    CNT-OUT1            TO  IDX
      *
           MOVE    SPACE               TO  WRK-DISEASE-ERRCD
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTBYO-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  PTBYOLST-PATIENTID      (IDX)
           ELSE
               MOVE    PTBYO-PTID      TO  WRK-PTBYO-PTID
               STRING  "*"                 DELIMITED   BY  SIZE
                       WRK-PTBYO-PTID-X    DELIMITED   BY  SIZE
                       INTO                PTBYOLST-PATIENTID      (IDX)
               END-STRING
               MOVE    "E10"           TO  PTBYOLST-ERRCD          (IDX)
           END-IF
      *??
      *    DISPLAY "PTNUM=" SPTID-PTNUM "#" 
      *??
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTBYO-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   0
               MOVE    PTINF-NAME          TO  PTBYOLST-NAME       (IDX)
               MOVE    PTINF-KANANAME      TO  PTBYOLST-KANANAME   (IDX)
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  PTBYOLST-BIRTHDAY   (IDX)
               MOVE    PTINF-SEX           TO  PTBYOLST-SEX        (IDX)
           ELSE
               IF      PTBYOLST-ERRCD (IDX)
                                           =   SPACE
                   MOVE    "E12"               TO  PTBYOLST-ERRCD  (IDX)
               END-IF
           END-IF
      *
           MOVE    WRK-BASE-MONTH      TO  PTBYOLST-BASE-MONTH     (IDX)
      *
           MOVE    ZERO                TO  IDY
           MOVE    PTBYO-PTID          TO  WRK-PTID
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    =   1        )
                   OR    ( PTBYO-PTID  NOT =   WRK-PTID )
             ADD     1                   TO  IDY
      *
             IF      IDY                 >   50
               CONTINUE
             ELSE
               EVALUATE    PTBYO-NYUGAIKBN
                   WHEN    "1"
                       MOVE    "I"             TO  PTBYOLST-NYUGAIKBN
                                                               (IDX IDY)
                   WHEN    "2"
                       MOVE    "O"             TO  PTBYOLST-NYUGAIKBN
                                                               (IDX IDY)
               END-EVALUATE
               MOVE    PTBYO-SRYKA         TO  PTBYOLST-SRYKA  (IDX IDY)
               IF      PTBYO-HKNCOMBI   NOT =  ZERO
                   MOVE    PTBYO-HKNCOMBI      TO  PTBYOLST-HKNCOMBI
                                                               (IDX IDY)
                   INITIALIZE                      HKNCOMBI-REC
                   MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
                   MOVE    WRK-PTID            TO  COMB-PTID
                   MOVE    PTBYO-HKNCOMBI      TO  COMB-HKNCOMBINUM
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   MOVE    "key"               TO  WRK-MCP-PATHNAME
                   PERFORM 900-COMB-READ-SEC
                   IF      FLG-HKNCOMBI        =   1
                       MOVE    "E19"           TO  WRK-DISEASE-ERRCD
                       PERFORM 20011-PTBYOMEILST-ERR-SEC
                   ELSE
                       IF      COMB-DELKBN         =  "1"
                           MOVE    "E28"           TO  WRK-DISEASE-ERRCD
                           PERFORM 20011-PTBYOMEILST-ERR-SEC
                       ELSE
                           IF      PTBYO-SRYYMD    <=  COMB-TEKEDYMD
                               CONTINUE
                           ELSE    
                               MOVE    "E27"       TO  WRK-DISEASE-ERRCD
                               PERFORM 20011-PTBYOMEILST-ERR-SEC
                           END-IF
                       END-IF
                   END-IF
               ELSE
                   IF      PTBYO-KOHID     >   ZERO
                       INITIALIZE                      PTRSIINF-REC
                       MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
                       MOVE    WRK-PTID            TO  PTRSI-PTID
                       MOVE    PTBYO-KOHID         TO  PTRSI-KOHID
                       MOVE    ZERO                TO  PTRSI-HKNID
                       MOVE    PTRSIINF-REC        TO  MCPDATA-REC
                       PERFORM 900-PTRSIINF-READ-SEC
                       IF      FLG-PTRSIINF        =   1
                           MOVE    "E25"           TO  WRK-DISEASE-ERRCD
                           PERFORM 20011-PTBYOMEILST-ERR-SEC
                       ELSE
                           IF      PTRSI-SAKUJOKBN     =   "1"
                               MOVE    "E29"           TO
                                                       WRK-DISEASE-ERRCD
                               PERFORM 20011-PTBYOMEILST-ERR-SEC
                           ELSE
                               IF      PTBYO-SRYYMD
                                                   >   PTRSI-RYOEDYMD
                                   MOVE    "E26"       TO
                                                       WRK-DISEASE-ERRCD
                                   PERFORM 20011-PTBYOMEILST-ERR-SEC
                               ELSE
                                   INITIALIZE              HKNCOMBI-REC
                                   MOVE    SPA-HOSPNUM TO  COMB-HOSPNUM
                                   MOVE    WRK-PTID    TO  COMB-PTID
                                   MOVE    PTBYO-KOHID TO  COMB-KOHID(1)
                                                           COMB-KOHID(2)
                                                           COMB-KOHID(3)
                                                           COMB-KOHID(4)
                                   MOVE    HKNCOMBI-REC
                                                       TO  MCPDATA-REC
                                   MOVE    "tbl_hkncombi"
                                                       TO  MCP-TABLE
                                   MOVE    "key4"      TO
                                                       WRK-MCP-PATHNAME
                                   PERFORM 900-COMB-READ-SEC
                                   IF    ( FLG-HKNCOMBI    =   1   )
                                   OR    ( COMB-DELKBN     =   "1" )
                                       MOVE    "E30"           TO
                                                       WRK-DISEASE-ERRCD
                                       PERFORM 20011-PTBYOMEILST-ERR-SEC
                                   ELSE
                                       MOVE    COMB-HKNCOMBINUM
                                                   TO  PTBYOLST-HKNCOMBI
                                                               (IDX IDY)
                                   END-IF
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    PTBYO-BYOMEI        TO  PTBYOLST-DISEASE-NAME
                                                               (IDX IDY)
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL     ( IDZ     >       21    )
                       OR        ( PTBYO-BYOMEICD (IDZ)
                                            =       SPACE )
                   MOVE    PTBYO-BYOMEICD (IDZ)
                                           TO  PTBYOLST-SINGLE-CODE
                                                           (IDX IDY IDZ)
      *
                   MOVE    PTBYO-BYOMEICD (IDZ)
                                           TO  BYO-BYOMEICD
                   IF  (   BYO-BYOMEICD (1:3)    =   "ZZZ"    )
                   AND ( ( BYO-BYOMEICD (4:1)    <   "1" ) OR 
                         ( BYO-BYOMEICD (4:1)    =   "C" )    )
      *                予約病名コード検索
                       SET     TBL-BYOMEICD-IDX  TO  1
                       SEARCH  TBL-BYOMEICD-OCC
                                           VARYING TBL-BYOMEICD-IDX
                           AT  END
                               MOVE    "1"       TO
                                       PTBYOLST-SINGLE-FLG
                                                           (IDX IDY IDZ)
                           WHEN    BYO-BYOMEICD    =
                                               TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                               MOVE    TBL-YYK-BYOMEI (TBL-BYOMEICD-IDX)
                                                   TO
                                       PTBYOLST-SINGLE-NAME
                                                           (IDX IDY IDZ)
                               MOVE    ZERO        TO  FLG-BYOMEI
                       END-SEARCH
                   ELSE
      *                病名マスタ検索
                       IF      BYO-BYOMEICD    =   "0000999"
                           MOVE    SPACE           TO
                                       PTBYOLST-SINGLE-NAME
                                                           (IDX IDY IDZ)
                       ELSE 
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEI      TO
                                                   PTBYOLST-SINGLE-NAME
                                                           (IDX IDY IDZ)
                               IF      BYO-HAISIYMD    =   "99999999"
                                                       OR  SPACE
                                   CONTINUE
                               ELSE
                                   MOVE    "2"         TO
                                                   PTBYOLST-SINGLE-FLG
                                                           (IDX IDY IDZ)
                               END-IF
                           ELSE
                               MOVE    "1"             TO
                                                   PTBYOLST-SINGLE-FLG
                                                           (IDX IDY IDZ)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF      PTBYO-SYUBYOFLG     =   "1"
                   MOVE    "PD"                TO  PTBYOLST-CATEGORY
                                                               (IDX IDY)
               END-IF
               EVALUATE    PTBYO-UTAGAIFLG
                   WHEN    "1"
                       MOVE    "S"         TO  PTBYOLST-SUSPECTEDFLAG
                                                               (IDX IDY)
                   WHEN    "2"
                       MOVE    "A"         TO  PTBYOLST-SUSPECTEDFLAG
                                                               (IDX IDY)
                   WHEN    "3"
                       MOVE    "SA"        TO  PTBYOLST-SUSPECTEDFLAG
                                                               (IDX IDY)
               END-EVALUATE
      *
               MOVE    SPACE               TO  STS-AREA-DAY
               INITIALIZE                      STS-AREA-DAY
               MOVE    SPACE               TO  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    PTBYO-SRYYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               IF      STS-DAY-RC1     NOT =   ZERO
                   MOVE    PTBYO-SRYYMD        TO
                                       PTBYOLST-START-DATE     (IDX IDY)
                   MOVE    "E16"               TO  WRK-DISEASE-ERRCD
                           PERFORM 20011-PTBYOMEILST-ERR-SEC
               ELSE
                   MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE        TO
                                       PTBYOLST-START-DATE     (IDX IDY)
               END-IF
      *
               IF      PTBYO-TENKIYMD      =   SPACE
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    PTBYO-TENKIYMD      TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    PTBYO-TENKIYMD      TO
                                       PTBYOLST-END-DATE       (IDX IDY)
                       MOVE    "E17"               TO  WRK-DISEASE-ERRCD
                           PERFORM 20011-PTBYOMEILST-ERR-SEC
                   ELSE
                       MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE        TO
                                       PTBYOLST-END-DATE       (IDX IDY)
                   END-IF
               END-IF
      *
               EVALUATE    PTBYO-TENKIKBN
                   WHEN    "1"
                       MOVE    "F"             TO  PTBYOLST-OUTCOME
                                                               (IDX IDY)
                   WHEN    "2"
                       MOVE    "D"             TO  PTBYOLST-OUTCOME
                                                               (IDX IDY)
                   WHEN    "3"
                       MOVE    "C"             TO  PTBYOLST-OUTCOME
                                                               (IDX IDY)
                   WHEN    "8"
                       MOVE    "S"             TO  PTBYOLST-OUTCOME
                                                               (IDX IDY)
               END-EVALUATE
               MOVE    PTBYO-HOSOKU-COMMENT
                                           TO  PTBYOLST-SUPPLEMENT-NAME
                                                               (IDX IDY)
      *        補足コメントコード
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   IDZ     >       3
                       OR      PTBYO-HOSOKUCD (IDZ)
                                       =       SPACE
                   MOVE    PTBYO-HOSOKUCD (IDZ)
                                       TO
                                   PTBYOLST-SUPPLEMENT-SINGLE-CODE
                                                           (IDX IDY IDZ)
               END-PERFORM
      *        カルテ病名
               MOVE    PTBYO-CHARTBYOMEI   TO  PTBYOLST-KARTE-NAME
                                                               (IDX IDY)
      *        疾患区分
               IF      PTBYO-MANSEIKBN
                                       NOT =   ZERO
                   MOVE    PTBYO-MANSEIKBN TO  PTBYOLST-DISEASE-CLASS
                                                               (IDX IDY)
               END-IF
      *        レセプト表示フラグ
               MOVE    PTBYO-REZEFLG       TO  PTBYOLST-RECEIPT-PRINT
                                                               (IDX IDY)
      *        レセプト表示月数
               IF      PTBYO-REZEMM    NOT =   ZERO
                   MOVE    PTBYO-REZEMM        TO
                                       PTBYOLST-RECEIPT-PERIOD (IDX IDY)
               END-IF
      *        保険病名フラグ
               IF      PTBYO-HKNBYOMEIFLG  =   "1"
                   MOVE    "True"              TO
                                   PTBYOLST-INSURANCE-DISEASE  (IDX IDY)
               ELSE
                   MOVE    "False"             TO
                                   PTBYOLST-INSURANCE-DISEASE  (IDX IDY)
               END-IF
      *        原疾患区分
               IF      PTBYO-CLASS1    NOT =   ZERO
                   MOVE    PTBYO-CLASS1        TO
                                   PTBYOLST-NUMBER-MASTER      (IDX IDY)
               END-IF
      *        合併症区分
               IF      PTBYO-CLASS2    NOT =   ZERO
                   MOVE    PTBYO-CLASS2        TO
                                   PTBYOLST-NUMBER-SERVANT     (IDX IDY)
               END-IF
      *        退院証明書記載フラグ
               IF      PTBYO-DISCHARGEFLG
                                           =   "1"
                   MOVE    "True"              TO
                                       PTBYOLST-DISCHARGEFLG   (IDX IDY)
               ELSE
                   MOVE    "False"             TO
                                       PTBYOLST-DISCHARGEFLG   (IDX IDY)
               END-IF
             END-IF
      *
             PERFORM 900-PTBYOMEI-READ-N-SEC
           END-PERFORM
      *
           IF      IDY                 =   ZERO
               CONTINUE
           ELSE
               IF      IDY                 >   50
      *            50件以上あり
                   MOVE    "True"             TO
                                           PTBYOLST-INFORMATION-OVERFLOW
                                                                   (IDX)
               ELSE
                   MOVE    "False"           TO
                                           PTBYOLST-INFORMATION-OVERFLOW
                                                                   (IDX)
               END-IF
           END-IF
           .
       2001-PTBYOMEILST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       20011-PTBYOMEILST-ERR-SEC      SECTION.
      *
           IF      PTBYOLST-DISEASE-ERRCD  (IDX IDY)
                                      =   SPACE
               MOVE    WRK-DISEASE-ERRCD  TO
                                       PTBYOLST-DISEASE-ERRCD  (IDX IDY)
           END-IF
      *
           IF      PTBYOLST-ERRCD (IDX)
                                      =   SPACE
               MOVE    WRK-DISEASE-ERRCD  TO  PTBYOLST-ERRCD (IDX)
           END-IF
      *
           MOVE    SPACE               TO  WRK-DISEASE-ERRCD
           .
       20011-PTBYOMEILST-ERR-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT 処理
      *****************************************************************
       910-XMLEXPORT-SEC          SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE   PTBYOMEILST-REC  TO  XMLDATA-DATA
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_patient_disease_all"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "Patient_Disease_All"
                                   TO  XMLDATA-RECORDNAME
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           INITIALIZE                   ORCA-BLOB
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    XMLDATA-OBJECT  TO  ORCA-BLOB-OBJECT
           INITIALIZE                  SGETTEMP-AREA
           STRING  "patient_disease_all"
                                   DELIMITED   BY  SIZE
                   CNT-XMLCOUNT    DELIMITED   BY  SIZE
                   ".xml"          DELIMITED   BY  SIZE
                                   INTO        SGETTEMP-BASENAMES  (1)
           END-STRING
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ORCA-BLOB-FILE
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       ORCA-BLOB
           .
       910-XMLEXPORT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               INITIALIZE                 ORCSAPIFILEAREA
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-IN-DIRNAME
               MOVE    "patient_disease_all"
                                       TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    "2"             TO  SAPIFILE-IN-KBN
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    "patient_disease_all.xml"
                                       TO  SAPIFILE-OT-FILENAME
               CALL    "ORCSAPIFILE"   USING   ORCSAPIFILEAREA
      *
               IF      SAPIFILE-RC     =   0
                   CONTINUE
               ELSE
                   STRING   "ｘｍｌファイル作成エラー RC="
                                           DELIMITED   BY  SIZE
                            SAPIFILE-RC    DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORAPI052BR5V3 OUT:" CNT-OUT
           DISPLAY "*** ORAPI052BR5V3 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
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
           MOVE    1                   TO  FLG-END
      *
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
           STRING  "ORAPI052BR3V3 "    DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
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
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
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
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報読込
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名読込
      *****************************************************************
       900-PTBYOMEI-READ-N-SEC         SECTION.
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    WRK-MCP-PTBYO-PATHNAME
                                       TO  MCP-PATHNAME
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC         TO  PTBYOMEI-REC
           ELSE
              MOVE    1                    TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYOMEI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-BYOMEI
                   MOVE    MCPDATA-REC     TO  BYOMEI-REC
               ELSE
                   MOVE    1               TO  FLG-BYOMEI
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
      *    自院病名読込
      *****************************************************************
       900-USERBYOMEI-READ-SEC         SECTION.
      *
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  USERBYOMEI-REC
                   MOVE    ZERO                TO  FLG-USERBYOMEI
               ELSE
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    1                   TO  FLG-USERBYOMEI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-USERBYOMEI
               INITIALIZE                      USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-USERBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ読込
      *****************************************************************
       900-COMB-READ-SEC                SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-N-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ読込
      *****************************************************************
       900-COMB-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-COMB-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読込
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTRSIINF-REC
                   MOVE    ZERO            TO  FLG-PTRSIINF
               ELSE
                   INITIALIZE                  PTRSIINF-REC
                   MOVE    1               TO  FLG-PTRSIINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       900-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       900-CLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
