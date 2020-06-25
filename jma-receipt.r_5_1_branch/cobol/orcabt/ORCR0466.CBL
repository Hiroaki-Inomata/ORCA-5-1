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
       PROGRAM-ID.             ORCR0466.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      金額摘要欄編集（労災）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04.04.26  ユーザーコメント追加対応
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/10/13  中止処理の追加
      *  01.01.02    NACL-門脇    05/07/15  自賠責従来様式対応
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/06/26  不要項目削除
      *  04.01.01    NACL-門脇    07/10/31  公務災害対応
      *  04.01.02    NACL-門脇    07/11/26  特定器材商品名記載対応
      *  04.03.01    NACL-門脇    09/01/23  円摘要欄編集修正
      *
      *  04.05.01    NACL-門脇    09/06/18  円摘要欄コメント編集対応
      *  04.05.02    NACL-藤原    09/07/14  小数点以下５桁対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *   
           SELECT  RECE46-FILE     ASSIGN  RECE46PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE46-KEY
                                   FILE    STATUS  IS  STS-RECE46.
      *   
           SELECT  RECE47-FILE     ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE47-KEY
                                   FILE    STATUS  IS  STS-RECE47.
      *    
           SELECT  RECE47X-FILE     ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE47X-KEY
                                   FILE    STATUS  IS  STS-RECE47.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
       FD  RECE46-FILE.
       01  RECE46-REC.
           COPY    "CPRCF0461.INC" REPLACING  //RECE461//
                                   BY         //RECE46//.
      *
      *
       FD  RECE47-FILE.
       01  RECE47-REC. 
           COPY    "CPRCF0471.INC"  REPLACING //RECE471//
                                   BY         //RECE47//.
      *
       FD  RECE47X-FILE.
       01  RECE47X-REC. 
           COPY    "CPRCF0471.INC"  REPLACING //RECE471//
                                   BY         //RECE47X//.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE01//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE46//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE47//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  SPA-STS             PIC 9(04).
           03  STS-RECE41          PIC X(02).
           03  STS-RECE47          PIC X(02).
           03  STS-RECE46          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE41          PIC 9(01).
           03  FLG-RECE46          PIC 9(01).
           03  FLG-RECE47          PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-NOPRT           PIC 9(01).
           03  FLG-HENSYU          PIC 9(01).
           03  FLG-HENSYU2         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE47          PIC 9(06).
           03  CNT-RECE46          PIC 9(06).
           03  CNT-IN-CANCEL       PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *    点数回数編集用     
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
      *    剤診療行為コード別編集用     
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM              PIC  X(06).
               05  WRK-PARA-TERMID             PIC  X(16).
               05  WRK-PARA-SYSYMD             PIC  X(08).
               05  WRK-PARA-RECEKBN            PIC  X(01).
               05  WRK-PARA-JOBID              PIC  9(07).
               05  WRK-PARA-SHELLID            PIC  X(08).
           03  WRK-PARA-JIBAIPRTKBN            PIC  X(01).
           03  WRK-PARA-HOSPNUM                PIC  9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-LINENUM         PIC 9(05).
      *    剤印字行数
           03  WRK-TBLMAX         PIC 9(03).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-KINGAKU         PIC 9(09)V9(02).
      *
      *    数字半角全角変換用     
           03  WRK-NUM                         PIC 9(01).
           03  WRK-CHAR-IDX                    PIC 9(02).
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(10).
               05  WRK-CHAR-N                  PIC X(20).
               05  WRK-CHAR-CNT                PIC 9(02).
               05  WRK-CHAR-FLG                PIC 9(01). 
               05  WRK-CHAR-N-CNT              PIC 9(02).
      *
           03  WRK-EDIT-AREA.
               05  WRK-CLM                     PIC 9(03).
               05  WRK-WCLM                    PIC 9(03).
      *
           03  WRK-KOUI                        PIC X(200).
      *
           03  WRK-ZAI.
               05  WRK-ZAI-NAME                PIC X(52).
      *
           03  WRK-ZAI1.
               05  WRK-ZAI1-NAME               PIC X(28).
      *
      *    点数・回数編集用
           03  WRK-ZAITENX                     PIC ZZZZZZZZ9.99.
           03  WRK-ZAIKAISUX                   PIC ZZZ9.
           03  WRK-TNSKAI-AREA.
               05  WRK-TNSKAI-ST               PIC 9(03).
               05  WRK-TNSKAI-ST1              PIC 9(03).
               05  WRK-TNSKAI-KETA             PIC 9(02).
               05  WRK-TNSKAI-LEN              PIC 9(02).
      *
               05  WRK-TNSKAI.
                   07  WRK-ZAITEN              PIC X(12).
                   07  WRK-X                   PIC X(02).
                   07  WRK-ZAIKAISU            PIC X(06).
      *             
           03  WRK-ST                          PIC 9(03).
      *     
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-SRYSURYOW                   PIC X(20).
           03  WRK-ZAIIDX                      PIC 9(03).
           03  WRK-NAMEKETA                    PIC 9(03).
           03  WRK-YOHAKU                      PIC 9(03).
           03  WRK-WKETA                       PIC 9(03).
      *     
           03  WRK-SRYKBN                      PIC X(02).
           03  WRK-Z9                          PIC ZZZ9.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *          
      *    診療行為別テーブル
       01  TBL-ZAIW-AREA.
           03  TBL-ZAIW-REC.
               05  TBL-ZAIW-SRYACTINF.
      *            診療コード/数量/回数
                   07  TBL-ZAIW-SRYCD           PIC  X(09).
                   07  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(05).
                   07  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *            単位コード/単位名称有効桁数/単位名称
                   07  TBL-ZAIW-TANICD          PIC  9(03).
                   07  TBL-ZAIW-TANIMOJISU      PIC  9(01).
                   07  TBL-ZAIW-TANINAME        PIC  X(24).
      *            点数識別区分/点数/明細請求フラグ
                   07  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
                   07  TBL-ZAIW-TEN             PIC  9(09)V9(02).
                   07  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *            自動算定区分/名称入力番号/点数欄集計先識別
                   07  TBL-ZAIW-AUTOKBN         PIC  X(01).
                   07  TBL-ZAIW-INPUTNUM        PIC  9(03).
                   07  TBL-ZAIW-TENTTLKBN       PIC  9(03).
               05  TBL-ZAIW-NAMEINF.
      *            漢字有効桁数/漢字名称
                   07  TBL-ZAIW-YUKOKETA        PIC  9(03).
                   07  TBL-ZAIW-NAME            PIC  X(200).
      *
      *    固定値
       01  CONST-AREA.
      *         
      *    適用欄最大文字数（金額欄）
           03  CONS-TEKIYOU1-LEN           PIC 9(02)   VALUE   28.
      *     
      *    適用欄最大文字数（点数欄）
           03  CONS-TEKIYOU2-LEN           PIC 9(02)   VALUE   52.
      *
      *    適用欄最大行数（金額欄）
           03  CONS-TEKIYOU1-LINE-MAX      PIC  9(03)  VALUE   9.
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
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
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *     
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       SPA-STS     =   2
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SPA-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  RECEERR
      *
      *    ファイルＯＰＥＮ処理
      *
           PERFORM 110-FILE-OPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               PERFORM 120-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0466"      TO  JOB-PGID
               MOVE    "金額摘要欄編集（労災）"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-JIBAIPRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE410"           TO  RECE01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE01PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE01PARA-HOSPNUM
      *
           MOVE    "RECE461"           TO  RECE46PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE46PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE46PARA-HOSPNUM
      *
           MOVE    "RECE471"           TO  RECE47PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE47PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE47PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE01PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE46PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE47PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE46PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE47PARA
      *
           OPEN    INPUT               RECE41-FILE
      *
           OPEN    INPUT               RECE46-FILE
      *
           OPEN    OUTPUT              RECE47X-FILE
           CLOSE                       RECE47X-FILE
      *
           OPEN    I-O                 RECE47-FILE
      *
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    行カウンタ初期化
           MOVE    ZERO                TO  WRK-LINENUM
           MOVE    ZERO                TO  WRK-TBLMAX
      *
           INITIALIZE                      TBL-ZAIW-AREA
      *
      *    外来管理加算（特例５２）の編集
           IF      RECE41-GAITOKU-PRTKBN1  =   "1"
               MOVE    1                   TO  RECE46-ZAILAST
               PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
               MOVE    RECE41-GAITOKU-TEN1 TO  WRK-ZAITENX
                                               WRK-KINGAKU
               MOVE    RECE41-GAITOKU-KAISU1
                                           TO  WRK-ZAIKAISUX
               PERFORM 4304-TENSUKAI-HEN-SEC
               MOVE    "＊（特）"          TO  WRK-ZAI1-NAME (1:8)
               MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                                           TO
                                        WRK-ZAI1-NAME(WRK-TNSKAI-ST1:)
               MOVE    RECE41-KEY        TO  RECE46-KEY
               MOVE    SPACE             TO  RECE46-SRYKBN
               MOVE    ZERO              TO  RECE46-GAITENTTLKBN
               MOVE    SPACE             TO  RECE46-SRYCD1
               MOVE    99999986          TO  RECE46-ZAINUM
               MOVE    RECE41-PTNUM      TO  RECE46-PTNUM
               MOVE    SPACE             TO  RECE46-SRYKA
               PERFORM 450-RECE47-OUT-SEC                         
           END-IF
      *
      *    外来管理加算（特例２６）の編集
           IF      RECE41-GAITOKU-PRTKBN2  =   "1"
               MOVE    1                   TO  RECE46-ZAILAST
               PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
               MOVE    RECE41-GAITOKU-TEN2 TO  WRK-ZAITENX
                                               WRK-KINGAKU
               MOVE    RECE41-GAITOKU-KAISU2
                                           TO  WRK-ZAIKAISUX
               PERFORM 4304-TENSUKAI-HEN-SEC
               MOVE    "＊（特）"          TO  WRK-ZAI1-NAME (1:8)
               MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                                           TO
                                        WRK-ZAI1-NAME(WRK-TNSKAI-ST1:)
               MOVE    RECE41-KEY        TO  RECE46-KEY
               MOVE    SPACE             TO  RECE46-SRYKBN
               MOVE    ZERO              TO  RECE46-GAITENTTLKBN
               MOVE    SPACE             TO  RECE46-SRYCD1
               MOVE    99999987          TO  RECE46-ZAINUM
               MOVE    RECE41-PTNUM      TO  RECE46-PTNUM
               MOVE    SPACE             TO  RECE46-SRYKA
               PERFORM 450-RECE47-OUT-SEC                       
           END-IF
      *
           MOVE        ZERO            TO  WRK-ZAINUM
                                           WRK-KINGAKU
           MOVE        ZERO            TO  FLG-HENSYU2
      *
      *    パラメータ０４ファイルより編集
           INITIALIZE                      RECE46-REC
           MOVE    RECE41-HOSPNUM      TO  RECE46-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE46-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE46-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE46-PTID
           MOVE    RECE41-RECEKA       TO  RECE46-RECEKA
           MOVE    RECE41-HKNID        TO  RECE46-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE46-TEKSTYMD
      *
           START   RECE46-FILE
               KEY IS    >=   RECE46-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE46
               NOT INVALID
                   PERFORM 990-RECE46-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( SPA-STS              =   2                )
               OR     ( FLG-RECE46           =   1                )
               OR     ( RECE41-HOSPNUM   NOT =   RECE46-HOSPNUM   )
               OR     ( RECE41-SRYYM     NOT =   RECE46-SRYYM     )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE46-NYUGAIKBN )
               OR     ( RECE41-PTID      NOT =   RECE46-PTID      )
               OR     ( RECE41-RECEKA    NOT =   RECE46-RECEKA    )
               OR     ( RECE41-HKNID     NOT =   RECE46-HKNID     )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE46-TEKSTYMD  )
      *
                   IF      RECE46-ZAINUM   NOT =   WRK-ZAINUM
                      MOVE    RECE46-ZAINUM  TO    WRK-ZAINUM
                      MOVE    ZERO           TO    WRK-KINGAKU
                      MOVE    ZERO           TO    FLG-HENSYU2
                   END-IF
      *
                   ADD     RECE46-TEN        TO    WRK-KINGAKU
                   MOVE    WRK-KINGAKU       TO    WRK-ZAITENX
                   MOVE    RECE46-ZAIKAISU   TO    WRK-ZAIKAISUX
      *
                   PERFORM 2001-RECE44-ZAIHEN-MAE-SEC
      *            
                   PERFORM 2002-TBLZAIW-TAIHI-SEC
      *
                   PERFORM 2003-TEKIYOU-HENSYU-SEC
      *
                   PERFORM 990-RECE46-NEXT-SEC
      *
      *            処理中止設定処理
                   IF      CNT-IN-CANCEL   =   50
                       MOVE    ZERO            TO  CNT-IN-CANCEL
                       PERFORM 500-CANCEL-HENSYU-SEC
                   END-IF
           END-PERFORM
      *
           IF      SPA-STS         =   ZERO
               PERFORM 990-RECE41-READ-SEC
      *
      *        処理中止設定処理
               IF      CNT-IN-CANCEL   =   50
                   MOVE    ZERO            TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF    
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤編集前処理
      *****************************************************************
       2001-RECE44-ZAIHEN-MAE-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAI
                                           WRK-ZAI1
           MOVE    ZERO                TO  WRK-CLM
      *
           .
       2001-RECE44-ZAIHEN-MAE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    明細テーブル退避処理
      *****************************************************************
       2002-TBLZAIW-TAIHI-SEC    SECTION.
      *
           MOVE    RECE46-SRYACTINF  TO  TBL-ZAIW-SRYACTINF
           MOVE    RECE46-NAMEINF    TO  TBL-ZAIW-NAMEINF  
      *
           .
       2002-TBLZAIW-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    編集処理
      *****************************************************************
       2003-TEKIYOU-HENSYU-SEC    SECTION.
      *
      *    金額・回数編集
      ***> MOVE    TBL-ZAIW-TEN        TO  WRK-ZAITENX
      ***> MOVE    RECE46-ZAIKAISU     TO  WRK-ZAIKAISUX
      *     
           PERFORM 4304-TENSUKAI-HEN-SEC 
      *
      *    明細編集・明細桁数計算
           PERFORM 4302-MEISAI-KETA-HEN1-SEC
      *
      *    剤・診療行為テーブル出力処理
      **** PERFORM 450-RECE47-OUT-SEC
      *
           .
       2003-TEKIYOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細・明細桁　編集処理
      *****************************************************************
       4302-MEISAI-KETA-HEN1-SEC    SECTION.
      *
           MOVE    1                   TO  WRK-ST              
      *
           MOVE    ZERO                TO  FLG-HENSYU
      *     
           PERFORM UNTIL   FLG-HENSYU  =   1  
               IF      WRK-TBLMAX         <   CONS-TEKIYOU1-LINE-MAX 
                   MOVE    SPACE               TO  WRK-ZAI1
                   IF  FLG-HENSYU2  =  ZERO
                       MOVE   "＊"  TO   WRK-ZAI1-NAME(1:2)
                       MOVE    1    TO   FLG-HENSYU2
                   END-IF
                   MOVE    TBL-ZAIW-NAME (WRK-ST:)
                                               TO  WRK-ZAI1-NAME(3:)
                   IF      WRK-TNSKAI      NOT =   SPACE                            
                       IF      WRK-ZAI1-NAME (WRK-TNSKAI-ST1:)
                                               =   SPACE
                           MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                                               TO
                                        WRK-ZAI1-NAME(WRK-TNSKAI-ST1:)
                           MOVE    1               TO  FLG-HENSYU
                       END-IF                 
                   END-IF                                                  
      *            剤・診療行為テーブル編集処理
                   PERFORM 450-RECE47-OUT-SEC
                   COMPUTE WRK-ST  =
                           WRK-ST  + CONS-TEKIYOU1-LEN - 2
               ELSE
                   MOVE    SPACE               TO  WRK-ZAI
                   IF  FLG-HENSYU2  =  ZERO
                       MOVE   "＊"  TO   WRK-ZAI-NAME(1:2)
                       MOVE    1    TO   FLG-HENSYU2
                   END-IF
                   MOVE    TBL-ZAIW-NAME (WRK-ST:)
                                               TO  WRK-ZAI-NAME(3:)
                   IF      WRK-TNSKAI      NOT =   SPACE                            
                       IF      WRK-ZAI-NAME (WRK-TNSKAI-ST:)
                                               =   SPACE
                           MOVE    WRK-TNSKAI (WRK-TNSKAI-LEN:)
                                               TO
                                        WRK-ZAI-NAME(WRK-TNSKAI-ST:)
                           MOVE    1               TO  FLG-HENSYU
                       END-IF                 
                   END-IF                                                  
      *            剤・診療行為テーブル編集処理
                   PERFORM 450-RECE47-OUT-SEC
                   COMPUTE WRK-ST  =
                           WRK-ST  + CONS-TEKIYOU2-LEN - 2
               END-IF
      *
      *　　　　金額の編集がないときまだ印刷する文字があるか         
               IF      WRK-TNSKAI          =   SPACE                            
                   IF      TBL-ZAIW-NAME (WRK-ST:)
                                               =   SPACE
                       MOVE    1               TO  FLG-HENSYU
                   END-IF                                                  
               END-IF
           END-PERFORM        
           .
       4302-MEISAI-KETA-HEN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額・回数編集処理
      *****************************************************************
       4304-TENSUKAI-HEN-SEC    SECTION.
      *
           INITIALIZE              WRK-TNSKAI-AREA
      *
      ***> IF    ( TBL-ZAIW-SRYCD (1:1)  =   "8"   )
      ***> OR    ( TBL-ZAIW-SRYCD (1:3)  =   "008" )
           IF    ( RECE46-ZAILAST   =   ZERO )
           OR    ( WRK-KINGAKU      =   ZERO )
               GO  TO  4304-TENSUKAI-HEN-EXT
           END-IF
      *                                           
           INITIALIZE              ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI 
           MOVE    WRK-ZAITENX(4:6)
                                   TO  KANACHK-MAE-INPUT (1:6)
           MOVE    "X"             TO  KANACHK-MAE-INPUT (7:1)
           MOVE    WRK-ZAIKAISUX (2:3)
                                   TO  KANACHK-MAE-INPUT (8:3)
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   ZERO
           AND     KANACHK-MAX     >   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       TO  WRK-TNSKAI
               MOVE    "×"                TO  WRK-X
           END-IF
      *
      *    必要有効桁数（点数ZZZZZ X 数量ZZZ の全角の桁数）
           MOVE    ZERO                TO  WRK-TNSKAI-KETA
           PERFORM VARYING   IDZ     FROM    1   BY  2
                   UNTIL     IDZ     >       20
      ***>     IF      WRK-TNSKAI (IDZ:2)      NOT =   SPACE
               IF      WRK-TNSKAI (IDZ:2)      NOT =   "　"
                   IF   IDZ = 1
                      COMPUTE WRK-TNSKAI-KETA = 20 - IDZ + 1
                      MOVE    IDZ   TO   WRK-TNSKAI-LEN
                   ELSE
                      COMPUTE WRK-TNSKAI-KETA = 20 - IDZ + 1 + 2
                      COMPUTE WRK-TNSKAI-LEN  = IDZ  -  2
                   END-IF
                   MOVE    20                  TO  IDZ
               END-IF 
           END-PERFORM
      *     
      *    印刷開始位置を取得
           COMPUTE   WRK-TNSKAI-ST     =   ( CONS-TEKIYOU2-LEN  -
                                             WRK-TNSKAI-KETA      )
                                       +   1
           COMPUTE   WRK-TNSKAI-ST1    =   ( CONS-TEKIYOU1-LEN  -
                                             WRK-TNSKAI-KETA      )
                                       +   1
      *
           .
       4304-TENSUKAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０６出力処理
      *****************************************************************
       450-RECE47-OUT-SEC    SECTION.
      *
           ADD     1                   TO  WRK-LINENUM
           MOVE    SPACE               TO  RECE47-REC
           INITIALIZE                      RECE47-REC
      *     
           IF      WRK-TBLMAX         <   CONS-TEKIYOU1-LINE-MAX 
               ADD     1                   TO  WRK-TBLMAX
               MOVE    WRK-ZAI1            TO  RECE47-TEKIYO-INF
           ELSE    
               MOVE    WRK-ZAI             TO  RECE47-TEKIYO-INF
           END-IF
      *     
           PERFORM 4501-RECE47-HENSYU-SEC
      *
           MOVE    SPACE                TO  WRK-ZAI
                                            WRK-ZAI1
           .
       450-RECE47-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０６編集処理
      *****************************************************************
       4501-RECE47-HENSYU-SEC    SECTION.
      *
           MOVE    RECE46-KEY        TO  RECE47-KEY
           MOVE    WRK-LINENUM       TO  RECE47-LINENUM
           MOVE    RECE46-PTNUM      TO  RECE47-PTNUM
           MOVE    RECE46-SRYKA      TO  RECE47-SRYKA
      **** MOVE    RECE46-SRYKBN     TO  RECE47-SRYKBN
      *
           WRITE   RECE47-REC
           IF      STS-RECE47          NOT =   ZERO
               DISPLAY "*** ORCR0466 RECE47 WRITE ERR  ***"
               MOVE    2             TO  SPA-STS                       
           ELSE
               ADD     1             TO  CNT-RECE47
           END-IF    
      *
           .
       4501-RECE47-HENSYU-EXT.
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
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理開始処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           MOVE    2                   TO  SPA-STS
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
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
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
grpsys                 PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           CLOSE                       RECE41-FILE
           CLOSE                       RECE47-FILE
           CLOSE                       RECE46-FILE     
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *    ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *
           DISPLAY "RECE41  IN  " CNT-RECE41   
           DISPLAY "RECE47  CNT " CNT-RECE47 
           DISPLAY "*** ORCR0466 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE         NEXT
               AT  END
                   MOVE    1           TO  FLG-END
                   MOVE    2           TO  SPA-STS
               NOT AT  END
                   ADD     1           TO  CNT-RECE41
                   ADD     1           TO  CNT-IN-CANCEL
      *
                   DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
                   IF       RECE41-HKNKBN   =   "5"
                       GO  TO  990-RECE41-READ-SEC
                   END-IF
      *
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE46-NEXT-SEC           SECTION.
      *
           READ    RECE46-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE46
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE46
                   ADD     1         TO  CNT-IN-CANCEL
           END-READ
      *
           .
       990-RECE46-NEXT-EXT.
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
      *    ＤＢ　オープン処理
      *****************************************************************
       120-DBOPEN-SEC                SECTION.
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
       120-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
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
