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
       PROGRAM-ID.             ORAPI052BR7V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 診療行為一括返却
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/12/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *  05.01.01    NACL-藤原    20/02/13  診療名称（コメント）の桁数変更
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
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
           03  FLG-SYORI-END       PIC 9(01).
      *
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-TENCHK          PIC 9(01).
           03  FLG-CONFCHK         PIC 9(01).
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
           03  IDY2                PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDM2                PIC 9(04).
           03  IDM3                PIC 9(04).
           03  IDXA                PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDS                 PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDX-J               PIC 9(04).
           03  IDXM                PIC 9(04).
           03  IDXR                PIC 9(04).
           03  IDX-ACT             PIC 9(04).
           03  IDX-REN             PIC 9(04).
      *
           03  MAX-KA              PIC 9(04).
           03  MAX-DRCD            PIC 9(04).
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
           03  WRK-PARA-BASE-YMD   PIC X(08).
           03  WRK-PARA-NYUGAIKBN  PIC X(01).
           03  WRK-PARA-ERRFILE    PIC X(64).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
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
           03  WRK-PTNUM               PIC X(20).
      *
           03  WRK-SRYYMD              PIC X(08).
           03  WRK-SYSYMD              PIC X(08).
      *    名称
           03  WRK-TNS-NAME            PIC X(200).
           03  WRK-HZN-NAME            PIC X(200).
      *    剤内容
           03  WRK-KAISU               PIC 9(05).
           03  WRK-ZAITEN              PIC S9(07).
           03  WRK-SUM-TENSU           PIC 9(07).
           03  WRK-SUM-KINGAKU         PIC 9(07).
      *    包括検査数
           03  WRK-HOUKNS-CNT          PIC 9(02).
      *    単位
           03  WRK-TANICD              PIC X(03).
           03  WRK-TANINAME            PIC X(24).
      *
           03  WRK-SRYSYUKBN           PIC X(03).
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-SRYSYUKBNMEI        PIC X(30).
      *
           03  WRK-SRYKA               PIC X(02).
           03  WRK-SRYKA-NAME          PIC X(30).
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD-NAME           PIC X(100).
      *
      *    キー領域
           03  WRK-KEY-AREA.
               05  WRK-KEY-PTID            PIC X(10).
               05  WRK-KEY-SRYYMD          PIC X(08).
               05  WRK-KEY-SRYKA           PIC X(02).
               05  WRK-KEY-HKNCOMBI        PIC X(04).
               05  WRK-KEY-RENNUM          PIC X(01).
               05  WRK-KEY-DOUJI-RENNUM    PIC X(01).
               05  WRK-KEY-DENPNUM         PIC X(07).
           03  WRK-OLD-AREA.
               05  WRK-OLD-PTID            PIC X(10).
               05  WRK-OLD-SRYYMD          PIC X(08).
               05  WRK-OLD-SRYKA           PIC X(02).
               05  WRK-OLD-HKNCOMBI        PIC X(04).
               05  WRK-OLD-RENNUM          PIC X(01).
               05  WRK-OLD-DOUJI-RENNUM    PIC X(01).
               05  WRK-OLD-DENPNUM         PIC X(07).
      *
      *    数値変換用
           03  WRK-SURYO               PIC S9(08)V9(05).
           03  WRK-SURYO-S             PIC 9(08)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO-S.
               05  WRK-SURYO-S1        PIC 9(08).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO2-S1           PIC S9(08).
      *
           03  WRK-SURYO-XX            PIC X(15).
           03  WRK-SURYO-X             REDEFINES    WRK-SURYO-XX.
               05  WRK-SURYO-Z1        PIC -(08)9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC ZZZZZ.
           03  WRK-SURYO-HX            PIC X(15).
           03  WRK-S-MAX               PIC 9(02).
      *
      *    出力領域
       01  MDCRES-OUT-AREA.
           02  MDCRES-OUT-REC          OCCURS  6.
      *    471883 byte
           COPY    "CPORAPI052BR7.INC".
      *
      *    受診履歴
       01  TBL-JYURRK-AREA.
         02  TBL-JYURRK-REC            OCCURS   15.
           COPY    "CPJYURRK.INC"      REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *    診療科テーブル
       01  TBL-SRYKACD-AREA.
           03  TBL-SRYKACD-OCC         OCCURS   99.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-SRYKA-NAME      PIC X(20).
      *
      *    ドクターーブル
       01  TBL-DRCD-AREA.
           03  TBL-DRCD-OCC         OCCURS   99.
               05  TBL-DRCD           PIC X(05).
               05  TBL-DRCD-NAME      PIC X(80).
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
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    （職員情報）
           COPY    "CPSK1010.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
       01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *    コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
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
      *    保険組合せセット領域
      **   COPY    "CPORCSAPIHKNALL.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
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
       PROCEDURE               DIVISION
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
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
                                       CNT-AREA
           INITIALIZE                  TBL-SRYKACD-AREA
           INITIALIZE                  TBL-DRCD-AREA
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
                                               WRK-PARA-BASE-YMD
                                               WRK-PARA-NYUGAIKBN
           END-UNSTRING
      *
           DISPLAY "BASE-YMD =" WRK-PARA-BASE-YMD
           DISPLAY "NYUGAIKBN=" WRK-PARA-NYUGAIKBN
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *    入外区分
           MOVE    WRK-PARA-NYUGAIKBN  TO  SPA-NYUGAIKBN
      *
      *    システム日付
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  WRK-SYSYMD
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
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
           MOVE    "ORAPI052BR7V3"     TO  JOB-PGID
           MOVE    "患者診療行為情報返却"
                                       TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    受診歴を検索する  医療機関、基準日以降
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-PARA-BASE-YMD   TO  JYURRK-UPYMD
      *
           IF      SPA-NYUGAIKBN       =   "1"
      *        診療日付、患者番号順
               MOVE    "key70"             TO  WRK-MCP-PATHNAME
           ELSE
      *        診療日付、伝票番号順
               MOVE    "key66"             TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           INITIALIZE                  WRK-KEY-AREA
           INITIALIZE                  WRK-OLD-AREA
           MOVE    SPACE               TO  MDCRES-OUT-AREA
           INITIALIZE                      MDCRES-OUT-AREA
           MOVE    ZERO                TO  IDM
           MOVE    ZERO                TO  IDM2
           MOVE    ZERO                TO  IDM3
      *
           PERFORM     UNTIL   FLG-JYURRK  =   1
               MOVE    ZERO                TO  CNT-OUT1
      *
               MOVE    SPACE               TO  MDCRES-OUT-AREA
               INITIALIZE                      MDCRES-OUT-AREA
               MOVE    ZERO                TO  IDM
      *
               PERFORM         UNTIL   FLG-JYURRK  =   1
                               OR      CNT-OUT1    >=  6
                   PERFORM 2001-MAIN-SYORI-SEC
               END-PERFORM
      *
               ADD     1                   TO  CNT-XMLCOUNT
               PERFORM 910-XMLEXPORT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                  TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴処理
      *****************************************************************
       2001-MAIN-SYORI-SEC         SECTION.
      *
           INITIALIZE                  TBL-JYURRK-AREA
           MOVE    ZERO                TO  IDX-TBL
      *    キー編集
           PERFORM 2001-JYURRK-KEY-SEC
           MOVE    WRK-KEY-AREA        TO  WRK-OLD-AREA
      *
           PERFORM UNTIL  (FLG-JYURRK      =   1  )
                   OR     (WRK-KEY-AREA    NOT =   WRK-OLD-AREA)
               ADD     1                   TO  IDX-TBL
               MOVE    JYURRK-REC          TO  TBL-JYURRK-REC(IDX-TBL)
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
      *
               IF      FLG-JYURRK          =   ZERO
      *            キー編集
                   PERFORM 2001-JYURRK-KEY-SEC
               END-IF
           END-PERFORM
      *
           IF      IDX-TBL             >   ZERO
               PERFORM 2001-JYURRK-SYORI-SEC
           END-IF
           .
       2001-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴キー編集処理
      *****************************************************************
       2001-JYURRK-KEY-SEC         SECTION.
      *
           MOVE    JYURRK-PTID         TO  WRK-KEY-PTID
           MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
           MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
           MOVE    JYURRK-HKNCOMBINUM  TO  WRK-KEY-HKNCOMBI
           MOVE    JYURRK-RENNUM       TO  WRK-KEY-RENNUM
           MOVE    JYURRK-DOUJI-RENNUM TO  WRK-KEY-DOUJI-RENNUM
           IF      JYURRK-NYUGAIKBN    =   "2"
               MOVE    JYURRK-DENPNUM      TO  WRK-KEY-DENPNUM
           END-IF
      *
           .
       2001-JYURRK-KEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       2001-JYURRK-SYORI-SEC         SECTION.
      *
           ADD     1                   TO  CNT-OUT1
                                           CNT-OUT
           MOVE    ZERO                TO  IDM2
           MOVE    ZERO                TO  IDM3
           MOVE    ZERO                TO  FLG-ERR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-TBL
      *        診療日付
               MOVE    TBL-JYURRK-SRYYMD(IDX)  TO  WRK-SRYYMD
      *
               IF      IDX             =   1
                   PERFORM 20011-JYURRK-INIT-SEC
               END-IF
               PERFORM VARYING     IDX-J   FROM    1   BY  1
                       UNTIL      (IDX-J       >   15      )
                               OR (FLG-ERR     NOT =   ZERO)
                   IF      TBL-JYURRK-ZAINUM (IDX IDX-J)
                                               NOT =   ZERO
                       PERFORM 20012-ZAISET-HEN-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       2001-JYURRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       20011-JYURRK-INIT-SEC         SECTION.
      *
      *    患者情報検索
           PERFORM 200211-PTNUM-HEN-SEC
      *
           ADD     1                   TO  IDM
           MOVE    ZERO                TO  IDM2
      *    患者情報
           MOVE    WRK-PTNUM           TO  MDCRES-PATIENTID(IDM)
      *
           MOVE    PTINF-NAME          TO  MDCRES-NAME     (IDM)
           MOVE    PTINF-KANANAME      TO  MDCRES-KANANAME (IDM)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  MDCRES-BIRTHDAY  (IDM)
           MOVE    PTINF-SEX           TO  MDCRES-SEX  (IDM)
      *    診療日付
           MOVE    TBL-JYURRK-SRYYMD(IDX)  TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE            TO  MDCRES-PERFORM-DATE(IDM)
      *    連番
           MOVE    TBL-JYURRK-RENNUM (IDX) TO  MDCRES-RENNUM (IDM)
      *    伝票番号
           MOVE    TBL-JYURRK-DENPNUM(IDX) TO  MDCRES-INVOICD-NUMBER
                                                             (IDM)
      *    診療科
           PERFORM 200212-SRYKAMEI-HEN-SEC
           MOVE    TBL-JYURRK-SRYKA(IDX)   TO  MDCRES-DEP-CODE (IDM)
           MOVE    WRK-SRYKA-NAME          TO  MDCRES-DEP-NAME (IDM)
      *    ドクター
           IF     (TBL-JYURRK-DRCD (IDX)   NOT =   ZERO )
              AND (SPA-NYUGAIKBN           =   "2"      )
               PERFORM 200213-DRCD-HEN-SEC
               MOVE    WRK-DRCD            TO  MDCRES-DRCD    (IDM)
               MOVE    WRK-DRCD-NAME       TO  MDCRES-DRCD-NAME(IDM)
           END-IF
      *    保険組合せ
           MOVE    TBL-JYURRK-HKNCOMBINUM(IDX)
                                       TO  MDCRES-COMBINATION-NUMBER
                                                               (IDM)
      *    更新日付（受診履歴）
           MOVE    TBL-JYURRK-UPYMD (IDX)  TO  WRK-SYMD
           MOVE    TBL-JYURRK-UPHMS (IDX)  TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE            TO  MDCRES-UPD-DATE(IDM)
           MOVE    WRK-HEN-TIME            TO  MDCRES-UPD-TIME(IDM)
           .
       20011-JYURRK-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック
      *****************************************************************
       200211-PTNUM-HEN-SEC     SECTION.
      *
      *    患者番号取得
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    TBL-JYURRK-PTID(IDX)    TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
           IF      SPTID-RC        NOT =   ZERO
      *        患者番号エラー
               MOVE    ALL "*"             TO  WRK-PTNUM
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    TBL-JYURRK-PTID(IDX)    TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    1                   TO  FLG-ERR
           END-IF
           .
       200211-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名称編集処理
      *****************************************************************
       200212-SRYKAMEI-HEN-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL       IDK     >=  MAX-KA
                           OR (IDX-H   NOT =   ZERO )
               IF     (TBL-JYURRK-SRYKA (IDX)
                                           =   TBL-SRYKA (IDK))
                   MOVE    IDK             TO  IDX-H
                   MOVE    MAX-KA          TO  IDK
               END-IF
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        診療科検索
               MOVE    TBL-JYURRK-SRYKA(IDX)   TO   WRK-SRYKA
               PERFORM 20011-SRYKA-CHK-SEC
               ADD     1                   TO  MAX-KA
               MOVE    TBL-JYURRK-SRYKA(IDX)   TO  TBL-SRYKA (MAX-KA)
               MOVE    WRK-SRYKA-NAME      TO  TBL-SRYKA-NAME (MAX-KA)
           ELSE
               MOVE    TBL-SRYKA-NAME (IDX-H)
                                           TO  WRK-SRYKA-NAME
           END-IF
           .
       200212-SRYKAMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科コードチェック
      *****************************************************************
       20011-SRYKA-CHK-SEC         SECTION.
      *
      *    診療科の存在チェック
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    WRK-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    WRK-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1005-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-SRYKA-NAME
           ELSE
               MOVE    SPACE               TO  WRK-SRYKA-NAME
           END-IF
      *
           .
       20011-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター名称編集処理
      *****************************************************************
       200213-DRCD-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX-H
           PERFORM VARYING     IDK     FROM    1   BY  1
                   UNTIL      (IDK     >=  MAX-DRCD )
                           OR (IDX-H   NOT =   ZERO )
               IF     (TBL-JYURRK-DRCD(IDX)    =   TBL-DRCD(IDK))
                   MOVE    IDK             TO  IDX-H
               END-IF 
           END-PERFORM
           IF       IDX-H              =   ZERO
      *        ドクター検索
               MOVE    TBL-JYURRK-DRCD(IDX)    TO   WRK-DRCD
               PERFORM 2002131-DRCD-CHK-SEC
               ADD     2                       TO  MAX-DRCD
               MOVE    WRK-DRCD                TO  TBL-DRCD(MAX-DRCD)
               MOVE    WRK-DRCD-NAME           TO  TBL-DRCD-NAME
                                                            (MAX-DRCD)
           ELSE
               MOVE    TBL-JYURRK-DRCD(IDX)    TO  WRK-DRCD
               MOVE    TBL-DRCD-NAME (IDX-H)   TO  WRK-DRCD-NAME
           END-IF
           .
       200213-DRCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       2002131-DRCD-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    WRK-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    WRK-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-SEL-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-NAME
           ELSE
               MOVE    SPACE               TO  WRK-DRCD-NAME
           END-IF
      *
           .
       2002131-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤内容編集処理
      *****************************************************************
       20012-ZAISET-HEN-SEC         SECTION.
      *
      *    診療会計
           INITIALIZE                      SRYACCT-REC
           MOVE    TBL-JYURRK-HOSPNUM (IDX)    TO  ACCT-HOSPNUM
           MOVE    TBL-JYURRK-NYUGAIKBN(IDX)   TO  ACCT-NYUGAIKBN
           MOVE    TBL-JYURRK-PTID (IDX)       TO  ACCT-PTID
           MOVE    TBL-JYURRK-SRYKA (IDX)      TO  ACCT-SRYKA
           MOVE    TBL-JYURRK-SRYYMD(IDX)(1:6) TO  ACCT-SRYYM
           MOVE    TBL-JYURRK-ZAINUM (IDX IDX-J)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                      SRYACCT-REC
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    TBL-JYURRK-SRYYMD(IDX)(7:2)  TO  IDX-ACT
      *    受診連番
           COMPUTE IDX-REN             =   TBL-JYURRK-RENNUM(IDX)
                                       +   1
      *
           MOVE    ACCT-DAY (IDX-REN IDX-ACT)
                                       TO  WRK-KAISU
      *
           MOVE    ZERO                TO  FLG-CHK
      *    回数ゼロは対象外
           IF     (WRK-KAISU           =   ZERO )
               MOVE    1                   TO  FLG-CHK
           END-IF
      *
           IF     (FLG-CHK             =   1     )
               GO      TO      20012-ZAISET-HEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SUM-TENSU
           MOVE    ZERO                TO  WRK-SUM-KINGAKU
           MOVE    ZERO                TO  FLG-TENCHK
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
      *
           INITIALIZE                  SRYACCTPLUS-REC
           IF     (ACCT-ZAIREQFLG      =   "1"   )
      *        附加情報あり
               PERFORM 32012-SRYACCTPLUS-READ-SEC
           END-IF
           IF      ACCTP-PLUSKBN       =   1
               COMPUTE WRK-ZAITEN          =   ACCT-ZAITEN
                                           *   -1
           ELSE
               MOVE    ACCT-ZAITEN           TO  WRK-ZAITEN
           END-IF
      *
      *    診療行為テーブル検索
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  IDM3
           PERFORM
               UNTIL  ( FLG-SRYACT      =   1  )
               PERFORM VARYING     IDXA    FROM    1   BY  1
                       UNTIL      (IDXA    >   5   )   OR
                                  (SRY-SRYCD (IDXA)    =   SPACE)
                   PERFORM 200341-MEISE-HEN-SEC
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    金額編集
           IF     (IDM3               >   ZERO )
             AND  (IDM2               <=  50   )
               PERFORM 200342-KINGAKU-HEN-SEC
           END-IF
      *
      *    50件以上あり
           IF      FLG-ERR             =   2
               MOVE    "True"              TO
                                       MDCRES-MEDICAL-INF-OVERFLOW (IDM)
           ELSE
               MOVE    "False"             TO
                                       MDCRES-MEDICAL-INF-OVERFLOW (IDM)
           END-IF
      *
           .
       20012-ZAISET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細編集処理
      *****************************************************************
       200341-MEISE-HEN-SEC          SECTION.
      *
           ADD     1                   TO  IDM3
           IF      IDM3                =   1
               ADD     1                   TO  IDM2
      *
               IF      IDM2                >   50
                   MOVE    2               TO  FLG-ERR
                   GO  TO  200341-MEISE-HEN-EXT
               END-IF
      *
      *        診療種別編集
               PERFORM 200342-ZAINUM-HEN-SEC
           END-IF
      *    明細
           MOVE    SRY-SRYCD (IDXA)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           MOVE    TNS-NAME            TO  WRK-TNS-NAME
      *    器材商品名コード
           IF      SRY-SRYCD (IDXA)(1:3)   =   "058"
               MOVE   TNS-FORMALNAME       TO  WRK-TNS-NAME
           END-IF
      *    器材商品名の器材コード
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "7" ) AND
                  (SRY-MEISKYFLG  (IDXA)   =   "1" )
               MOVE    SPACE               TO  WRK-TNS-NAME
               STRING     "〔"             DELIMITED   BY  SIZE
                          TNS-NAME         DELIMITED   BY  "    "
                          "〕"             DELIMITED   BY  SIZE
                          INTO             WRK-TNS-NAME
               END-STRING
           END-IF
      *    患者コメント
           IF      SRY-INPUTNUM (IDXA) NOT =   ZERO
               PERFORM 200342-PTCOM-HEN-SEC
           END-IF
      *    用法コード
           IF      SRY-SRYCD (IDXA)(1:3)   =   "001"
               MOVE    SPACE               TO  WRK-HZN-NAME
               STRING     "【"             DELIMITED   BY  SIZE
                          WRK-TNS-NAME     DELIMITED   BY  "    "
                          "】"             DELIMITED   BY  SIZE
                          INTO             WRK-HZN-NAME
               END-STRING
               MOVE    WRK-HZN-NAME        TO  WRK-TNS-NAME
           END-IF
      *
      *    数量
           MOVE    SRY-SRYSURYO(IDXA)  TO  WRK-SURYO
      *    減点点数編集
           IF      TNS-TENSIKIBETU     =   7
                                       OR  8
               MOVE    ZERO                TO  WRK-SURYO
           END-IF
           IF      WRK-SURYO           =   ZERO
               MOVE    1                   TO  WRK-SURYO
           END-IF
           IF      WRK-SURYO           =   ZERO
               MOVE    SPACE               TO  WRK-SURYO-HX
           ELSE
               PERFORM 810-NUMHENKAN-SEC
           END-IF
      *
           MOVE    SRY-SRYCD (IDXA)    TO  MDCRES-PRSCRPT-CODE
                                                       (IDM IDM2 IDM3)
           MOVE    WRK-TNS-NAME        TO  MDCRES-PRSCRPT-NAME
                                                       (IDM IDM2 IDM3)
           MOVE    WRK-SURYO-HX        TO  MDCRES-PRSCRPT-NUMBER
                                                       (IDM IDM2 IDM3)
      *
      *    単位
           PERFORM 200341-TANINAME-HEN-SEC
           MOVE    WRK-TANICD          TO  MDCRES-UNIT-CODE
                                                       (IDM IDM2 IDM3)
           MOVE    WRK-TANINAME        TO  MDCRES-UNIT-NAME
                                                       (IDM IDM2 IDM3)
      *
      *    診療コードの金額集計
           IF      SRY-SRYKBN          =   "95"
                                       OR  "96"
               MOVE    SRY-JIHIMONEYTOTAL      TO  WRK-SUM-KINGAKU
           ELSE
               IF     (SRY-SRYCD (IDXA)(1:1)   =   "1"  )
                   EVALUATE    TNS-TENSIKIBETU
                       WHEN    3
                           ADD     TNS-TEN         TO  WRK-SUM-TENSU
                       WHEN    1
                           ADD     TNS-TEN         TO  WRK-SUM-KINGAKU
                   END-EVALUATE
               ELSE
                   IF      TNS-TEN             >   ZERO
                       MOVE    1               TO  FLG-TENCHK
                   END-IF
               END-IF
      *        自賠責のその他
               IF     (SRY-SRYCD (IDXA)(1:4)   =   "0959" )
      *            自賠責金額入力を点数とする
                   IF     (SRY-SRYSYUKBN       =   "809")
                       MOVE    SRY-JIHIMONEY(IDXA)     TO  TNS-TEN
                   END-IF
                   ADD     TNS-TEN             TO  WRK-SUM-KINGAKU
               END-IF
           END-IF
      *
      *    包括検査の判定
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "1" )  AND
                  (SRY-SRYKBN              =   "60" ) AND
                  (TNS-DATAKBN             =   1    ) AND
                  (TNS-HOUKNSKBN       NOT =   ZERO )
               ADD     1                   TO  WRK-HOUKNS-CNT
           END-IF
      *
           .
       200341-MEISE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤内容編集処理
      *****************************************************************
       200342-ZAINUM-HEN-SEC          SECTION.
      *
      *    診療種別編集
           IF      SRY-SRYSYUKBN       =   SPACE
               MOVE    SRY-SRYKBN          TO  WRK-SRYSYUKBN(1:2)
               MOVE    "0"                 TO  WRK-SRYSYUKBN(3:1)
           ELSE
               MOVE    SRY-SRYSYUKBN       TO  WRK-SRYSYUKBN
           END-IF
           MOVE    WRK-SRYSYUKBN       TO  MDCRES-MDC-CLASS
                                                       (IDM IDM2)
      *    診療種別区分名称
           PERFORM 200341-SRYKBN-MEI-SEC
           MOVE    WRK-SRYSYUKBNMEI   TO  MDCRES-MDC-CLASS-NAME
                                                       (IDM IDM2)
      *    回数
           MOVE    WRK-KAISU           TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MDC-CLASS-NUMBER
                                                       (IDM IDM2)
      *
           MOVE    "0"                 TO  MDCRES-MDC-CODE (IDM IDM2)
      *    薬評分
           IF      ACCT-ZAIKBN         =   1
               MOVE    "2"             TO  MDCRES-MDC-CODE (IDM IDM2)
           END-IF
      *    包括区分
           IF      ACCT-JIHOKBN        =   "1"
               MOVE    "1"             TO  MDCRES-MDC-CODE  (IDM IDM2)
               MOVE    "True"          TO  MDCRES-INCLUSION-CLASS
                                                            (IDM IDM2)
           END-IF
           .
       200342-ZAINUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤点数編集処理
      *****************************************************************
       200342-KINGAKU-HEN-SEC          SECTION.
      *
      *    診療コードの金額集計
           IF      ACCT-SRYKBN         =   "95"
                                       OR  "96"
               MOVE    ZERO                TO  WRK-ZAITEN
           ELSE
               IF     (WRK-SUM-KINGAKU     =   ZERO )
                OR    (FLG-TENCHK          =   1    )
                   MOVE    ZERO                TO  WRK-SUM-KINGAKU
               ELSE
      *            金額有
                   MOVE    WRK-SUM-TENSU       TO  WRK-ZAITEN
               END-IF
           END-IF
      *    点数
           MOVE    WRK-ZAITEN          TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MDC-POINT
                                                             (IDM IDM2)
      *    金額（自費・労災円）
           MOVE    WRK-SUM-KINGAKU     TO  WRK-SURYO
           PERFORM 810-NUMHENKAN-SEC
           MOVE    WRK-SURYO-HX(1:WRK-S-MAX)
                                       TO  MDCRES-MDC-MOENY
                                                             (IDM IDM2)
      *
      *    包括検査の検査数
           IF      WRK-HOUKNS-CNT      >   ZERO
               MOVE    WRK-HOUKNS-CNT  TO  MDCRES-HKTKNSA-CNT
                                                             (IDM IDM2)
           END-IF
           .
       200342-KINGAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     単位名称編集処理
      *****************************************************************
       200341-TANINAME-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-TANICD
           MOVE    SPACE               TO  WRK-TANINAME
      *    薬剤・器材
           IF     (SRY-SRYCD (IDXA)(1:1)   =   "6"
                                           OR  "7" )
               MOVE    TNS-TANICD          TO  WRK-TANICD
               MOVE    TNS-TANINAME        TO  WRK-TANINAME
           END-IF
      *    診療コード・商品コードは単位ありの時
           IF     (TNS-TANICD          =   ZERO
                                       OR  SPACE )
               CONTINUE
           ELSE
               IF     (SRY-SRYCD (IDXA)(1:1)   =   "1"  ) 
                  OR  (SRY-SRYCD (IDXA)(1:3)   =   "059"
                                               OR  "058")
                   MOVE    TNS-TANICD          TO  WRK-TANICD
                   MOVE    TNS-TANINAME        TO  WRK-TANINAME
               END-IF
           END-IF
           .
       200341-TANINAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       200341-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       200341-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントから編集する処理
      *****************************************************************
       200342-PTCOM-HEN-SEC          SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDXA)    TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDXA)  TO  PTCOM-RENNUM
      *    主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WRK-TNS-NAME
           END-IF
           .
       200342-PTCOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT 処理
      *****************************************************************
       910-XMLEXPORT-SEC          SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE   MDCRES-OUT-AREA  TO  XMLDATA-DATA
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_patient_medical_all"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "Patient_Medical_All"
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
           STRING  "patient_medical_all"
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
      *****************************************************************
      *    数値変換編集処理
      *****************************************************************
       810-NUMHENKAN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           IF      WRK-SURYO           <   ZERO
               COMPUTE WRK-SURYO-S         =   WRK-SURYO      *  -1
               COMPUTE WRK-SURYO2-S1       =   WRK-SURYO-S1   *  -1
               MOVE    WRK-SURYO2-S1       TO  WRK-SURYO-Z1
           ELSE
               MOVE    WRK-SURYO           TO  WRK-SURYO-S
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDXM    FROM    5  BY  -1
                   UNTIL       IDXM    <   1
               IF      WRK-SURYO-S2(IDXM:1)    NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDXM:1)    TO  WRK-SURYO-Z3
                                                             (IDXM:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SURYO-Z2
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
           END-IF
      *    左詰
           MOVE    SPACE              TO  WRK-SURYO-HX
           MOVE    1                  TO  IDXR
           MOVE    ZERO               TO  WRK-S-MAX
           PERFORM VARYING     IDXM    FROM    1    BY  1
                   UNTIL       IDXM    >   15
               IF      WRK-SURYO-X (IDXM:1)    NOT =   SPACE
                   MOVE    WRK-SURYO-X (IDXM:1)    TO  WRK-SURYO-HX
                                                       (IDXR:1)
                   COMPUTE IDXR            =   IDXR    +   1
      *            桁数
                   ADD     1               TO  WRK-S-MAX
               END-IF
           END-PERFORM
           .
       810-NUMHENKAN-EXT.
           EXIT.
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
               MOVE    "patient_medical_all"
                                       TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               MOVE    "2"             TO  SAPIFILE-IN-KBN
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    "patient_medical_all.xml"
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
           DISPLAY "*** ORAPI052BR7V3 OUT:" CNT-OUT
           DISPLAY "*** ORAPI052BR7V3 END ***"
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
      *    患者番号マスター読込処理
      *****************************************************************
       900-PTNUM-READ-SEC            SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTNUM
               MOVE    MCPDATA-REC         TO  PTNUM-REC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                      PTNUM-REC
           END-IF
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報取得
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
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
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       910-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       920-SRYACT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       930-SRYACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       32012-SRYACCTPLUS-READ-SEC       SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   INITIALIZE                      SRYACCTPLUS-REC
                   MOVE    1                   TO  FLG-SRYACCTPLUS
               END-IF
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       32012-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
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
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SEL-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI         =   ZERO
                   MOVE    MCPDATA-REC      TO  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    点数附加情報検索
           IF     (FLG-TENSU           =   ZERO)
               PERFORM 9101-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *R02.2
      *    ユーザー単位
           IF     (FLG-TENSUPLUS       =   ZERO)
              AND (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7" )
              AND (TNSP-USERTANICD NOT =   SPACE)
                MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
           END-IF
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORAPI053BR7V3 "    DELIMITED  BY  SIZE
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
