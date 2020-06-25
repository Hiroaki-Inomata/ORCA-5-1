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
       PROGRAM-ID.             ORAPI052BR4V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者基本情報一括返却
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/10/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  18/10/XX  認知症地域包括設定など追加
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
           03  FLG-PTPUBLIC        PIC 9(01).
           03  FLG-PTCARE-HKNINF   PIC 9(01).
           03  FLG-PTCARE-NINTEI   PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-LABOR-SIO       PIC 9(01).
           03  FLG-PTMYNUMBER      PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
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
           03  WRK-PARA-ERRFILE    PIC X(64).
      *    1:更新、新規、2:新規
           03  WRK-PARA-SYORIKBN   PIC X(01).
      *    患者情報更新日付対象
           03  WRK-PARA-START-YMD  PIC X(08).
           03  WRK-PARA-END-YMD    PIC X(08).
           03  WRK-PARA-SYS-YMD.
               05  WRK-SYSYM       PIC X(06).
               05  WRK-SYSDD       PIC X(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *    割引率
           03  WRK-RATE-EDIT.
               05  WRK-RATE-EDITZ      PIC ZZZZZZZ.
               05  WRK-RATE-TANI       PIC X(04).
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
       01  PTINFLST-REC.
           02  PTINFLST-OCC        OCCURS  39.
           COPY    "CPORAPI012BR1.INC".
      *
       01  PTINFLST-MAX            PIC 9(02)   VALUE   39.
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
      *    職員情報
           COPY  "CPSK1010.INC".
      *
           COPY  "CPSK4002.INC".
      *    状態情報
           COPY    "CPSK1018.INC".
      *    減免事由情報
           COPY    "CPSK1026.INC".
      *    割引率情報
           COPY    "CPSK1027.INC".
      *    入金方法
           COPY    "CPSK1041.INC".
      *
      *    患者マスタ−
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者地域連携テーブル
      *01  PTPUBLIC-REC.
      *    COPY    "CPPTNUM-PUBLIC.INC".
      *    患者介護保険情報テーブル
      *01  PTCARE-HKNINF-REC.
      *    COPY    "CPPTCARE-HKNINF.INC".
      *    患者介護認定者情報テーブル
      *01  PTCARE-NINTEI-REC.
      *    COPY    "CPPTCARE-NINTEI.INC".
      *    患者個別設定テーブル
      *01  PTCONF-REC.
      *    COPY    "CPPTCONF.INC".
      *    受診履歴
      *01  JYURRK-REC.
      *    COPY    "CPJYURRK.INC".
      *
      *    患者個人番号テーブル
      *01  PTMYNUMBER-REC.
      *    COPY    "CPPTMYNUMBER.INC".
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
           COPY    "CPORCSAPIHKNALL.INC".
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *    患者地域連携等編集パラメタ
           COPY    "CPORAPI012RSUB1.INC".
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
                                               WRK-PARA-SYORIKBN
                                               WRK-PARA-START-YMD
                                               WRK-PARA-END-YMD
                                               WRK-PARA-SYS-YMD
           END-UNSTRING
      *
           DISPLAY "SYORIKBN =" WRK-PARA-SYORIKBN
           DISPLAY "START-YMD=" WRK-PARA-START-YMD
           DISPLAY "END-YMD  =" WRK-PARA-END-YMD
           DISPLAY "BASE-YMD =" WRK-PARA-SYS-YMD
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *R02.1
           MOVE    WRK-PARA-SYS-YMD    TO  SPA-SYSYMD
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
           MOVE    "ORAPI052BR4V3"     TO  JOB-PGID
           MOVE    "患者基本情報返却"  TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
      *    作成年月日
           MOVE    WRK-PARA-START-YMD      TO  PTNUM-CREYMD
      *    更新年月日
           MOVE    WRK-PARA-START-YMD      TO  PTNUM-UPYMD
           IF      WRK-PARA-SYORIKBN   =   "1"
      *        更新・登録
               MOVE    "key6"              TO  WRK-MCP-PATHNAME
           ELSE
      *        登録
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTNUM-READ-SEC
           END-IF
      *
           PERFORM UNTIL     (FLG-PTNUM    =   1    )
                         OR  (FLG-END      =   1    )
               MOVE    ZERO                TO  CNT-OUT1
      *
               MOVE    SPACE               TO  PTINFLST-REC
      *
               PERFORM         UNTIL ( FLG-PTNUM    =   1  )
                               OR    ( FLG-END      =   1  )
                               OR    ( CNT-OUT1     >=  PTINFLST-MAX )
                   MOVE    ZERO                TO  FLG-SYORI-END
      *
                   IF      WRK-PARA-SYORIKBN   =   "1"
      *                更新・登録日　対象判定
                       MOVE    1               TO  FLG-SYORI-END
                       IF     (PTNUM-CREYMD    NOT =   SPACE      )
                          IF      (PTNUM-CREYMD
                                               >=  WRK-PARA-START-YMD)
                            AND   (PTNUM-CREYMD
                                               <=  WRK-PARA-END-YMD  )
                               MOVE    ZERO            TO  FLG-SYORI-END
                          END-IF
                       END-IF
                       IF     (PTNUM-UPYMD     >=  WRK-PARA-START-YMD)
                         AND  (PTNUM-UPYMD     <=  WRK-PARA-END-YMD  )
                           MOVE    ZERO            TO  FLG-SYORI-END
                       END-IF
                   ELSE
      *                登録判定
                       IF     (PTNUM-CREYMD        >   WRK-PARA-END-YMD)
                           MOVE    1               TO  FLG-SYORI-END
                       END-IF
                   END-IF
      *
                   IF      FLG-SYORI-END       =   ZERO
                       PERFORM 2001-PTINFLST-SYORI-SEC
                   END-IF
      *
                   MOVE    "tbl_ptnum"         TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-PTNUM-READ-SEC
               END-PERFORM
      *
               IF      CNT-OUT1        >   ZERO
                   ADD     1                   TO  CNT-XMLCOUNT
                   PERFORM 910-XMLEXPORT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者基本情報編集処理
      *****************************************************************
       2001-PTINFLST-SYORI-SEC         SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTNUM-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               GO      TO      2001-PTINFLST-SYORI-EXT
           END-IF
      *
           ADD     1                   TO  CNT-OUT1
                                           CNT-OUT
           MOVE    CNT-OUT1            TO  IDZ
      *
      *    患者情報
           PERFORM 20011-PTINF-HEN-SEC
      *
      *    保険組合せ編集処理
           PERFORM 20012-HKNCOMBI-HEN-SEC
      *    地域連携など編集処理
           PERFORM 20013-ETC-HEN-SEC
           .
       2001-PTINFLST-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       20011-PTINF-HEN-SEC                SECTION.
      *
      *    患者番号
           MOVE    PTNUM-PTNUM         TO  PTINFLST-PATIENTID      (IDZ)
      *
           MOVE    PTINF-NAME          TO  PTINFLST-NAME           (IDZ)
           MOVE    PTINF-KANANAME      TO  PTINFLST-KANANAME       (IDZ)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PTINFLST-BIRTHDAY       (IDZ)
           MOVE    PTINF-SEX           TO  PTINFLST-SEX            (IDZ)
           MOVE    PTINF-HOME-POST     TO  PTINFLST-HOME-ZIP-CODE  (IDZ)
           MOVE    PTINF-HOME-ADRS     TO  PTINFLST-HOME-ADDRESSES1(IDZ)
           MOVE    PTINF-HOME-BANTI    TO  PTINFLST-HOME-ADDRESSES2(IDZ)
           MOVE    PTINF-HOME-TEL1     TO  PTINFLST-HOME-HPONE1    (IDZ)
           MOVE    PTINF-HOME-TEL2     TO  PTINFLST-HOME-HPONE2    (IDZ)
      *
      *    世帯主名
           MOVE    PTINF-SETAINUSI     TO  PTINFLST-SETAI-NAME     (IDZ)
      *    続柄
           MOVE    PTINF-ZOKUGARA      TO  PTINFLST-ZOKUGARA       (IDZ)
      *    禁忌
           MOVE    PTINF-TABOO-1       TO  PTINFLST-TABOO-1        (IDZ)
           MOVE    PTINF-TABOO-2       TO  PTINFLST-TABOO-2        (IDZ)
      *    アレルギー
           MOVE    PTINF-ALLERGY-1     TO  PTINFLST-ALLERGY-1      (IDZ)
           MOVE    PTINF-ALLERGY-2     TO  PTINFLST-ALLERGY-2      (IDZ)
      *    感染症
           MOVE    PTINF-KANSENSYO-1   TO  PTINFLST-KANSENSYO-1    (IDZ)
           MOVE    PTINF-KANSENSYO-2   TO  PTINFLST-KANSENSYO-2    (IDZ)
      *    コメント
           MOVE    PTINF-COMMENT-1     TO  PTINFLST-COMMENT-1      (IDZ)
           MOVE    PTINF-COMMENT-2     TO  PTINFLST-COMMENT-2      (IDZ)
      *
      *    勤務先情報
           MOVE    PTINF-OFFICE-NAME   TO  PTINFLST-OFFICE-NAME    (IDZ)
           MOVE    PTINF-OFFICE-POST   TO  PTINFLST-OFFICE-ZIP-CODE(IDZ)
           MOVE    PTINF-OFFICE-ADRS   TO  PTINFLST-OFFICE-ADDRESSES1
                                                                   (IDZ)
           MOVE    PTINF-OFFICE-BANTI  TO  PTINFLST-OFFICE-ADDRESSES2
                                                                   (IDZ)
           MOVE    PTINF-OFFICE-TEL    TO  PTINFLST-OFFICE-HPONE   (IDZ)
      *    連絡先情報
           MOVE    PTINF-RENRAKU-NAME  TO  PTINFLST-RENRAKU-NAME   (IDZ)
           MOVE    PTINF-RENRAKU-ZOKUGARA
                                       TO  PTINFLST-RENRAKU-ZOKUGARA
                                                                   (IDZ)
           MOVE    PTINF-RENRAKU-POST  TO  PTINFLST-RENRAKU-ZIP-CODE
                                                                   (IDZ)
           MOVE    PTINF-RENRAKU-ADRS  TO  PTINFLST-RENRAKU-ADDRESSES1
                                                                   (IDZ)
           MOVE    PTINF-RENRAKU-BANTI TO  PTINFLST-RENRAKU-ADDRESSES2
                                                                   (IDZ)
           MOVE    PTINF-RENRAKU-TEL1  TO  PTINFLST-RENRAKU-HPONE1 (IDZ)
           MOVE    PTINF-RENRAKU-TEL2  TO  PTINFLST-RENRAKU-HPONE2 (IDZ)
      *    帰省先
           MOVE    PTINF-KISEI-NAME    TO  PTINFLST-KISEI-NAME     (IDZ)
           MOVE    PTINF-KISEI-POST    TO  PTINFLST-KISEI-ZIP-CODE (IDZ)
           MOVE    PTINF-KISEI-ADRS    TO  PTINFLST-KISEI-ADDRESSES1
                                                                   (IDZ)
           MOVE    PTINF-KISEI-BANTI   TO  PTINFLST-KISEI-ADDRESSES2
                                                                   (IDZ)
           MOVE    PTINF-KISEI-TEL     TO  PTINFLST-KISEI-HPONE    (IDZ)
      *
      *    テスト患者番号区分
           MOVE    PTINF-TSTPTNUMKBN   TO  PTMODRES-TEST-PATIENT   (IDZ)
           IF      PTINF-TSTPTNUMKBN   =   SPACE
               MOVE    "0"                 TO  PTMODRES-TEST-PATIENT
                                                                   (IDZ)
           END-IF
      *    死亡区分
           MOVE    PTINF-DEATHKBN      TO  PTMODRES-DEATHKBN       (IDZ)
      *
      *    通称名
           MOVE    PTINF-NICKNAME      TO  PTINFLST-NICKNAME       (IDZ)
      *    職業
           MOVE    PTINF-JOB           TO  PTINFLST-JOB            (IDZ)
      *    ＦＡＸ番号
           MOVE    PTINF-FAX           TO  PTINFLST-FAX            (IDZ)
      *    携帯
           MOVE    PTINF-KEITAI-TEL    TO  PTINFLST-KEITAI-TEL     (IDZ)
      *    メール
           MOVE    PTINF-EMAIL         TO  PTINFLST-EMAIL          (IDZ)
      *    減免事由
           MOVE    PTINF-DISCOUNT-KBN  TO  PTINFLST-DISCOUNTKBN    (IDZ)
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1026"              TO  SYS-KANRICD
           MOVE    PTINF-DISCOUNT-KBN  TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1026-REC
               MOVE    SYS-1026-DISCOUNT-BODY
                                           TO  PTINFLST-DISCOUNTKBN-NAME
                                                                   (IDZ)
           END-IF
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  PTINFLST-DISCOUNT       (IDZ)
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1027"              TO  SYS-KANRICD
           MOVE    PTINF-DISCOUNT      TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEYREAD-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1027-REC
               MOVE    SPACE               TO  WRK-RATE-EDIT
               MOVE    SYS-1027-DISCOUNT-RATE
                                       TO  WRK-RATE-EDITZ
               EVALUATE    SYS-1027-TEIRITU
               WHEN    "0"
                   MOVE    "該当なし"      TO  PTINFLST-DISCOUNT-NAME
                                                                   (IDZ)
               WHEN    "1"
                   MOVE    "(%)"           TO  WRK-RATE-TANI
                   MOVE    WRK-RATE-EDIT   TO  PTINFLST-DISCOUNT-NAME
                                                                   (IDZ)
               WHEN    "2"
                   MOVE    "(円)"          TO  WRK-RATE-TANI
                   MOVE    WRK-RATE-EDIT   TO  PTINFLST-DISCOUNT-NAME
                                                                   (IDZ)
               END-EVALUATE
           END-IF
      *    状態１〜３
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX       >  3        )
               INITIALIZE                      SYSKANRI-REC
               EVALUATE    IDX
               WHEN    1
                   MOVE    "1018"              TO  SYS-KANRICD
                   MOVE    PTINF-CONDITION-1   TO  SYS-KBNCD
                   MOVE    PTINF-CONDITION-1   TO
                                               PTINFLST-CONDITION-KBN
                                                               (IDZ IDX)
               WHEN    2
                   MOVE    "1019"              TO  SYS-KANRICD
                   MOVE    PTINF-CONDITION-2   TO  SYS-KBNCD
                   MOVE    PTINF-CONDITION-2   TO
                                               PTINFLST-CONDITION-KBN
                                                               (IDZ IDX)
               WHEN    3
                   MOVE    "1020"              TO  SYS-KANRICD
                   MOVE    PTINF-CONDITION-3   TO  SYS-KBNCD
                   MOVE    PTINF-CONDITION-3   TO
                                               PTINFLST-CONDITION-KBN
                                                               (IDZ IDX)
               END-EVALUATE
               PERFORM 900-SYSKANRI-KEYREAD-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC    TO  SYS-1018-REC
                   MOVE    SYS-1018-CONDITION-BODY
                                           TO  PTINFLST-CONDITION-NAME
                                                               (IDZ IDX)
               END-IF
           END-PERFORM
      *    入金方法
           MOVE    PTINF-NYUKIN-HOHO   TO  PTINFLST-IC-CODE        (IDZ)
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1041"              TO  SYS-KANRICD
           MOVE    PTINF-NYUKIN-HOHO   TO  SYS-KBNCD
           PERFORM 900-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1041-REC
               MOVE    SYS-1041-NYKN-TANNAME
                                           TO  PTINFLST-IC-CODE-NAME
                                                                   (IDZ)
           END-IF
           .
       20011-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20012-HKNCOMBI-HEN-SEC                SECTION.
      *
           MOVE    PTINF-PTID          TO  SPA-PTID
      *    保険組合せ編集処理（全件）
           INITIALIZE                      ORCSAPIHKNALLAREA
           MOVE    SPACE               TO  ORCSAPIHKN-KBN
           CALL    "ORCSAPIHKNALL"     USING
                                       SPA-AREA
                                       ORCSAPIHKNALLAREA
      *
           IF      ORCSAPIHKN-ERRCD    =   1
      *        200件以上ありは、基準年月のみ対象
               INITIALIZE                      ORCSAPIHKNALLAREA
               MOVE    SPACE               TO  ORCSAPIHKN-KBN
               MOVE    WRK-PARA-SYS-YMD          TO  ORCSAPIHKN-SYSYMD
      *        基準始年月
               MOVE    WRK-PARA-SYS-YMD(1:6)     TO  ORCSAPIHKN-START-YM
               MOVE    WRK-PARA-SYS-YMD(1:6)     TO  ORCSAPIHKN-END-YM
               CALL    "ORCSAPIHKNALL"     USING
                                           SPA-AREA
                                           ORCSAPIHKNALLAREA
           END-IF
      *    出力領域保険情報編集
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   ORCSAPIHKN-OT-MAX)
                          OR  (IDY     >=  40      )
               IF      ORCSAPIHKN-OT-MAX   >   40
                   IF     (ORCSAPIHKN-COMBN-TEKSTYMD(IDX)(1:6)
                                           <=  WRK-PARA-SYS-YMD(1:6))
                      AND (ORCSAPIHKN-COMBN-TEKEDYMD(IDX)(1:6)
                                           >=  WRK-PARA-SYS-YMD(1:6))
      *                対象  
                       MOVE    1               TO  FLG-OK
                   ELSE
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-OK
               END-IF
               IF      FLG-OK         =   1
                   PERFORM 200121-HKNHEN-TBL-SEC
               END-IF
           END-PERFORM
           .
       20012-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200121-HKNHEN-TBL-SEC                SECTION.
      *
           ADD     1               TO  IDY
      *
      *    保険組合せ
           MOVE    ORCSAPIHKN-THKNCOMBI  (IDX)
                                   TO  PTINFLST-COMB-NUMBER    (IDZ IDY)
      *    保険組合せ有効開始日
           MOVE    ORCSAPIHKN-COMB-TEKSTYMD (IDX)
                                   TO  PTINFLST-COMB-START-DATE
                                                               (IDZ IDY)
           MOVE    ORCSAPIHKN-COMB-TEKEDYMD (IDX)
                                   TO  PTINFLST-COMB-END-DATE
                                                               (IDZ IDY)
      *    保険組合せ負担割合（入院）
           MOVE    ORCSAPIHKN-COMB-RATEN (IDX)
                                   TO  PTINFLST-COMB-RATE-ADM
                                                               (IDZ IDY)
      *    保険組合せ負担割合（外来）
           MOVE    ORCSAPIHKN-COMB-RATEG (IDX)
                                   TO  PTINFLST-COMB-RATE-OUT
                                                               (IDZ IDY)
      *    保険組合せ非表示
           EVALUATE    ORCSAPIHKN-HYOJIKBN (IDX)
               WHEN    "2"
      *            入院非表示
                   MOVE    "I"         TO  PTINFLST-COMB-HYOJI (IDZ IDY)
               WHEN    "3"
      *            外来非表示
                   MOVE    "O"         TO  PTINFLST-COMB-HYOJI (IDZ IDY)
               WHEN    OTHER
      *            非表示なし
                   MOVE    "N"         TO  PTINFLST-COMB-HYOJI (IDZ IDY)
           END-EVALUATE
      *
      *    保険の種類
           MOVE    ORCSAPIHKN-HKNNUM (IDX)
                                   TO  PTINFLST-MAIN-CLASS
                                                               (IDZ IDY)
      *    保険者番号
           MOVE    ORCSAPIHKN-HKNJANUM (IDX)
                                   TO  PTINFLST-INSURANCE-NUMBER
                                                               (IDZ IDY)
      *    保険の名称
           MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX)
                                   TO  PTINFLST-MAIN-NAME
                                                               (IDZ IDY)
      *    記号
           MOVE    ORCSAPIHKN-KIGO  (IDX)
                                   TO  PTINFLST-MAIN-MARK      (IDZ IDY)
      *    番号
           MOVE    ORCSAPIHKN-NUM  (IDX)
                                   TO  PTINFLST-MAIN-NUMBER    (IDZ IDY)
      *    継続区分
           MOVE    ORCSAPIHKN-CONTKBN  (IDX)
                                   TO  PTINFLST-MAIN-CONTINUATION
                                                               (IDZ IDY)
      *    補助区分
           MOVE    ORCSAPIHKN-HOJOKBN  (IDX)
                                   TO  PTINFLST-MAIN-ASSISTANCE
                                                               (IDZ IDY)
      *    補助区分名称
           MOVE    ORCSAPIHKN-HOJOKBN-NAME  (IDX)
                                   TO  PTINFLST-MAIN-ASSISTANCE-NAME
                                                               (IDZ IDY)
      *    本人家族区分
           MOVE    ORCSAPIHKN-HONKZKKBN  (IDX)
                                   TO  PTINFLST-MAIN-FAMILY    (IDZ IDY)
      *    被保険者名
           MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX)
                                   TO  PTINFLST-MAIN-POLICYHOLDER
                                                               (IDZ IDY)
      *    有効開始日
           MOVE    ORCSAPIHKN-TEKSTYMD  (IDX)
                                   TO  PTINFLST-MAIN-START-DATE(IDZ IDY)
      *    有効終了日
           MOVE    ORCSAPIHKN-TEKEDYMD  (IDX)
                                   TO  PTINFLST-MAIN-END-DATE  (IDZ IDY)
      *    確認年月日
           MOVE    ORCSAPIHKN-KAKUNINYMD  (IDX)
                                   TO  PTINFLST-MAIN-CHECK-DATE
                                                               (IDZ IDY)
      *    資格取得日
           MOVE    ORCSAPIHKN-SKKGETYMD (IDX)
                                   TO  PTINFLST-MAIN-GET-DATE  (IDZ IDY)
      *
      *    公費情報
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL       IDY2    >   4
      *        公費の種類
               MOVE    ORCSAPIHKN-KOHNUM (IDX IDY2)
                                       TO  PTINFLST-PUBLIC-CLASS
                                                       (IDZ IDY IDY2)
      *        公費の種類名称
               MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX IDY2)
                                       TO  PTINFLST-PUBLIC-NAME
                                                       (IDZ IDY IDY2)
      *        負担者番号
               MOVE    ORCSAPIHKN-FTNJANUM (IDX IDY2)
                                       TO  PTINFLST-PROVIDER-NUMBER
                                                       (IDZ IDY IDY2)
      *        受給者番号
               MOVE    ORCSAPIHKN-JKYSNUM (IDX IDY2)
                                       TO  PTINFLST-RECIPIENT-NUMBER
                                                       (IDZ IDY IDY2)
      *        有効開始日
               MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX IDY2)
                                       TO  PTINFLST-PUBLIC-START-DATE
                                                       (IDZ IDY IDY2)
      *        有効終了日
               MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX IDY2)
                                       TO  PTINFLST-PUBLIC-END-DATE
                                                       (IDZ IDY IDY2)
      *R01.6
      *        確認年月日
               MOVE    ORCSAPIHKN-KOH-KAKUNINYMD  (IDX IDY2)
                                       TO  PTINFLST-PUBLIC-CHECK-DATE
                                                       (IDZ IDY IDY2)
      *        入院−負担率（割）
               MOVE    ORCSAPIHKN-RATE-ADMISSION (IDX IDY2)
                                       TO  PTINFLST-RATE-ADMISSION
                                                       (IDZ IDY IDY2)
      *        入院−固定額
               MOVE    ORCSAPIHKN-MONEY-ADMISSION (IDX IDY2)
                                       TO  PTINFLST-MONEY-ADMISSION
                                                       (IDZ IDY IDY2)
      *        外来−負担率（割）
               MOVE    ORCSAPIHKN-RATE-OUTPATIENT (IDX IDY2)
                                       TO  PTINFLST-RATE-OUTPATIENT
                                                       (IDZ IDY IDY2)
      *        外来−固定額
               MOVE    ORCSAPIHKN-MONEY-OUTPATIENT (IDX IDY2)
                                       TO  PTINFLST-MONEY-OUTPATIENT
                                                       (IDZ IDY IDY2)
           END-PERFORM
      *
      *    労災保険名称
           MOVE    ORCSAPIHKN-ACCIDENT-NAME (IDX)
                                       TO  PTINFLST-ACCIDENT-NAME
                                                               (IDZ IDY)
      *    傷病の部位
           MOVE    ORCSAPIHKN-DISEASE-LOCATION (IDX)
                                       TO  PTINFLST-DISEASE-LOCATION
                                                               (IDZ IDY)
      *    傷病年月日
           MOVE    ORCSAPIHKN-DISEASE-DATE (IDX)
                                       TO  PTINFLST-DISEASE-DATE
                                                               (IDZ IDY)
      *労災
      *    労働保険番号
           MOVE    ORCSAPIHKN-ACCIDENT-NUMBER (IDX)
                                       TO  PTINFLST-ACCIDENT-NUMBER
                                                               (IDZ IDY)
      *    年金証書番号
           MOVE    ORCSAPIHKN-PENSIONCERTIFICATE (IDX)
                                       TO  PTINFLST-PENSIONCERTIFICATE
                                                               (IDZ IDY)
      *    災害区分（名称）
           MOVE    ORCSAPIHKN-ACCIDENT-CLASS (IDX)
                                       TO  PTINFLST-ACCIDENT-CLASS
                                                               (IDZ IDY)
      *    労働基準監督署コード
           MOVE    ORCSAPIHKN-LABOR-ST-CODE (IDX)
                                       TO  PTINFLST-LABOR-ST-CODE
                                                               (IDZ IDY)
           MOVE    ORCSAPIHKN-LABOR-ST-NAME (IDX)
                                       TO  PTINFLST-LABOR-ST-NAME
                                                               (IDZ IDY)
      *    事業所名称
           MOVE    ORCSAPIHKN-RSI-JIGYOUNAME (IDX)
                                       TO  PTINFLST-RSI-JIGYOUNAME
                                                               (IDZ IDY)
      *    所在地都道府県名
           MOVE    ORCSAPIHKN-RSI-PREFNAME (IDX)
                                       TO  PTINFLST-RSI-PREFNAME
                                                               (IDZ IDY)
      *    所在地都道府県区分
           MOVE    ORCSAPIHKN-RSI-PREFKBN (IDX)
                                       TO  PTINFLST-RSI-PREFKBN
                                                               (IDZ IDY)
      *    所在地都道府県区分名称
           MOVE    ORCSAPIHKN-RSI-PREFKBNNAME (IDX)
                                       TO  PTINFLST-RSI-PREFKBNNAME
                                                               (IDZ IDY)
      *    所在地郡市区名
           MOVE    ORCSAPIHKN-RSI-CITYNAME (IDX)
                                       TO  PTINFLST-RSI-CITYNAME
                                                               (IDZ IDY)
      *    所在地郡市区区分
           MOVE    ORCSAPIHKN-RSI-CITYKBN (IDX)
                                       TO  PTINFLST-RSI-CITYKBN
                                                               (IDZ IDY)
      *    所在地郡市区名称
           MOVE    ORCSAPIHKN-RSI-CITYKBNNAME (IDX)
                                       TO  PTINFLST-RSI-CITYKBNNAME
                                                               (IDZ IDY)
      *自賠責
      *    自賠責保険会社名
           MOVE    ORCSAPIHKN-RSI-HOKENNAME (IDX)
                                       TO  PTINFLST-RSI-HOKENNAME
                                                               (IDZ IDY)
      *アフターケア
      *    健康管理手帳番号
           MOVE    ORCSAPIHKN-RSI-KANRITCHO-NUMBER (IDX)
                                       TO  PTINFLST-RSI-KANRITCHO-NUMBER
                                                               (IDZ IDY)
      *    損傷区分
           MOVE    ORCSAPIHKN-DAMAGE-CLASS (IDX)
                                       TO  PTINFLST-DAMAGE-CLASS
                                                               (IDZ IDY)
      *    損傷区分名称
           MOVE    ORCSAPIHKN-DAMAGE-CLASS-NAME (IDX)
                                       TO  PTINFLST-DAMAGE-CLASS-NAME
                                                               (IDZ IDY)
           .
       200121-HKNHEN-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    地域連携など編集処理
      *****************************************************************
       20013-ETC-HEN-SEC       SECTION.
      *
           INITIALIZE                      ORAPI012RSUB1AREA
           MOVE    PTINF-PTID          TO  API12SUB-PTID
           CALL    "ORAPI012RSUB1"     USING
                                       SPA-AREA
                                       ORAPI012RSUB1AREA
      *
      *    地域連携ID
           MOVE    API12SUB-COMMUNITY-CID
                                       TO  PTINFLST-COMMUNITY-CID  (IDZ)
      *    情報提供同意有無
           MOVE    API12SUB-COMMUNITY-CID-AGREE
                                       TO  PTINFLST-COMMUNITY-CID-AGREE
                                                                   (IDZ)
      *    初回来院日
           MOVE    API12SUB-FIRST-DATE TO  PTINFLST-FIRST-DATE     (IDZ)
      *    最終来院日（退院日）
           MOVE    API12SUB-LAST-DATE  TO  PTINFLST-LAST-DATE      (IDZ)
      *    入院中区分（１：入院中）
           MOVE    API12SUB-OUTPATIENT-CLASS
                                       TO  PTINFLST-OUTPATIENT-CLASS
                                                                   (IDZ)
      *    入院日（入院中のみ）
           MOVE    API12SUB-ADMISSION-DATE
                                       TO  PTINFLST-ADMISSION-DATE (IDZ)
      *    退院日（当日退院時のみ）
           MOVE    API12SUB-DISCHARGE-DATE
                                       TO  PTINFLST-DISCHARGE-DATE (IDZ)
      *
      *    患者介護保険情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               MOVE    API12SUB-CARE-INSURANCE-NUMBER (IDX)
                                   TO  PTINFLST-CARE-INSURANCE-NUMBER
                                                              (IDZ IDX)
               MOVE    API12SUB-CARE-POLICYHOLDER     (IDX)
                                       TO  PTINFLST-CARE-POLICYHOLDER
                                                              (IDZ IDX)
               MOVE    API12SUB-CARE-START-DATE       (IDX)
                                       TO  PTINFLST-CARE-START-DATE
                                                              (IDZ IDX)
               MOVE    API12SUB-CARE-END-DATE         (IDX)
                                       TO  PTINFLST-CARE-END-DATE
                                                              (IDZ IDX)
           END-PERFORM
      *    要介護認定情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   50
      *        要介護状態区分
               MOVE    API12SUB-NEED-CARE-STATE-CODE (IDX)
                                   TO  PTINFLST-NEED-CARE-STATE-CODE
                                                               (IDZ IDX)
      *        要介護状態名称
               MOVE    API12SUB-NEED-CARE-STATE (IDX)
                                       TO  PTINFLST-NEED-CARE-STATE
                                                               (IDZ IDX)
      *        認定日付
               MOVE    API12SUB-NEED-DATE (IDX)
                                       TO  PTINFLST-NEED-DATE
                                                               (IDZ IDX)
      *        有効開始日
               MOVE    API12SUB-NEED-START-DATE (IDX)
                                       TO  PTINFLST-NEED-START-DATE
                                                               (IDZ IDX)
      *        有効終了日
               MOVE    API12SUB-NEED-END-DATE (IDX)
                                       TO  PTINFLST-NEED-END-DATE
                                                               (IDZ IDX)
           END-PERFORM
      *    地域包括診療対象疾病
           MOVE    API12SUB-COMMUNITY-DISEASE
                                       TO  PTINFLST-COMMUNITY-DISEASE
                                                                   (IDZ)
      *    患者個人番号情報
           MOVE    API12SUB-PTMYNUMBER-G
                                       TO  PTINFLST-PTMYNUMBER-G   (IDZ)
      *
      *H30.10
      *    妊婦加算
           MOVE    API12SUB-NINPUKBN   TO  PTINFLST-NINPUKBN(IDZ)
      *    認知症地域包括診療
           MOVE    API12SUB-NINTIHOKATU
                                       TO  PTINFLST-NINTIHOKATU(IDZ)
      *    小児かかりつけ診療
           MOVE    API12SUB-SYOUNIKAKARI
                                       TO  PTINFLST-SYOUNIKAKARI(IDZ)
      *
      *    管理料等自動算定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    API12SUB-AUTO-SRYCD(IDX)
                                       TO  PTINFLST-AUTO-SRYCD
                                                           (IDZ IDX)
               MOVE    API12SUB-AUTO-NAME(IDX)
                                       TO  PTINFLST-AUTO-NAME
                                                           (IDZ IDX)
               MOVE    API12SUB-AUTO-ENDYMD(IDX)
                                       TO  PTINFLST-AUTO-ENDYMD
                                                           (IDZ IDX)
           END-PERFORM
      *
      *    患者禁忌薬剤
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   100  )
                           OR (API12SUB-DRUGCD-CODE (IDX)  =   SPACE )
      *        禁忌薬剤コード
               MOVE    API12SUB-DRUGCD-CODE (IDX)
                                       TO  PTINFLST-DRUGCD-CODE
                                                           (IDZ IDX)
               MOVE    API12SUB-DRUGCD-NAME (IDX)
                                       TO  PTINFLST-DRUGCD-NAME
                                                           (IDZ IDX)
      *        有効終了日（マスタ終了日）
               MOVE    API12SUB-DRUGCD-END-DATE (IDX)
                                       TO  PTINFLST-DRUGCD-END-DATE
                                                           (IDZ IDX)
      *        禁忌開始日
               MOVE    API12SUB-START-DATE (IDX)
                                       TO  PTINFLST-START-DATE
                                                           (IDZ IDX)
           END-PERFORM
      *
           .
       20013-ETC-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT 処理
      *****************************************************************
       910-XMLEXPORT-SEC          SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE   PTINFLST-REC     TO  XMLDATA-DATA
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_patient_basis_all"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "Patient_Basis_All"
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
           STRING  "patient_basis_all"
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
               MOVE    "patient_basis_all"
                                       TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               move    "2"             TO  SAPIFILE-IN-KBN
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    "patient_basis_all.xml"
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
           DISPLAY "*** ORAPI052BR4V3 OUT:" CNT-OUT
           DISPLAY "*** ORAPI052BR4V3 END ***"
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
      *****************************************************************
      *    システム管理マスタ主キー読み込み処理
      *****************************************************************
       900-SYSKANRI-KEYREAD-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    WRK-PARA-SYS-YMD    TO  SYS-STYUKYMD
           MOVE    WRK-PARA-SYS-YMD    TO  SYS-EDYUKYMD
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
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
