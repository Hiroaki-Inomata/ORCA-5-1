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
       PROGRAM-ID.             ORCBDASCONF.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データ収集
      *  コンポーネント名  : 定点設定
      *  管理者            :
      *  作成日付    作業者        記述
      *  11/09/01    伊藤          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    データ
           SELECT  USER-FILE       ASSIGN  FILE-NAME
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-DATA.
      *    基本設定
           SELECT  CONF-FILE       ASSIGN  CONF-NAME
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-CONF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    データ
       FD  USER-FILE.
       01  USER-REC                PIC X(600).
      *
      *    基本設定
       FD  CONF-FILE.
       01  CONF-REC                PIC X(600).
      *
       WORKING-STORAGE             SECTION.
      *
      * データ領域
       01  FILE-NAME.
           03  FILLER              PIC X(256).
      *
       01  CONF-NAME.
           03  FILLER              PIC X(256).
      *
       01  STS-AREA.
           03  STS-DATA            PIC X(02).
           03  STS-CONF            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-USER            PIC 9(01).
           03  FLG-CONF            PIC 9(01).
           03  FLG-DASCONF         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-REC             PIC 9(06).
           03  CNT-CONF            PIC 9(06).
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-MODE               PIC X(01).
               05  WRK-PARA-HOSPNUM            PIC X(02).
               05  WRK-PARA-HOSPID             PIC X(24).
               05  WRK-PARA-RESULT             PIC X(03).
               05  WRK-PARA-FILENAME           PIC X(256).
               05  WRK-PARA-CONFFILE           PIC X(256).
           03  WRK-CONF.
               05  WRK-CONF-HOST               PIC X(100).
               05  WRK-CONF-PATH               PIC X(100).
               05  WRK-CONF-PATHPROF           PIC X(100).
               05  WRK-CONF-PATHINF            PIC X(100).
               05  WRK-CONF-CAFILE             PIC X(100).
      *
       01  WRK-DATA-REC1.
           03  FILLER              PIC X(11)   VALUE
               'config = {'.
       01  WRK-DATA-REC2.
           03  FILLER              PIC X(11)   VALUE
               '"Host" => "'.
       01  WRK-DATA-REC2E.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC31.
           03  FILLER              PIC X(11)   VALUE
               '"Path" => "'.
       01  WRK-DATA-REC312.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC32.
           03  FILLER              PIC X(15)   VALUE
               '"PathProf" => "'.
       01  WRK-DATA-REC322.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC33.
           03  FILLER              PIC X(14)   VALUE
               '"PathInf" => "'.
       01  WRK-DATA-REC332.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC4.
           03  FILLER              PIC X(24)   VALUE
               '"CertificateCAFile" => "'.
       01  WRK-DATA-REC42.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC5.
           03  FILLER              PIC X(17)   VALUE
               '"HospitalID" => "'.
       01  WRK-DATA-REC52.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC6.
           03  FILLER              PIC X(21)   VALUE
               '"ApplicationDay" => "'.
       01  WRK-DATA-REC62.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC7.
           03  FILLER              PIC X(20)   VALUE
               '"ClerkNameKana" => "'.
       01  WRK-DATA-REC72.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC8.
           03  FILLER              PIC X(16)   VALUE
               '"ClerkName" => "'.
       01  WRK-DATA-REC82.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC9.
           03  FILLER              PIC X(17)   VALUE
               '"ClerkEmail" => "'.
       01  WRK-DATA-REC92.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC10.
           03  FILLER              PIC X(20)   VALUE
               '"HospitalClass" => "'.
       01  WRK-DATA-REC102.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC11.
           03  FILLER              PIC X(22)   VALUE
               '"GeneralHospital" => "'.
       01  WRK-DATA-REC112.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC12.
           03  FILLER              PIC X(21)   VALUE
               '"SickbedGeneral" => "'.
       01  WRK-DATA-REC122.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC13.
           03  FILLER              PIC X(26)   VALUE
               '"SickbedMedTreatment" => "'.
       01  WRK-DATA-REC132.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC14.
           03  FILLER              PIC X(20)   VALUE
               '"SickbedMental" => "'.
       01  WRK-DATA-REC142.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC15.
           03  FILLER              PIC X(26)   VALUE
               '"SickbedTuberculosis" => "'.
       01  WRK-DATA-REC152.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC16.
           03  FILLER              PIC X(19)   VALUE
               '"SickbedTotal" => "'.
       01  WRK-DATA-REC162.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC17.
           03  FILLER              PIC X(20)   VALUE
               '"MainlyInstall" => "'.
       01  WRK-DATA-REC172.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC18.
           03  FILLER              PIC X(26)   VALUE
               '"MainlyMedDepartment" => "'.
       01  WRK-DATA-REC182.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC19.
           03  FILLER              PIC X(19)   VALUE
               '"CommunityMed" => "'.
       01  WRK-DATA-REC192.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC20.
           03  FILLER              PIC X(10)   VALUE
               '"DPC" => "'.
       01  WRK-DATA-REC202.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC21.
           03  FILLER              PIC X(18)   VALUE
               '"HomeMedCare" => "'.
       01  WRK-DATA-REC212.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC22.
           03  FILLER              PIC X(18)   VALUE
               '"OpeningTime" => "'.
       01  WRK-DATA-REC222.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC23.
           03  FILLER              PIC X(15)   VALUE
               '"Postcode" => "'.
       01  WRK-DATA-REC232.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC24.
           03  FILLER              PIC X(11)   VALUE
               '"User" => "'.
       01  WRK-DATA-REC242.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC25.
           03  FILLER              PIC X(15)   VALUE
               '"Password" => "'.
       01  WRK-DATA-REC252.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC26.
           03  FILLER              PIC X(16)   VALUE
               '"Agreement" => "'.
       01  WRK-DATA-REC262.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC27.
           03  FILLER              PIC X(17)   VALUE
               '"SendResult" => "'.
       01  WRK-DATA-REC272.
           03  FILLER              PIC X(02)   VALUE
               '",'.
       01  WRK-DATA-REC28.
           03  FILLER              PIC X(15)   VALUE
               '"SendDate" => "'.
       01  WRK-DATA-REC282.
           03  FILLER              PIC X(01)   VALUE
               '"'.
       01  WRK-DATA-REC29.
           03  FILLER              PIC X(11)   VALUE
               "}".
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-KEY        PIC X(08)   VALUE   "orca-prj".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    定点設定
       01  DASCF-REC.
           COPY    "CPDASCONF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
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
           PERFORM 200-MAIN-SEC        UNTIL FLG-END  =  1
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
           INITIALIZE                  STS-AREA
                                       FLG-AREA
                                       CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
      *    WRK-PARA-MODE
      *      "T" : conf file(old) -> tbl_das_conf
      *      "M" : tbl_das_conf and das-upload.conf -> conf file(/tmp)
      *      "R" : sending file result code -> tbl_das_conf
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-MODE
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-HOSPID
                                               WRK-PARA-RESULT
                                               WRK-PARA-FILENAME
                                               WRK-PARA-CONFFILE
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           MOVE    WRK-PARA-FILENAME   TO  FILE-NAME
           MOVE    WRK-PARA-CONFFILE   TO  CONF-NAME
      *
           EVALUATE    WRK-PARA-MODE
               WHEN    "T"
                   OPEN    INPUT                   USER-FILE
                   IF      STS-DATA        NOT =   ZERO
                       MOVE    1                   TO  FLG-END
                   END-IF
               WHEN    "M"
                   OPEN    INPUT                   CONF-FILE
                   OPEN    OUTPUT                  USER-FILE
                   IF     (STS-CONF        NOT =   ZERO)  OR
                          (STS-DATA        NOT =   ZERO)
                       MOVE    1                   TO  FLG-END
                   END-IF
           END-EVALUATE
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-DASCONF-READ-SEC
      *
           EVALUATE    WRK-PARA-MODE
               WHEN    "T"
                   IF      FLG-DASCONF         =   ZERO
                       DISPLAY "das configuration already exist"
                   ELSE
                       PERFORM 210-CONF-FILE-TO-DB-SEC
                       PERFORM 900-DASCONF-WRITE-SEC
                   END-IF
               WHEN    "M"
                   IF      FLG-DASCONF         =   ZERO
                       PERFORM 210-CONF-DB-TO-FILE-SEC
                   ELSE
                       DISPLAY "das configuration is nothing"
                   END-IF
               WHEN    "R"
                   IF      FLG-DASCONF         =   ZERO
                       PERFORM 210-CONF-RESULT-SET-SEC
                   ELSE
                       DISPLAY "das configuration is nothing"
                   END-IF
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    申請データ移行処理（ファイル→テーブル）
      *****************************************************************
       210-CONF-FILE-TO-DB-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-USER
           MOVE    SPACE               TO  USER-REC
           MOVE    SPACE               TO  DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           PERFORM UNTIL   FLG-USER    =   1
               READ    USER-FILE       NEXT
                   AT  END
                       MOVE    1                   TO  FLG-USER
                   NOT AT  END
                       COMPUTE CNT-REC             =   CNT-REC  +  1
      **               IF      USER-REC(1:17)  =   WRK-DATA-REC5
      **                   UNSTRING  USER-REC(18:) DELIMITED  BY  '",'
      **                                     INTO  DASCF-HOSPID
      **                   END-UNSTRING
      **                   IF      SPA-HOSPID  NOT =   SPA-MUID-HOSPID
      **                       MOVE    1       TO  FLG-USER
      **                   END-IF
      **               END-IF
                       IF      USER-REC(1:21)  =   WRK-DATA-REC6
                           UNSTRING  USER-REC(22:) DELIMITED  BY  '",'
                                             INTO  DASCF-SINSEIYMD
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:20)  =   WRK-DATA-REC7
                           UNSTRING  USER-REC(21:) DELIMITED  BY  '",'
                                             INTO  DASCF-KANANAME
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:16)  =   WRK-DATA-REC8
                           UNSTRING  USER-REC(17:) DELIMITED  BY  '",'
                                             INTO  DASCF-NAME
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:17)  =   WRK-DATA-REC9
                           UNSTRING  USER-REC(18:) DELIMITED  BY  '",'
                                             INTO  DASCF-EMAIL
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:20)  =   WRK-DATA-REC10
                           UNSTRING  USER-REC(21:) DELIMITED  BY  '",'
                                             INTO  DASCF-HOSPSBT1
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:22)  =   WRK-DATA-REC11
                           UNSTRING  USER-REC(23:) DELIMITED  BY  '",'
                                             INTO  DASCF-HOSPSBT2
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:21)  =   WRK-DATA-REC12
                           UNSTRING  USER-REC(22:) DELIMITED  BY  '",'
                                             INTO  DASCF-BEDSUIPPAN
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:26)  =   WRK-DATA-REC13
                           UNSTRING  USER-REC(27:) DELIMITED  BY  '",'
                                             INTO  DASCF-BEDSURYOYO
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:20)  =   WRK-DATA-REC14
                           UNSTRING  USER-REC(21:) DELIMITED  BY  '",'
                                             INTO  DASCF-BEDSUSEISIN
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:26)  =   WRK-DATA-REC15
                           UNSTRING  USER-REC(27:) DELIMITED  BY  '",'
                                             INTO  DASCF-BEDSUKEKKAKU
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:19)  =   WRK-DATA-REC16
                           UNSTRING  USER-REC(20:) DELIMITED  BY  '",'
                                             INTO  DASCF-BEDSU
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:20)  =   WRK-DATA-REC17
                           UNSTRING  USER-REC(21:) DELIMITED  BY  '",'
                                             INTO  DASCF-SETTI
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:26)  =   WRK-DATA-REC18
                           UNSTRING  USER-REC(27:) DELIMITED  BY  '",'
                                             INTO  DASCF-SYUKA
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:19)  =   WRK-DATA-REC19
                           UNSTRING  USER-REC(20:) DELIMITED  BY  '",'
                                             INTO  DASCF-CHIIKI
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:10)  =   WRK-DATA-REC20
                           UNSTRING  USER-REC(11:) DELIMITED  BY  '",'
                                             INTO  DASCF-DPCKBN
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:18)  =   WRK-DATA-REC21
                           UNSTRING  USER-REC(19:) DELIMITED  BY  '",'
                                             INTO  DASCF-ZAITAKU
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:18)  =   WRK-DATA-REC22
                           UNSTRING  USER-REC(19:) DELIMITED  BY  '",'
                                             INTO  DASCF-KAIINYM
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:15)  =   WRK-DATA-REC23
                           UNSTRING  USER-REC(16:) DELIMITED  BY  '",'
                                             INTO  DASCF-POST
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:11)  =   WRK-DATA-REC24
                           UNSTRING  USER-REC(12:) DELIMITED  BY  '",'
                                             INTO  DASCF-USERID
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:15)  =   WRK-DATA-REC25
                           UNSTRING  USER-REC(16:) DELIMITED  BY  '",'
                                             INTO  DASCF-PASSWORD
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:16)  =   WRK-DATA-REC26
                           UNSTRING  USER-REC(17:) DELIMITED  BY  '",'
                                             INTO  DASCF-AGREEMENT
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:17)  =   WRK-DATA-REC27
                           UNSTRING  USER-REC(18:) DELIMITED  BY  '",'
                                             INTO  DASCF-SENDRESULT
                           END-UNSTRING
                       END-IF
                       IF      USER-REC(1:15)  =   WRK-DATA-REC28
                           UNSTRING  USER-REC(16:) DELIMITED  BY  '"'
                                             INTO  DASCF-SENDDATE
                           END-UNSTRING
                       END-IF
               END-READ
           END-PERFORM
           .
       210-CONF-FILE-TO-DB-EXT.
           EXIT.
      *
      *****************************************************************
      *    申請データ移行処理（テーブル→ファイル）
      *****************************************************************
       210-CONF-DB-TO-FILE-SEC     SECTION.
      *
           PERFORM 2101-CONF-DB-TO-FILE-SEC
      *
           MOVE    SPACE               TO  USER-REC
           MOVE    WRK-DATA-REC1       TO  USER-REC
           PERFORM 900-DATA-WRITE-SEC
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC2           DELIMITED  BY  SIZE
                       WRK-CONF-HOST           DELIMITED  BY  SPACE
                       WRK-DATA-REC2E          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC31          DELIMITED  BY  SIZE
                       WRK-CONF-PATH           DELIMITED  BY  SPACE
                       WRK-DATA-REC312         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC32          DELIMITED  BY  SIZE
                       WRK-CONF-PATHPROF       DELIMITED  BY  SPACE
                       WRK-DATA-REC322         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC33          DELIMITED  BY  SIZE
                       WRK-CONF-PATHINF        DELIMITED  BY  SPACE
                       WRK-DATA-REC332         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC4           DELIMITED  BY  SIZE
                       WRK-CONF-CAFILE         DELIMITED  BY  SPACE
                       WRK-DATA-REC42          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC5           DELIMITED  BY  SIZE
                       WRK-PARA-HOSPID         DELIMITED  BY  SPACE
                       WRK-DATA-REC52          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC6           DELIMITED  BY  SIZE
                       DASCF-SINSEIYMD         DELIMITED  BY  SPACE
                       WRK-DATA-REC62          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC7           DELIMITED  BY  SIZE
                       DASCF-KANANAME          DELIMITED  BY  SPACE
                       WRK-DATA-REC72          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC8           DELIMITED  BY  SIZE
                       DASCF-NAME              DELIMITED  BY  SPACE
                       WRK-DATA-REC82          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC9           DELIMITED  BY  SIZE
                       DASCF-EMAIL             DELIMITED  BY  SPACE
                       WRK-DATA-REC92          DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC10          DELIMITED  BY  SIZE
                       DASCF-HOSPSBT1          DELIMITED  BY  SPACE
                       WRK-DATA-REC102         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC11          DELIMITED  BY  SIZE
                       DASCF-HOSPSBT2          DELIMITED  BY  SPACE
                       WRK-DATA-REC112         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC12          DELIMITED  BY  SIZE
                       DASCF-BEDSUIPPAN        DELIMITED  BY  SPACE
                       WRK-DATA-REC122         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC13          DELIMITED  BY  SIZE
                       DASCF-BEDSURYOYO        DELIMITED  BY  SPACE
                       WRK-DATA-REC132         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC14          DELIMITED  BY  SIZE
                       DASCF-BEDSUSEISIN       DELIMITED  BY  SPACE
                       WRK-DATA-REC142         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC15          DELIMITED  BY  SIZE
                       DASCF-BEDSUKEKKAKU      DELIMITED  BY  SPACE
                       WRK-DATA-REC152         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC16          DELIMITED  BY  SIZE
                       DASCF-BEDSU             DELIMITED  BY  SPACE
                       WRK-DATA-REC162         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC17          DELIMITED  BY  SIZE
                       DASCF-SETTI             DELIMITED  BY  SPACE
                       WRK-DATA-REC172         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC18          DELIMITED  BY  SIZE
                       DASCF-SYUKA             DELIMITED  BY  SPACE
                       WRK-DATA-REC182         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC19          DELIMITED  BY  SIZE
                       DASCF-CHIIKI            DELIMITED  BY  SPACE
                       WRK-DATA-REC192         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC20          DELIMITED  BY  SIZE
                       DASCF-DPCKBN            DELIMITED  BY  SPACE
                       WRK-DATA-REC202         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC21          DELIMITED  BY  SIZE
                       DASCF-ZAITAKU           DELIMITED  BY  SPACE
                       WRK-DATA-REC212         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC22          DELIMITED  BY  SIZE
                       DASCF-KAIINYM           DELIMITED  BY  SPACE
                       WRK-DATA-REC222         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC23          DELIMITED  BY  SIZE
                       DASCF-POST              DELIMITED  BY  SPACE
                       WRK-DATA-REC232         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC24          DELIMITED  BY  SIZE
                       DASCF-USERID            DELIMITED  BY  SPACE
                       WRK-DATA-REC242         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC25          DELIMITED  BY  SIZE
                       DASCF-PASSWORD          DELIMITED  BY  SPACE
                       WRK-DATA-REC252         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC26          DELIMITED  BY  SIZE
                       DASCF-AGREEMENT         DELIMITED  BY  SPACE
                       WRK-DATA-REC262         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
      *ここから下の項目はスクリプトでセットするらしい
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC27          DELIMITED  BY  SIZE
                       WRK-DATA-REC272         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    SPACE               TO  USER-REC
               STRING  WRK-DATA-REC28          DELIMITED  BY  SIZE
                       WRK-DATA-REC282         DELIMITED  BY  SIZE
                                         INTO  USER-REC
               END-STRING
               PERFORM 900-DATA-WRITE-SEC
           END-IF
      *
           IF      STS-DATA            =   ZERO
               MOVE    WRK-DATA-REC29      TO  USER-REC
               PERFORM 900-DATA-WRITE-SEC
           END-IF
           .
       210-CONF-DB-TO-FILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    申請データ移行処理（ファイル→テーブル）
      *****************************************************************
       2101-CONF-DB-TO-FILE-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-CONF
           MOVE    SPACE           TO  CONF-REC
           PERFORM UNTIL   FLG-CONF    =   1
               READ    CONF-FILE       NEXT
                   AT  END
                       MOVE    1           TO  FLG-CONF
                   NOT AT  END
                       COMPUTE CNT-CONF    =   CNT-CONF  +  1
                       IF      CONF-REC(1:11)  =   WRK-DATA-REC2
                           UNSTRING  CONF-REC(12:) DELIMITED  BY  '",'
                                             INTO  WRK-CONF-HOST
                           END-UNSTRING
                       END-IF
                       IF      CONF-REC(1:11)  =   WRK-DATA-REC31
                           UNSTRING  CONF-REC(12:) DELIMITED  BY  '",'
                                             INTO  WRK-CONF-PATH
                           END-UNSTRING
                       END-IF
                       IF      CONF-REC(1:15)  =   WRK-DATA-REC32
                           UNSTRING  CONF-REC(16:) DELIMITED  BY  '",'
                                             INTO  WRK-CONF-PATHPROF
                           END-UNSTRING
                       END-IF
                       IF      CONF-REC(1:14)  =   WRK-DATA-REC33
                           UNSTRING  CONF-REC(15:) DELIMITED  BY  '",'
                                             INTO  WRK-CONF-PATHINF
                           END-UNSTRING
                       END-IF
                       IF      CONF-REC(1:24)  =   WRK-DATA-REC4
                           UNSTRING  CONF-REC(25:) DELIMITED  BY  '",'
                                             INTO  WRK-CONF-CAFILE
                           END-UNSTRING
                       END-IF
               END-READ
           END-PERFORM
           .
       2101-CONF-DB-TO-FILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    送信結果更新処理（テーブル）
      *****************************************************************
       210-CONF-RESULT-SET-SEC     SECTION.
      *
           MOVE    WRK-PARA-RESULT     TO  DASCF-SENDRESULT
           MOVE    SMCNDATE-YMD        TO  DASCF-SENDDATE
           MOVE    SMCNDATE-YMD        TO  DASCF-UPYMD
           MOVE    SMCNDATE-HMS        TO  DASCF-UPHMS
      *
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Result Code Set Error"
           END-IF
           .
       210-CONF-RESULT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-MODE
               WHEN    "T"
                   CLOSE   USER-FILE
               WHEN    "M"
                   CLOSE   USER-FILE
                   CLOSE   CONF-FILE
           END-EVALUATE
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           IF      WRK-PARA-MODE   NOT =   "R"
               DISPLAY "CONF LINE [ " CNT-REC " ]"
               DISPLAY "ETC  LINE [ " CNT-CONF " ]"
           END-IF
           DISPLAY "*** ORCBDASCONF END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    定点設定登録
      *****************************************************************
       900-DASCONF-WRITE-SEC       SECTION.
      *
           MOVE    SPACE               TO  DASCF-TERMID
           MOVE    SPACE               TO  DASCF-OPID
           MOVE    SMCNDATE-YMD        TO  DASCF-CREYMD
      **   MOVE    SMCNDATE-YMD        TO  DASCF-UPYMD
      **   MOVE    SMCNDATE-HMS        TO  DASCF-UPHMS
      *
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DASCONF-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       900-DATA-WRITE-SEC          SECTION.
      *
           WRITE   USER-REC
           COMPUTE CNT-REC             =   CNT-REC  +  1
           .
       900-DATA-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    定点設定読込
      *****************************************************************
       900-DASCONF-READ-SEC        SECTION.
      *
           INITIALIZE  DASCF-REC
           MOVE    SPA-HOSPNUM         TO  DASCF-HOSPNUM
           MOVE    DASCF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_das_conf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-DASCONF
                   MOVE    MCPDATA-REC         TO  DASCF-REC
               ELSE
                   MOVE    1                   TO  FLG-DASCONF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-DASCONF
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_das_conf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DASCONF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
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
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
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
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
