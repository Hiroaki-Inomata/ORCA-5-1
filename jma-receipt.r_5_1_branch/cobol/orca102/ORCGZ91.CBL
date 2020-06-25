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
       PROGRAM-ID.             ORCGZ91.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ出力設定画面（Ｚ９１）
      *  管理者            : 
      *  02/01/11    MCC-森脇　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00-00-01    森脇         02/04/02  出力形式追加
      *  01.00.02    MCC-藤原     02/04/24  印刷管理・印刷ＤＢ・
      *                                     ジョブ管理ＤＢの追加
      *  01.00.03    MCC-藤原     02/05/15  処理前にジョブ管理を削除する
      *  01.00.04    MCC-藤原     02/05/22  再印刷ボタン・情報削除ボタン追加
      *  01.00.05    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.01.01    NACL-森脇    02/12/06  コメント追加
      *  01.01.02    NACL-藤原    03/05/12  印刷開始・終了頁の編集の修正 
      *  01.01.03    NACL-太田    03/07/02  出力先プリンタサブ追加
      *                                     印刷する・しないのボタン追加
      *  01.01.04    NACL-太田    03/07/11  自費追加
      *  01.01.05    NACL-森脇    03/08/22  分類区分追加
      *  02.00.01    NACL-森脇    03/12/24  その他材料追加
      *  02.00.02    NACL-森脇    04/07/29  直前ジョブ管理チェック追加
      *  02.00.03    NACL-森脇    04/10/26  診療行為注射範囲修正
      *  02.00.04    NACL-森脇    04/10/27  診療行為選択変更
      *  02.00.05    NACL-多々納  05/12/12  MONFUNC対応 他
      *  02.00.06    NACL-森脇    06/03/20  プレビュー表示
      *  02.00.07    NACL-森脇    06/07/27  抽出データと出力順に自院採用を追加
      *  03.05.01    NACL-森脇    07/05/29  グループ診療対応
      *  04.01.01    NACL-森脇    08/01/17  器材商品追加
      *  04.02.01    NACL-森脇    08/03/13  病理診療追加
      *  04.03.01    NACL-森脇    08/06/13  ＣＳＶ出力対応
      *  04.08.01    NACL-森脇    14/08/21  一時ファイルディレクトリ設定
      *                           14/09/03  GINBEE対応
      *  05.00.00    NACL-森脇    17/11/29  クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  TENSUERR-FILE   ASSIGN  TENSUERR
                                   FILE    STATUS  IS  STS-TENSUERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  TENSUERR-FILE.
       01  TENSUERR-REC            PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TENSUERR//.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CSV             PIC X(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TENSUERR        PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    印刷用呼出SHELL
           COPY    "CPHCMLSHELL.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-PRTKANRI-RENNUM PIC 9(04).
           03  WRK-SHORIERR        PIC X(200).
           03  WRK-ZIDMSG          PIC X(70).
           03  WRK-PGID            PIC X(30).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
      *
           03  WRK-LNKYMD          PIC X(10).
      *
           03  WRK-TENSUERR        PIC X(200).
      *
           03  WRK-CSVDIR          PIC X(100).
           03  WRK-CSVFILE         PIC X(100).
      *
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE          PIC 9(05).
           03  WRK-PRT-PAGE        PIC 9(10).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値
           03  WRK-CONS-3003-KBNCD         PIC X(08)   VALUE   "102".
      *    印刷管理ＤＢのキー値
           03  WRK-CONS-PRTKANRI-TBL-KEY   PIC X(08)   VALUE   "TENSU".
           03  WRK-CONS-PRTKANRI-TBL-GROUP PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値
           03  WRK-CONS-JOB-SHELLID        PIC X(08)   VALUE   "TENSU".
           03  WRK-CONS-JOB-JOBID          PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1031.INC".
      *
           COPY    "CPSK3003.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *----(04.08.01)--ADD-START---
      *    クライアント保存可否情報取得サブ
           COPY    "CPORCSCLIENTSV.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *----(05.00.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(05.00.00)--ADD-END-----
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA102SCRAREA.INC".
      *
       PROCEDURE               DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE        TO  SPA-Z01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-Z01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Z01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD   NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE.
           MOVE    "Z91"           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
      *
      *        元の画面へ戻す
               IF    (  SPA-MOTOPG     =   "XA01" OR "XB01" OR "XC01" )
                   MOVE    LNK-COMMON      TO  SPA-KYOTUKEY
                   MOVE    LNK-SCRSPA      TO  SPA-Z01FREE
                   MOVE    SPA-WORK        TO  SPA-Z01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *
               MOVE    SPA-MOTOPG      TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "ZID3"
                   PERFORM 350-ZID3-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "Z97"
                   PERFORM 340-Z97-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "Z100"
                   PERFORM 350-Z100-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-Z91-GMN-AREA
           INITIALIZE                  SPA-GMN-SRYSTR-AREA
           INITIALIZE                  SPA-GMN-SRYEND-AREA
      *
           MOVE    "F"             TO  SPA-GMN-SRYCHK
           MOVE    "F"             TO  SPA-GMN-SRY(1)
           MOVE    "F"             TO  SPA-GMN-SRY(2)
      *
           MOVE    "F"             TO  SPA-GMN-IYKCHKALL
           MOVE    "F"             TO  SPA-GMN-IPNCHKALL
           MOVE    "F"             TO  SPA-GMN-IYKCHK(1)
           MOVE    "F"             TO  SPA-GMN-IYKCHK(2)
           MOVE    "F"             TO  SPA-GMN-IYKCHK(3)
           MOVE    "F"             TO  SPA-GMN-IYKCHK(4)
           MOVE    "F"             TO  SPA-GMN-IYKCHK(5)
           MOVE    "F"             TO  SPA-GMN-IYKCHK(6)
           MOVE    "F"             TO  SPA-GMN-IYKCHK(7)
      *
           MOVE    "F"             TO  SPA-GMN-TOKCHK
           MOVE    "F"             TO  SPA-GMN-HOKACHK
           MOVE    "F"             TO  SPA-GMN-KZICHK
           MOVE    "F"             TO  SPA-GMN-JIHCHK
      *
           MOVE    "F"             TO  SPA-GMN-COMCHKALL
           MOVE    "F"             TO  SPA-GMN-COMCHK(1)
           MOVE    "F"             TO  SPA-GMN-COMCHK(2)
           MOVE    "F"             TO  SPA-GMN-COMCHK(3)
      *
           MOVE    "T"             TO  SPA-GMN-CYU(1)
           MOVE    "F"             TO  SPA-GMN-CYU(2)
           MOVE    "F"             TO  SPA-GMN-CYU(3)
      *
           MOVE    "F"             TO  SPA-GMN-IPN
      *
           MOVE    "T"             TO  SPA-GMN-SYU(1)
           MOVE    "F"             TO  SPA-GMN-SYU(2)
           MOVE    "F"             TO  SPA-GMN-SYU(3)
           MOVE    "F"             TO  SPA-GMN-SYU(4)
      *
           MOVE    "T"             TO  SPA-GMN-KEI(1)
           MOVE    "F"             TO  SPA-GMN-KEI(2)
      *
           MOVE    "11"            TO  SPA-GMN-SRYSTRL(1)
           MOVE    "12"            TO  SPA-GMN-SRYSTRL(2)
           MOVE    "13"            TO  SPA-GMN-SRYSTRL(3)
           MOVE    "14"            TO  SPA-GMN-SRYSTRL(4)
           MOVE    "20"            TO  SPA-GMN-SRYSTRL(5)
           MOVE    "30"            TO  SPA-GMN-SRYSTRL(6)
           MOVE    "40"            TO  SPA-GMN-SRYSTRL(7)
           MOVE    "50"            TO  SPA-GMN-SRYSTRL(8)
           MOVE    "54"            TO  SPA-GMN-SRYSTRL(9)
           MOVE    "60"            TO  SPA-GMN-SRYSTRL(10)
           MOVE    "64"            TO  SPA-GMN-SRYSTRL(11)
           MOVE    "70"            TO  SPA-GMN-SRYSTRL(12)
           MOVE    "80"            TO  SPA-GMN-SRYSTRL(13)
           MOVE    "90"            TO  SPA-GMN-SRYSTRL(14)
      *
           MOVE    "初診料"        TO  SPA-GMN-SRYSTRMEIL(1)
           MOVE    "再診料"        TO  SPA-GMN-SRYSTRMEIL(2)
           MOVE    "指導料"        TO  SPA-GMN-SRYSTRMEIL(3)
           MOVE    "在宅料"        TO  SPA-GMN-SRYSTRMEIL(4)
           MOVE    "投薬料"        TO  SPA-GMN-SRYSTRMEIL(5)
           MOVE    "注射料"        TO  SPA-GMN-SRYSTRMEIL(6)
           MOVE    "処置料"        TO  SPA-GMN-SRYSTRMEIL(7)
           MOVE    "手術料"        TO  SPA-GMN-SRYSTRMEIL(8)
           MOVE    "麻酔料"        TO  SPA-GMN-SRYSTRMEIL(9)
           MOVE    "検査料"        TO  SPA-GMN-SRYSTRMEIL(10)
           MOVE    "病理診断"      TO  SPA-GMN-SRYSTRMEIL(11)
           MOVE    "画像診断料"    TO  SPA-GMN-SRYSTRMEIL(12)
           MOVE    "その他料"      TO  SPA-GMN-SRYSTRMEIL(13)
           MOVE    "入院料"        TO  SPA-GMN-SRYSTRMEIL(14)
      *
           MOVE    14              TO  SPA-SRYSTR-MAX
      *
           MOVE    SPA-GMN-SRYSTR-AREA
                                   TO  SPA-GMN-SRYEND-AREA
      *
      *    マスタ日付セット
           MOVE    SPA-SYSYMD      TO  SPA-NAI-MSTYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-MSTYMD
      *
           MOVE    1               TO  SPA-Z91-GMN-CUR
      *
      *----(04.08.01)--ADD-START---
      *    クライアント保存可否情報取得サブ
           INITIALIZE                      ORCSCLIENTSVAREA
           CALL    "ORCSCLIENTSV"          USING
                                           ORCSCLIENTSVAREA
                                           SPA-AREA
           MOVE    SCLIENTSV-CLIENT-SAVE
                                       TO  SPA-Z100-CLIENT-SAVE
      *----(04.08.01)--ADD-END-----
           .
       310-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           IF      SPA-ZID1-FLG    =   "OK"
               EVALUATE    SPA-ZIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1001"
                   WHEN    "1002"
                       PERFORM 4501-JOBKANRI-CHK-SEC
                       IF      SPA-ERRCD       =   SPACE
                           PERFORM 490-PRINT-SEC
                       END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-Z97-FLG
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
      *
           .
       330-ZID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ２）ＯＫ処理
      *****************************************************************
      * 340-ZID2-SET-SEC            SECTION.
      *
      *     MOVE    SPACE           TO  SPA-ZIDCD2
      *     .
      * 340-ZID2-SET-EXT.
      *     EXIT.
      *
      *****************************************************************
      *    確認画面（ＺＩＤ３）ＯＫ処理
      *****************************************************************
       350-ZID3-SET-SEC            SECTION.
      *
           IF      SPA-ZID1-FLG    NOT =   SPACE
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "0"         TO  FLG-CSV
                   PERFORM 490-PRINT-SEC
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-ZID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-Z97-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
      *
           .
       350-ZID3-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＺＩＤ３）ＯＫ処理
      *****************************************************************
       350-Z100-SET-SEC            SECTION.
      *
           IF      SPA-ZID1-FLG    NOT =   SPACE
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "1"         TO  FLG-CSV
                   PERFORM 490-PRINT-SEC
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-ZID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-Z97-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
      *
           .
       350-Z100-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-Z97-SET-SEC             SECTION.
      *
           IF    ( SPA-Z97-FLG     =   "OK" )
               PERFORM 4501-NEXT-GMN-SEC
           END-IF
           IF    ( SPA-ZID1-FLG    =   "SV" )
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "D"             TO  SFILESV-MODE
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SFILESV-TBL-KEY
               CALL    "ORCSFILESV"        USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
      *
           MOVE    ZERO            TO  SPA-Z91-GMN-CUR
           MOVE    ZERO            TO  IDX
           MOVE    3               TO  IDY
      *
           MOVE    SPACE           TO  SPA-Z97-FLG
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
      *
           .
       340-Z97-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC           SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "Z91"           TO  SPA-MOTOPG
           MOVE    "XC01"          TO  SPA-SAKIPG
      *
           MOVE    0               TO  WRK-STPAGE
           MOVE    0               TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-Z91-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE          TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE          TO  LNK-XC01-ED-PAGE
           MOVE    "Z91"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Z01FREE         TO  LNK-SCRSPA
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 310-SPASET-SEC
      *        再印刷
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-REPRINT-SEC
      *        情報削除
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *        CSV
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-CSV-MAE-SEC
      *        印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-PRINT-MAE-SEC
      *        処理確認
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
      *        診療行為チェック
               WHEN    "CLICKED"       ALSO    "SRYCHK"
                   EVALUATE    SPA-GMN-SRYCHK
                       WHEN    "T"
                           MOVE    "F"         TO  SPA-GMN-SRYCHK
                           MOVE    "F"         TO  SPA-GMN-SRY(1)
                           MOVE    "F"         TO  SPA-GMN-SRY(2)
                       WHEN    "F"
                           MOVE    "T"         TO  SPA-GMN-SRYCHK
                           MOVE    "T"         TO  SPA-GMN-SRY(1)
                           MOVE    "F"         TO  SPA-GMN-SRY(2)
                   END-EVALUATE
                   MOVE  1             TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "SRY1"
                   EVALUATE    SPA-GMN-SRY(1)
                       WHEN    "T"
                           MOVE    "F"         TO  SPA-GMN-SRYCHK
                           MOVE    "F"         TO  SPA-GMN-SRY(1)
                           MOVE    "F"         TO  SPA-GMN-SRY(2)
                       WHEN    "F"
                           MOVE    "T"         TO  SPA-GMN-SRYCHK
                           MOVE    "T"         TO  SPA-GMN-SRY(1)
                           MOVE    "F"         TO  SPA-GMN-SRY(2)
                   END-EVALUATE
                   MOVE    2           TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "SRY2"
                   EVALUATE    SPA-GMN-SRY(2)
                       WHEN    "T"
                           MOVE    "F"         TO  SPA-GMN-SRYCHK
                           MOVE    "F"         TO  SPA-GMN-SRY(1)
                           MOVE    "F"         TO  SPA-GMN-SRY(2)
                           MOVE    3           TO  SPA-Z91-GMN-CUR
                       WHEN    "F"
                           MOVE    "T"         TO  SPA-GMN-SRYCHK
                           MOVE    "F"         TO  SPA-GMN-SRY(1)
                           MOVE    "T"         TO  SPA-GMN-SRY(2)
                           MOVE    4           TO  SPA-Z91-GMN-CUR
                   END-EVALUATE
      *        医薬品チェック
               WHEN    "CLICKED"       ALSO    "IYKCHKALL"
                   EVALUATE    SPA-GMN-IYKCHKALL
                       WHEN    "T"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   7
                               MOVE    "F"     TO  SPA-GMN-IYKCHK(IDX)
                           END-PERFORM
                           MOVE    "F"     TO  SPA-GMN-IYKCHKALL
                       WHEN    "F"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   7
                               MOVE    "T"     TO  SPA-GMN-IYKCHK(IDX)
                           END-PERFORM
                           MOVE    "T"     TO  SPA-GMN-IYKCHKALL
                           MOVE    "F"     TO  SPA-GMN-IPNCHKALL
                           MOVE    "F"     TO  SPA-GMN-IPN
                   END-EVALUATE
                   MOVE    6           TO  SPA-Z91-GMN-CUR
      *        一般名チェック
               WHEN    "CLICKED"       ALSO    "IPNCHKALL"
                   EVALUATE    SPA-GMN-IPNCHKALL
                       WHEN    "T"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   7
                               MOVE    "F"     TO  SPA-GMN-IYKCHK(IDX)
                           END-PERFORM
                           MOVE    "F"     TO  SPA-GMN-IPNCHKALL
                           MOVE    "F"     TO  SPA-GMN-IPN
                       WHEN    "F"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   7
                               IF      IDX     <=   2
                               MOVE    "T"     TO  SPA-GMN-IYKCHK(IDX)
                               ELSE
                               MOVE    "F"     TO  SPA-GMN-IYKCHK(IDX)
                               END-IF
                           END-PERFORM
                           MOVE    "T"     TO  SPA-GMN-IPNCHKALL
                           MOVE    "F"     TO  SPA-GMN-IYKCHKALL
                   END-EVALUATE
                   MOVE    32          TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK1"
                   EVALUATE    SPA-GMN-IYKCHK(1)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-IYKCHK(1)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-IYKCHK(1)
                   END-EVALUATE
                   MOVE    7           TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK2"
                   EVALUATE    SPA-GMN-IYKCHK(2)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-IYKCHK(2)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-IYKCHK(2)
                   END-EVALUATE
                   MOVE    8           TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK3"
                   EVALUATE    SPA-GMN-IYKCHK(3)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-IYKCHK(3)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-IYKCHK(3)
                   END-EVALUATE
                   MOVE    9           TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK4"
                   EVALUATE    SPA-GMN-IYKCHK(4)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-IYKCHK(4)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-IYKCHK(4)
                   END-EVALUATE
                   MOVE    10          TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK5"
                   EVALUATE    SPA-GMN-IYKCHK(5)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-IYKCHK(5)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-IYKCHK(5)
                   END-EVALUATE
                   MOVE    11          TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK6"
                   EVALUATE    SPA-GMN-IYKCHK(6)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-IYKCHK(6)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-IYKCHK(6)
                   END-EVALUATE
                   MOVE    12          TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "IYKCHK7"
                   EVALUATE    SPA-GMN-IYKCHK(7)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-IYKCHK(7)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-IYKCHK(7)
                   END-EVALUATE
                   MOVE    13          TO  SPA-Z91-GMN-CUR
      *        特定器材チェック
               WHEN    "CLICKED"       ALSO    "TOKCHK"
                   EVALUATE    SPA-GMN-TOKCHK
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-TOKCHK
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-TOKCHK
                   END-EVALUATE
                   MOVE    14          TO  SPA-Z91-GMN-CUR
      *        その他材料チェック
               WHEN    "CLICKED"       ALSO    "HOKACHK"
                   MOVE    Z91-HOKACHK     TO  SPA-GMN-HOKACHK
                   MOVE    15              TO  SPA-Z91-GMN-CUR
      *        器材商品チェック
               WHEN    "CLICKED"       ALSO    "KZICHK" 
                   MOVE    Z91-KZICHK      TO  SPA-GMN-KZICHK
                   MOVE    31              TO  SPA-Z91-GMN-CUR
      *        自費チェック
               WHEN    "CLICKED"       ALSO    "JIHCHK" 
                   MOVE    Z91-JIHCHK      TO  SPA-GMN-JIHCHK
                   MOVE    16              TO  SPA-Z91-GMN-CUR
      *        コメントチェック
               WHEN    "CLICKED"       ALSO    "COMCHKALL"
                   EVALUATE    SPA-GMN-COMCHKALL
                       WHEN    "T"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   3
                               MOVE    "F"     TO  SPA-GMN-COMCHK(IDX)
                           END-PERFORM
                       WHEN    "F"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX         >   3
                               MOVE    "T"     TO  SPA-GMN-COMCHK(IDX)
                           END-PERFORM
                   END-EVALUATE
                   MOVE    17          TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "COMCHK1"
                   EVALUATE    SPA-GMN-COMCHK(1)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-COMCHK(1)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-COMCHK(1)
                   END-EVALUATE  
                   MOVE    18          TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "COMCHK2"
                   EVALUATE    SPA-GMN-COMCHK(2)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-COMCHK(2)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-COMCHK(2)
                   END-EVALUATE
                   MOVE    19          TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "COMCHK3"
                   EVALUATE    SPA-GMN-COMCHK(3)
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-COMCHK(3)
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-COMCHK(3)
                   END-EVALUATE
                   MOVE  20            TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "KEI1"
               WHEN    "CLICKED"       ALSO    "KEI2"
                   MOVE  Z91-KEI(1)            TO  SPA-GMN-KEI(1)
                   MOVE  Z91-KEI(2)            TO  SPA-GMN-KEI(2)
                   IF      SPA-GMN-KEI(1)  =   "T"
                       MOVE    "T"         TO  SPA-GMN-CYU(1)
                       MOVE    "F"         TO  SPA-GMN-CYU(2)
                       MOVE    "F"         TO  SPA-GMN-CYU(3)
                       IF      SPA-GMN-SYU(4)  =   "T"
                           MOVE    "T"         TO  SPA-GMN-SYU(1)
                           MOVE    "F"         TO  SPA-GMN-SYU(2)
                           MOVE    "F"         TO  SPA-GMN-SYU(3)
                           MOVE    "F"         TO  SPA-GMN-SYU(4)
                       END-IF
                   ELSE
                       IF      SPA-GMN-CYU(1)  =   "T"
                           MOVE    "F"         TO  SPA-GMN-CYU(1)
                           MOVE    "T"         TO  SPA-GMN-CYU(2)
                           MOVE    "F"         TO  SPA-GMN-CYU(3)
                       END-IF
                   END-IF
               WHEN    "CLICKED"       ALSO    "CYU1"
               WHEN    "CLICKED"       ALSO    "CYU2"
               WHEN    "CLICKED"       ALSO    "CYU3"
                   MOVE  Z91-CYU1              TO  SPA-GMN-CYU(1)
                   MOVE  Z91-CYU2              TO  SPA-GMN-CYU(2)
                   MOVE  Z91-CYU3              TO  SPA-GMN-CYU(3)
                   IF      SPA-GMN-CYU(1)  =   "T"
                       IF      SPA-GMN-SYU(4)  =   "T"
                           MOVE    "T"         TO  SPA-GMN-SYU(1)
                           MOVE    "F"         TO  SPA-GMN-SYU(2)
                           MOVE    "F"         TO  SPA-GMN-SYU(3)
                           MOVE    "F"         TO  SPA-GMN-SYU(4)
                       END-IF
                   END-IF
               WHEN    "CLICKED"       ALSO    "IPN"
                   EVALUATE    SPA-GMN-IPN
                       WHEN    "T"
                           MOVE    "F"     TO  SPA-GMN-IPN
                       WHEN    "F"
                           MOVE    "T"     TO  SPA-GMN-IPN
                   END-EVALUATE
                   MOVE    33          TO  SPA-Z91-GMN-CUR
               WHEN    "CLICKED"       ALSO    "SYU1"
               WHEN    "CLICKED"       ALSO    "SYU2"
               WHEN    "CLICKED"       ALSO    "SYU3"
               WHEN    "CLICKED"       ALSO    "SYU4"
                   MOVE  Z91-SYU1              TO  SPA-GMN-SYU(1)
                   MOVE  Z91-SYU2              TO  SPA-GMN-SYU(2)
                   MOVE  Z91-SYU3              TO  SPA-GMN-SYU(3)
                   MOVE  Z91-SYU4              TO  SPA-GMN-SYU(4)
           END-EVALUATE
      *
           IF      SPA-GMN-IYKCHKALL   =   "T"
               IF          SPA-GMN-IYKCHK(1)   =   "F"
                   AND     SPA-GMN-IYKCHK(2)   =   "F"
                   AND     SPA-GMN-IYKCHK(3)   =   "F"
                   AND     SPA-GMN-IYKCHK(4)   =   "F"
                   AND     SPA-GMN-IYKCHK(5)   =   "F"
                   AND     SPA-GMN-IYKCHK(6)   =   "F"
                   AND     SPA-GMN-IYKCHK(7)   =   "F"
                   MOVE    "F"             TO  SPA-GMN-IYKCHKALL
               END-IF
           END-IF
           IF      SPA-GMN-IPNCHKALL   =   "T"
               IF          SPA-GMN-IYKCHK(1)   =   "F"
                   AND     SPA-GMN-IYKCHK(2)   =   "F"
                   MOVE    "F"             TO  SPA-GMN-IPNCHKALL
                   MOVE    "F"             TO  SPA-GMN-IPN
               END-IF
           END-IF
      *
           IF          SPA-GMN-COMCHK(1)   =   "F"
               AND     SPA-GMN-COMCHK(2)   =   "F"
               AND     SPA-GMN-COMCHK(3)   =   "F"
               MOVE    "F"             TO  SPA-GMN-COMCHKALL
           ELSE
               MOVE    "T"             TO  SPA-GMN-COMCHKALL
           END-IF
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT        ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"       ALSO    "MSTYMD" 
                   PERFORM  400-MSTYMD-CHK-SEC
               WHEN    "ACTIVATE"       ALSO    "SRYSTR"
                   PERFORM  400-SRYSTR-SEC
               WHEN    "ACTIVATE"       ALSO    "SRYEND"
                   PERFORM  400-SRYEND-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "6666"          TO  SPA-ZIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      * 
           MOVE    "Z01"               TO  SPA-SAKIPG
           MOVE    "Z91"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *  　マスタ年月日チェック
      *****************************************************************
       400-MSTYMD-CHK-SEC          SECTION.
      *
           MOVE  Z91-MSTYMD             TO  SPA-GMN-MSTYMD
      *
           MOVE    ZERO                 TO  SPA-NAI-MSTYMD
      *          
           IF      SPA-GMN-MSTYMD        =  SPACE
               MOVE    SPA-SYSYMDWH         TO  SPA-GMN-MSTYMD
           END-IF     
      *
           MOVE  SPA-GMN-MSTYMD         TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-LNKYMD          TO  SPA-GMN-MSTYMD
               MOVE    WRK-SYMD            TO  SPA-NAI-MSTYMD
               MOVE    99                  TO  SPA-Z91-GMN-CUR
           ELSE
               MOVE    21                  TO  SPA-Z91-GMN-CUR
           END-IF 
      *
           .
       400-MSTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分開始
      *****************************************************************
       400-SRYSTR-SEC              SECTION.
      *
           MOVE  Z91-SRYSTR        TO  SPA-Z91-SRYSTR
      *
           PERFORM 410-SRYSTR-CHK-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  400-SRYSTR-EXT
           END-IF
      *
           MOVE    SPA-Z91-SRYSTR  TO  SPA-Z91-SRYEND
      *
           MOVE    5               TO  SPA-Z91-GMN-CUR
      *
           .
       400-SRYSTR-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分終了
      *****************************************************************
       400-SRYEND-SEC              SECTION.
      *
           MOVE  Z91-SRYEND        TO  SPA-Z91-SRYEND
      *
           PERFORM 410-SRYEND-CHK-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  400-SRYEND-EXT
           END-IF
      *
           MOVE  6               TO  SPA-Z91-GMN-CUR
      *
           .
       400-SRYEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療開始区分チェック
      *****************************************************************
       410-SRYSTR-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           IF      SPA-Z91-SRYSTR      =   SPACE
               MOVE    4               TO  SPA-Z91-GMN-CUR
               MOVE    "0006"          TO  SPA-ERRCD
               GO  TO  410-SRYSTR-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-SRYSTR-MAX
               IF       SPA-GMN-SRYSTRL(IDX)   =  SPA-Z91-SRYSTRL
                    MOVE    SPA-GMN-SRYSTR-LIST(IDX)
                                           TO  SPA-Z91-SRYSTR
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-SRYSTR-MAX TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    4               TO  SPA-Z91-GMN-CUR
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *
           .
       410-SRYSTR-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療終了区分チェック
      *****************************************************************
       410-SRYEND-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           IF      SPA-Z91-SRYEND      =   SPACE
               MOVE    5               TO  SPA-Z91-GMN-CUR
               MOVE    "0006"          TO  SPA-ERRCD
               GO  TO  410-SRYEND-CHK-EXT
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-SRYEND-MAX
               IF       SPA-GMN-SRYENDL(IDX)   =  SPA-Z91-SRYENDL
                    MOVE    SPA-GMN-SRYEND-LIST(IDX)
                                           TO  SPA-Z91-SRYEND
                    MOVE    1              TO  FLG-OK
                    MOVE    SPA-SRYEND-MAX TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK          =   ZERO
               MOVE    5               TO  SPA-Z91-GMN-CUR
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *
           IF    SPA-Z91-SRYSTRL   >   SPA-Z91-SRYENDL
               MOVE    5               TO  SPA-Z91-GMN-CUR
               MOVE    "0007"          TO  SPA-ERRCD
               GO  TO  410-SRYEND-CHK-EXT
           END-IF
           .
       410-SRYEND-CHK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Z01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "Z91"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Z01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "Z91"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >       SYS-3003-JOBID )
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-SHORIERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                                                   INTO  WRK-SHORIERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
         .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷前確認処理
      *****************************************************************
       490-PRINT-MAE-SEC                   SECTION.
      *
           IF     (SPA-GMN-IYKCHKALL   =   "F")    AND
                  (SPA-GMN-IPNCHKALL   =   "F")    AND
                  (SPA-GMN-SRYCHK      =   "F")    AND
                  (SPA-GMN-TOKCHK      =   "F")    AND
                  (SPA-GMN-JIHCHK      =   "F")    AND
                  (SPA-GMN-COMCHKALL   =   "F")    AND
                  (SPA-GMN-KZICHK      =   "F")    AND
                  (SPA-GMN-HOKACHK     =   "F")
               MOVE    "0004"          TO  SPA-ERRCD
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
           IF      SPA-GMN-SRY(2)      =   "T"
               PERFORM 410-SRYSTR-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  490-PRINT-MAE-EXT
               END-IF
               PERFORM 410-SRYEND-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  490-PRINT-MAE-EXT
               END-IF
           END-IF
      *
           IF      SPA-NAI-MSTYMD      =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-Z91-GMN-CUR
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD       NOT  =   SPACE
               GO  TO  490-PRINT-MAE-EXT
           END-IF
      *     
           IF     SPA-ERRCD           =   SPACE     
               IF      SPA-GMN-KEI(1)        =   "T"
                   MOVE    "1001"              TO  SPA-ZIDCD
               ELSE   
                   MOVE    "1002"              TO  SPA-ZIDCD
               END-IF
           END-IF
      *
           .
       490-PRINT-MAE-EXT.
           EXIT.               
      *
      *****************************************************************
      *    印刷前確認処理
      *****************************************************************
       490-CSV-MAE-SEC                   SECTION.
      *
           IF     (SPA-GMN-IYKCHKALL   =   "F")    AND
                  (SPA-GMN-IPNCHKALL   =   "F")    AND
                  (SPA-GMN-SRYCHK      =   "F")    AND
                  (SPA-GMN-TOKCHK      =   "F")    AND
                  (SPA-GMN-JIHCHK      =   "F")    AND
                  (SPA-GMN-COMCHKALL   =   "F")    AND
                  (SPA-GMN-KZICHK      =   "F")    AND
                  (SPA-GMN-HOKACHK     =   "F")
               MOVE    "0004"          TO  SPA-ERRCD
               GO  TO  490-CSV-MAE-EXT
           END-IF
      *
           IF      SPA-GMN-SRY(2)      =   "T"
               PERFORM 410-SRYSTR-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  490-CSV-MAE-EXT
               END-IF
               PERFORM 410-SRYEND-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  490-CSV-MAE-EXT
               END-IF
           END-IF
      *
           IF      SPA-NAI-MSTYMD      =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    21              TO  SPA-Z91-GMN-CUR
               GO  TO  490-CSV-MAE-EXT
           END-IF
      *
           PERFORM 4501-JOBKANRI-CHK-SEC
           IF      SPA-ERRCD       NOT  =   SPACE
               GO  TO  490-CSV-MAE-EXT
           END-IF
      *     
           INITIALIZE                  SPA-CSV-AREA
           IF     (SPA-GMN-SRYCHK      =   "T")
               MOVE    "T"             TO  SPA-CSV-CHK(1)
           END-IF
           IF     (SPA-GMN-IYKCHK(1) =   "T") OR
                  (SPA-GMN-IYKCHK(2) =   "T") OR
                  (SPA-GMN-IYKCHK(3) =   "T") OR
                  (SPA-GMN-IYKCHK(4) =   "T") OR
                  (SPA-GMN-IYKCHK(5) =   "T") OR
                  (SPA-GMN-IYKCHK(6) =   "T")
               MOVE    "T"             TO  SPA-CSV-CHK(2)
           END-IF
           IF     (SPA-GMN-TOKCHK      =   "T")
               MOVE    "T"             TO  SPA-CSV-CHK(3)
           END-IF
           IF     (SPA-GMN-HOKACHK     =   "T")
               MOVE    "T"             TO  SPA-CSV-CHK(4)
           END-IF
           IF     (SPA-GMN-KZICHK      =   "T")
               MOVE    "T"             TO  SPA-CSV-CHK(5)
           END-IF
           IF     (SPA-GMN-COMCHKALL   =   "T")
               MOVE    "T"             TO  SPA-CSV-CHK(6)
           END-IF
           IF     (SPA-GMN-JIHCHK      =   "T")
               MOVE    "T"             TO  SPA-CSV-CHK(7)
           END-IF
           IF     (SPA-GMN-IYKCHK(7) =   "T")
               MOVE    "T"             TO  SPA-CSV-CHK(8)
           END-IF
      *
      *    メッセージ
           MOVE    SPACE           TO  SPA-MOTOPG
           MOVE    SPA-Z01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Z01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGZ100"      USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE        TO  SPA-Z01FREE
      *
           MOVE  "B12"             TO  MCP-WIDGET
      *
           MOVE  "Z91"             TO  SPA-MOTOPG
           MOVE  "Z100"            TO  SPA-SAKIPG
      *
           MOVE  "NEW"             TO  MCP-PUTTYPE
           MOVE  "Z100"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1                 TO  FLG-END
      *
           .
       490-CSV-MAE-EXT.
           EXIT.               
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       490-PRINT-SEC                       SECTION.
      *
           INITIALIZE                      SPA-Z91-TBL-GROUP
                                           SPA-Z91-JOB-JOBID
      *
           INITIALIZE                      SPA-Z97-AREA
           MOVE    ZERO                TO  SPA-Z97-FILESV-CHK-FLG
      *
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE                                 
                   MOVE    "1005"              TO  SPA-ERRCD
                   GO  TO  490-PRINT-EXT
               END-IF
           END-IF                 
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  490-PRINT-EXT
           END-IF             
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP
      *
           MOVE    "OR102ERR"          TO  TENSUERR-FILE-ID
           MOVE    SPA-TERMID          TO  TENSUERR-TERMID
           MOVE    SPA-HOSPNUM         TO  TENSUERR-HOSPNUM
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF 
      *
           INITIALIZE                  SHELL-AREA
      *
      *    診療行為チェックセット
           IF      SPA-GMN-SRYCHK      =   "T"
               MOVE    1                   TO  SHELL-SRYCHK
      *        診療区分セット
               IF      SPA-GMN-SRY(2)  =   "T"
                   MOVE  2                 TO  SHELL-SRYKBN
                   IF    SPA-Z91-SRYSTRL   <=  SPA-Z91-SRYENDL
                       MOVE  SPA-Z91-SRYSTRL   TO  SHELL-SRYSTR
                       MOVE  SPA-Z91-SRYENDL   TO  SHELL-SRYEND
                   ELSE
                       MOVE  SPA-Z91-SRYENDL   TO  SHELL-SRYSTR
                       MOVE  SPA-Z91-SRYSTRL   TO  SHELL-SRYEND
                   END-IF
                   EVALUATE    SHELL-SRYEND
                     WHEN      "20"
                       MOVE    29          TO  SHELL-SRYEND
                     WHEN      "30"
                       MOVE    34          TO  SHELL-SRYEND
                     WHEN      "90"
                       MOVE    97          TO  SHELL-SRYEND
                   END-EVALUATE
               ELSE
                   MOVE  1                 TO  SHELL-SRYKBN
                   MOVE  11                TO  SHELL-SRYSTR
                   MOVE  97                TO  SHELL-SRYEND
               END-IF
           END-IF
      *     
      *    医薬品チェックセット
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   7
               IF      SPA-GMN-IYKCHK(IDX)     =  "T"
                   EVALUATE    IDX
                       WHEN    1
                           MOVE    1           TO  SHELL-IYKCHK (IDX)
                       WHEN    2
                           MOVE    6           TO  SHELL-IYKCHK (IDX)
                       WHEN    3
                           MOVE    4           TO  SHELL-IYKCHK (IDX)
                       WHEN    4
                           MOVE    8           TO  SHELL-IYKCHK (IDX)
                       WHEN    5    
                           MOVE    9           TO  SHELL-IYKCHK (IDX)
                       WHEN    6
                           MOVE    3           TO  SHELL-IYKCHK (IDX)
                       WHEN    7
                           MOVE    1           TO  SHELL-IYKCHK (IDX)
                   END-EVALUATE        
               END-IF
           END-PERFORM
      *
      *    特定器材チェックセット
           IF      SPA-GMN-TOKCHK      =   "T"
               MOVE  1                     TO  SHELL-TOKCHK
           ELSE
               MOVE  ZERO                  TO  SHELL-TOKCHK 
           END-IF
      *
      *    その他チェックセット
           IF      SPA-GMN-HOKACHK     =   "T"
               MOVE  1                     TO  SHELL-HOKACHK
           ELSE
               MOVE  ZERO                  TO  SHELL-HOKACHK
           END-IF
      *
      *    器材商品チェックセット
           IF      SPA-GMN-KZICHK     =   "T"
               MOVE  1                     TO  SHELL-KZICHK
           ELSE
               MOVE  ZERO                  TO  SHELL-KZICHK
           END-IF
      *
      *    自費チェックセット
           IF      SPA-GMN-JIHCHK      =   "T"
               MOVE  1                     TO  SHELL-JIHCHK
           ELSE
               MOVE  ZERO                  TO  SHELL-JIHCHK 
           END-IF
      *
      *    コメントチェックセット
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   3
               IF      SPA-GMN-COMCHK(IDX)     =  "T"
                   MOVE    1           TO  SHELL-COMCHK (IDX) 
               ELSE
                   MOVE    ZERO        TO  SHELL-COMCHK (IDX)
               END-IF
           END-PERFORM     
      *
      *    抽出区分セット
           EVALUATE    TRUE
               WHEN    SPA-GMN-CYU(1)      =   "T"
                   MOVE  1                     TO  SHELL-CYUKBN
               WHEN    SPA-GMN-CYU(2)      =   "T"
                   MOVE  2                     TO  SHELL-CYUKBN
               WHEN    SPA-GMN-CYU(3)      =   "T"
                   MOVE  3                     TO  SHELL-CYUKBN
           END-EVALUATE
      *
      *    一般チェックセット
           IF      SPA-GMN-IPN         =   "T"
               MOVE  1                     TO  SHELL-IPN
           ELSE
               MOVE  ZERO                  TO  SHELL-IPN
           END-IF
      *
      *    出力区分セット
           EVALUATE    TRUE
               WHEN    SPA-GMN-SYU (1) =   "T"
                   MOVE    1               TO  SHELL-SYUKBN
               WHEN    SPA-GMN-SYU (2) =   "T"
                   MOVE    2               TO  SHELL-SYUKBN
               WHEN    SPA-GMN-SYU (3) =   "T"
                   MOVE    3               TO  SHELL-SYUKBN
               WHEN    SPA-GMN-SYU (4) =   "T"
                   MOVE    4               TO  SHELL-SYUKBN
           END-EVALUATE
      *
      *    マスタ日付チェック
           MOVE  SPA-NAI-MSTYMD        TO  SHELL-MSTYMD
      *
      *    印刷処理
           MOVE    WRK-SYSYMD          TO
                                       WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                       WRK-CONS-PRTKANRI-TBL-GROUP (9:)
      *
           IF      SPA-ZID1-FLG        =  "SV"
               INITIALIZE                  ORCSFILESVAREA
               MOVE    "D"             TO  SFILESV-MODE
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SFILESV-TBL-KEY
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
      *   
           IF      SPA-GMN-KEI(1)        =   "T"
               IF      SHELL-SRYCHK          =   1
                       MOVE    SPA-CSV-DIRNAME(1)    TO  WRK-CSVDIR
                       MOVE    SPA-CSV-FILENAME(1)   TO  WRK-CSVFILE
                       MOVE    "ORHCML03"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SPA-GMN-IYKCHKALL  =   "T"
                   IF      SHELL-IYKCHK(7)   >   ZERO
                       MOVE    SPA-CSV-DIRNAME(8)    TO  WRK-CSVDIR
                       MOVE    SPA-CSV-FILENAME(8)   TO  WRK-CSVFILE
                       MOVE    "ORHCML12"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
                   IF     (SHELL-IYKCHK(1)   >   ZERO)
                       OR (SHELL-IYKCHK(2)   >   ZERO)
                       OR (SHELL-IYKCHK(3)   >   ZERO)
                       OR (SHELL-IYKCHK(4)   >   ZERO)
                       OR (SHELL-IYKCHK(5)   >   ZERO)
                       OR (SHELL-IYKCHK(6)   >   ZERO)
                       MOVE    SPA-CSV-DIRNAME(2)    TO  WRK-CSVDIR
                       MOVE    SPA-CSV-FILENAME(2)   TO  WRK-CSVFILE
                       MOVE    "ORHCML01"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
               END-IF
               IF      SPA-GMN-IPNCHKALL  =   "T"
                   IF     (SHELL-IYKCHK(1)   >   ZERO)
                       OR (SHELL-IYKCHK(2)   >   ZERO)
                       MOVE    SPA-CSV-DIRNAME(2)    TO  WRK-CSVDIR
                       MOVE    SPA-CSV-FILENAME(2)   TO  WRK-CSVFILE
                       MOVE    "ORHCML13"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
               END-IF
      *
               IF      SHELL-TOKCHK        =   1
                   MOVE    1                 TO  SHELL-KBN
                   MOVE    SPA-CSV-DIRNAME(3)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(3)   TO  WRK-CSVFILE
                   MOVE    "ORHCML02"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-HOKACHK       =   1
                   MOVE    2                 TO  SHELL-KBN
                   MOVE    SPA-CSV-DIRNAME(4)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(4)   TO  WRK-CSVFILE
                   MOVE    "ORHCML02"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-KZICHK        =   1
                   MOVE    3                 TO  SHELL-KBN
                   MOVE    SPA-CSV-DIRNAME(5)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(5)   TO  WRK-CSVFILE
                   MOVE    "ORHCML02"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-JIHCHK        =   1   
                   MOVE    SPA-CSV-DIRNAME(7)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(7)   TO  WRK-CSVFILE
                   MOVE    "ORHCML10"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-COMCHK-G        =   ALL "0"
                   CONTINUE
               ELSE
                   MOVE    SPA-CSV-DIRNAME(6)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(6)   TO  WRK-CSVFILE
                   MOVE    "ORHCML04"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
           ELSE
               IF      SHELL-SRYCHK          =   1
                   MOVE    SPA-CSV-DIRNAME(1)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(1)   TO  WRK-CSVFILE
                   MOVE    "ORHCML08"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SPA-GMN-IYKCHKALL  =   "T"
                   IF      SHELL-IYKCHK(7)   >   ZERO
                       MOVE    SPA-CSV-DIRNAME(8)    TO  WRK-CSVDIR
                       MOVE    SPA-CSV-FILENAME(8)   TO  WRK-CSVFILE
                       MOVE    "ORHCML12"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
                   IF     (SHELL-IYKCHK(1)   >   ZERO)
                       OR (SHELL-IYKCHK(2)   >   ZERO)
                       OR (SHELL-IYKCHK(3)   >   ZERO)
                       OR (SHELL-IYKCHK(4)   >   ZERO)
                       OR (SHELL-IYKCHK(5)   >   ZERO)
                       OR (SHELL-IYKCHK(6)   >   ZERO)
                       MOVE    SPA-CSV-DIRNAME(2)    TO  WRK-CSVDIR
                       MOVE    SPA-CSV-FILENAME(2)   TO  WRK-CSVFILE
                       MOVE    "ORHCML06"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
               END-IF
               IF      SPA-GMN-IPNCHKALL  =   "T"
                   IF     (SHELL-IYKCHK(1)   >   ZERO)
                       OR (SHELL-IYKCHK(2)   >   ZERO)
                       MOVE    SPA-CSV-DIRNAME(2)    TO  WRK-CSVDIR
                       MOVE    SPA-CSV-FILENAME(2)   TO  WRK-CSVFILE
                       MOVE    "ORHCML13"        TO  WRK-PGID
                       PERFORM 4901-PRINT-SYORI-SEC
                   END-IF
               END-IF
      *
               IF      SHELL-TOKCHK        =   1
                   MOVE    1                 TO  SHELL-KBN
                   MOVE    SPA-CSV-DIRNAME(3)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(3)   TO  WRK-CSVFILE
                   MOVE    "ORHCML07"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-HOKACHK       =   1
                   MOVE    2                 TO  SHELL-KBN
                   MOVE    SPA-CSV-DIRNAME(4)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(4)   TO  WRK-CSVFILE
                   MOVE    "ORHCML07"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-KZICHK        =   1
                   MOVE    3                 TO  SHELL-KBN
                   MOVE    SPA-CSV-DIRNAME(5)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(5)   TO  WRK-CSVFILE
                   MOVE    "ORHCML07"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-JIHCHK        =   1    
                   MOVE    SPA-CSV-DIRNAME(7)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(7)   TO  WRK-CSVFILE
                   MOVE    "ORHCML11"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
      *
               IF      SHELL-COMCHK-G        =   ALL "0"
                   CONTINUE
               ELSE    
                   MOVE    SPA-CSV-DIRNAME(6)    TO  WRK-CSVDIR
                   MOVE    SPA-CSV-FILENAME(6)   TO  WRK-CSVFILE
                   MOVE    "ORHCML05"        TO  WRK-PGID
                   PERFORM 4901-PRINT-SYORI-SEC
               END-IF
           END-IF
      *
      *    印刷処理
      *
           IF      SPA-ZID1-FLG        =   "OK"  OR "PV"
               PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  SPA-Z91-TBL-GROUP
               MOVE    WRK-CONS-JOB-JOBID
                                           TO  SPA-Z91-JOB-JOBID
      *
               MOVE    0                   TO  WRK-STPAGE
               MOVE    99999               TO  WRK-EDPAGE
      *             
               MOVE    SPACE               TO  SHELLTBL
      *****    MOVE    "/usr/local/orca/scripts/allways/print.sh"
      *****                                TO  SHELLTBL-NAME
      *        ファイルデイレクトリ位置指定
               MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
               MOVE    SPACE               TO  LNK-PRTKANRI-AREA
               INITIALIZE                      LNK-PRTKANRI-AREA
               MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                           TO  LNK-PRTKANRI-TBL-KEY 
               MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                           TO  LNK-PRTKANRI-TBL-GROUP
               MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    SPA-NAI-MSTYMD      TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"          TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
               MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
               MOVE    LNK-PRTKANRI-TBL-KEY
                                           TO  SHELLTBL-ARG2
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                           TO  SHELLTBL-ARG3
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                           TO  SHELLTBL-ARG4
               MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
               MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
               MOVE    LNK-PRTKANRI-SHELLID
                                           TO  SHELLTBL-ARG7
               MOVE    LNK-PRTKANRI-PRIORITY
                                           TO  SHELLTBL-ARG8
               MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
               MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
               MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
      *                 
      *----(04.08.01)--UPD-START---
      *         MOVE    TENSUERR            TO  SHELLTBL-ARG12
               MOVE    TENSUERR-BASENAME   TO  SHELLTBL-ARG12
      *----(04.08.01)--UPD-END-----
               MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
               EVALUATE    SPA-ZID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
      *         WHEN    "NO"
      *             MOVE    "3"             TO  SHELLTBL-ARG17
      *----(05.00.00)--ADD-START---
               WHEN    "OK"
                   IF      SPA-CLIENT-PRT-FLG  =  2
                       MOVE    "5"         TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    "2"         TO  SHELLTBL-ARG17
                   END-IF
               END-EVALUATE
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
      *----(05.00.00)--ADD-END-----
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SPACE               TO  SHELLTBL-ARG19
               MOVE    SPACE               TO  SHELLTBL-ARG20
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC 
      *     
           PERFORM 490-KAKUNIN-SEC
      *     
           .
       490-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       4901-PRINT-SYORI-SEC            SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *                                   
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           ADD     1               TO  WRK-PRTKANRI-RENNUM
           MOVE    WRK-PRTKANRI-RENNUM
                                   TO  LNK-PRTKANRI-RENNUM 
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY 
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-NAI-MSTYMD  TO  LNK-PRTKANRI-SKYYMD
           MOVE    "tensu.sh"      TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    SPACE               TO  SHELLTBL 
      *****MOVE    WRK-SHELL           TO  SHELLTBL-NAME
      *    ファイルデイレクトリ位置指定
           MOVE    "tensu.sh"      TO  SHELLTBL-NAME
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11      
      *                 
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG13
           MOVE    WRK-PGID        TO  SHELLTBL-ARG14
           MOVE    SHELL-AREA      TO  SHELLTBL-ARG15
           MOVE    TENSUERR        TO  SHELLTBL-ARG16
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG17
           MOVE    FLG-CSV         TO  SHELLTBL-ARG18
           MOVE    SPA-CSV-FILE    TO  SHELLTBL-ARG19
           MOVE    SPA-CSV-CDKBN   TO  SHELLTBL-ARG20
           MOVE    WRK-CSVDIR      TO  SHELLTBL-ARG21
           MOVE    WRK-CSVFILE     TO  SHELLTBL-ARG22
      *             
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ERROR MCP-RC=" MCP-RC 
           END-IF   
      *
           .
       4501-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *       
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SYS-3003-JOBID                                      
      *
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           IF      FLG-SYSKANRI        =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF 
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"   
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      SPA-Z97-JOB-AREA
           MOVE    WRK-CONS-3003-KBNCD TO  SPA-Z97-JOB-KBNCD
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-Z97-JOB-SHELLID
           MOVE    SPA-Z91-JOB-JOBID   TO  SPA-Z97-JOB-JOBID
      *
           MOVE    SPACE               TO  Z97
           INITIALIZE                      Z97
           MOVE    ZERO                TO  Z97-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  Z97-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           MOVE    5                   TO  Z97-DURATION
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                  CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC 
                                           SPA-AREA
                   IF      SJOBKANRI-RETURN    =   ZERO
                       MOVE    JOB-JOBID       TO  Z97-RENNUM   (IDX)
                       MOVE    JOB-SHELLMSG    TO  Z97-SHORINM  (IDX)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  Z97-STARTTIME(IDX)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  Z97-ENDTIME  (IDX)
                       MOVE    JOB-UPDCNT      TO  WRK-PAGE
                       MOVE    WRK-PAGE        TO  Z97-PAGE     (IDX)
                       MOVE    JOB-YOBI        TO  Z97-ERRMSG   (IDX)
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "処理中です"    TO  Z97-MSG
                       ELSE
                           IF      JOB-ERRCD       =   SPACE
                               CONTINUE
                           ELSE
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE
                                                   INTO  Z97-MSG
                               END-STRING
                           END-IF                          
                       END-IF    
                       MOVE    IDX                     TO  Z97-COUNT    
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( Z97-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( Z97-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                               TO  Z97-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                               TO  Z97-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-ZID1-FLG    =   "PV" )
               MOVE    "F12:プレビュー"    TO  Z97-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                   MOVE    WIDGET-NORMAL   TO  Z97-B12-STATE
                   MOVE    "B12"           TO  MCP-WIDGET
               END-IF
           END-IF
      *
           MOVE    "Z91"                TO  SPA-MOTOPG
                                            SPA-Z97-MOTOPG
           MOVE    "Z97"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "Z97"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ZIDCD       NOT =   SPACE
               EVALUATE    SPA-ZIDCD
               WHEN    "1001"
               WHEN    "1002"
                   PERFORM 540-ZID3SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               WHEN    OTHER
                   PERFORM 520-ZIDSET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z91    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE  SPACE              TO  Z91
      *
           INITIALIZE                   Z91
           MOVE  SPA-GMN-MSTYMD     TO  Z91-MSTYMD
           MOVE  SPA-GMN-SRYCHK     TO  Z91-SRYCHK
           MOVE  SPA-GMN-TOKCHK     TO  Z91-TOKCHK
           MOVE  SPA-GMN-JIHCHK     TO  Z91-JIHCHK
           MOVE  SPA-GMN-HOKACHK    TO  Z91-HOKACHK
           MOVE  SPA-GMN-KZICHK     TO  Z91-KZICHK
           MOVE  SPA-GMN-SRY(1)     TO  Z91-SRY(1)
           MOVE  SPA-GMN-SRY(2)     TO  Z91-SRY(2)
           MOVE  SPA-GMN-KEI(1)     TO  Z91-KEI(1)
           MOVE  SPA-GMN-KEI(2)     TO  Z91-KEI(2)
           MOVE  SPA-GMN-SYU(1)     TO  Z91-SYU1
           MOVE  SPA-GMN-SYU(2)     TO  Z91-SYU2
           MOVE  SPA-GMN-SYU(3)     TO  Z91-SYU3
           MOVE  SPA-GMN-SYU(4)     TO  Z91-SYU4
           MOVE  SPA-GMN-CYU(1)     TO  Z91-CYU1
           MOVE  SPA-GMN-CYU(2)     TO  Z91-CYU2
           MOVE  SPA-GMN-CYU(3)     TO  Z91-CYU3
           MOVE  SPA-GMN-IPN        TO  Z91-IPN
           MOVE  SPA-GMN-IYKCHK(1)     TO  Z91-IYKCHK1
           MOVE  SPA-GMN-IYKCHK(2)     TO  Z91-IYKCHK2
           MOVE  SPA-GMN-IYKCHK(3)     TO  Z91-IYKCHK3
           MOVE  SPA-GMN-IYKCHK(4)     TO  Z91-IYKCHK4
           MOVE  SPA-GMN-IYKCHK(5)     TO  Z91-IYKCHK5
           MOVE  SPA-GMN-IYKCHK(6)     TO  Z91-IYKCHK6
           MOVE  SPA-GMN-IYKCHK(7)     TO  Z91-IYKCHK7
           MOVE  SPA-GMN-COMCHK(1)     TO  Z91-COMCHK(1)
           MOVE  SPA-GMN-COMCHK(2)     TO  Z91-COMCHK(2)
           MOVE  SPA-GMN-COMCHK(3)     TO  Z91-COMCHK(3)
           MOVE  SPA-GMN-IYKCHKALL     TO  Z91-IYKCHKALL
           MOVE  SPA-GMN-COMCHKALL     TO  Z91-COMCHKALL
           MOVE  SPA-GMN-IPNCHKALL     TO  Z91-IPNCHKALL
      *
           PERFORM  VARYING IDX FROM 1 BY 1
                    UNTIL   IDX > SPA-SRYSTR-MAX
               MOVE  SPA-GMN-SRYSTR-LIST(IDX)  TO  Z91-SRYSTR-ITEM(IDX)
           END-PERFORM
      * 
           PERFORM  VARYING IDX FROM 1 BY 1
                    UNTIL   IDX > SPA-SRYEND-MAX
               MOVE  SPA-GMN-SRYEND-LIST(IDX)  TO  Z91-SRYEND-ITEM(IDX)
           END-PERFORM
      *    
           MOVE  SPA-SRYSTR-MAX  TO  Z91-SRYSTR-COUNT
           MOVE  SPA-SRYEND-MAX  TO  Z91-SRYEND-COUNT
      *
           IF  (SPA-GMN-SRYCHK  =  "T")  AND  (SPA-GMN-SRY(2)  =  "T")
               MOVE    SPA-Z91-SRYSTR      TO  Z91-SRYSTR 
               MOVE    SPA-Z91-SRYEND      TO  Z91-SRYEND 
               MOVE    WIDGET-NORMAL       TO  Z91-SRYSTR-STATE
               MOVE    WIDGET-NORMAL       TO  Z91-SRYEND-STATE 
           ELSE
               MOVE    SPACE               TO  SPA-Z91-SRYSTR
               MOVE    SPACE               TO  SPA-Z91-SRYEND
               MOVE    SPACE               TO  Z91-SRYSTR 
               MOVE    SPACE               TO  Z91-SRYEND 
               MOVE    WIDGET-INSENSITIVE  TO  Z91-SRYSTR-STATE
               MOVE    WIDGET-INSENSITIVE  TO  Z91-SRYEND-STATE     
           END-IF  
      *
           IF      SPA-GMN-KEI(2)  =  "T"
               MOVE    WIDGET-INSENSITIVE  TO  Z91-CYU1-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  Z91-CYU1-STATE 
           END-IF      
           IF      SPA-GMN-CYU(1)  =  "T"
               MOVE    WIDGET-INSENSITIVE  TO  Z91-SYU4-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  Z91-SYU4-STATE 
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE  TO  Z91-IYKCHK1-STATE
                                           Z91-IYKCHK2-STATE
                                           Z91-IYKCHK3-STATE
                                           Z91-IYKCHK4-STATE
                                           Z91-IYKCHK5-STATE
                                           Z91-IYKCHK6-STATE
                                           Z91-IYKCHK7-STATE
                                           Z91-IPN-STATE
           IF      SPA-GMN-IPNCHKALL  =  "T"
               MOVE    WIDGET-NORMAL       TO  Z91-IYKCHK1-STATE
                                               Z91-IYKCHK2-STATE
                                               Z91-IPN-STATE
           END-IF
           IF      SPA-GMN-IYKCHKALL  =  "T"
               MOVE    WIDGET-NORMAL       TO  Z91-IYKCHK1-STATE
                                               Z91-IYKCHK2-STATE
                                               Z91-IYKCHK3-STATE
                                               Z91-IYKCHK4-STATE
                                               Z91-IYKCHK5-STATE
                                               Z91-IYKCHK6-STATE
                                               Z91-IYKCHK7-STATE
           END-IF
      *----(04.08.01)--ADD-START---
      *    GINBEEのとき
           IF      SPA-MW      =   SPA-GINBEE
      *        クライアント保存可能であればＣＳＶボタン活性化
               IF      SPA-Z100-CLIENT-SAVE    =   "1"
                   MOVE    WIDGET-NORMAL       TO  Z91-B11-STATE
               ELSE
                   MOVE    WIDGET-INSENSITIVE  TO  Z91-B11-STATE
               END-IF
           ELSE
               MOVE    WIDGET-NORMAL       TO  Z91-B11-STATE
           END-IF
      *----(04.08.01)--ADD-END-----
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-Z91-GMN-CUR
               WHEN    1
                   MOVE    "SRYCHK"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "SRY1"              TO  MCP-WIDGET
               WHEN    3
                   MOVE    "SRY2"              TO  MCP-WIDGET
               WHEN    4
                   MOVE    "SRYSTR"            TO  MCP-WIDGET
               WHEN    5
                   MOVE    "SRYEND"            TO  MCP-WIDGET
               WHEN    6
                   MOVE    "IYKCHKALL"         TO  MCP-WIDGET
               WHEN    7
                   MOVE    "IYKCHK1"           TO  MCP-WIDGET
               WHEN    8
                   MOVE    "IYKCHK2"           TO  MCP-WIDGET
               WHEN    9
                   MOVE    "IYKCHK3"           TO  MCP-WIDGET
               WHEN    10
                   MOVE    "IYKCHK4"           TO  MCP-WIDGET
               WHEN    11
                   MOVE    "IYKCHK5"           TO  MCP-WIDGET
               WHEN    12
                   MOVE    "IYKCHK6"           TO  MCP-WIDGET
               WHEN    13
                   MOVE    "IYKCHK7"           TO  MCP-WIDGET
               WHEN    14
                   MOVE    "TOKCHK"            TO  MCP-WIDGET
               WHEN    15
                   MOVE    "HOKACHK"           TO  MCP-WIDGET
               WHEN    16
                   MOVE    "JIHCHK"            TO  MCP-WIDGET
               WHEN    17
                   MOVE    "COMCHKALL"         TO  MCP-WIDGET
               WHEN    18
                   MOVE    "COMCHK1"           TO  MCP-WIDGET
               WHEN    19
                   MOVE    "COMCHK2"           TO  MCP-WIDGET
               WHEN    20
                   MOVE    "COMCHK3"           TO  MCP-WIDGET
               WHEN    21
                   MOVE    "MSTYMD"            TO  MCP-WIDGET
               WHEN    22
                   MOVE    "KEI1"              TO  MCP-WIDGET
               WHEN    23
                   MOVE    "KEI2"              TO  MCP-WIDGET
               WHEN    24 
                   MOVE    "CYU1"              TO  MCP-WIDGET
               WHEN    25
                   MOVE    "CYU2"              TO  MCP-WIDGET
               WHEN    26
                   MOVE    "CYU3"              TO  MCP-WIDGET
               WHEN    27
                   MOVE    "SYU1"              TO  MCP-WIDGET
               WHEN    28
                   MOVE    "SYU2"              TO  MCP-WIDGET
               WHEN    29
                   MOVE    "SYU3"              TO  MCP-WIDGET
               WHEN    30
                   MOVE    "SYU4"              TO  MCP-WIDGET
               WHEN    31
                   MOVE    "KZICHK"            TO  MCP-WIDGET
               WHEN    32
                   MOVE    "IPNCHKALL"         TO  MCP-WIDGET
               WHEN    33
                   MOVE    "IPN"               TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0002"
                   MOVE    "日付入力エラー"           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "日付に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "パラメタが入力されてない項目があります"
                                                      TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "診療区分選択エラー"       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "診療区分が選択されていません"
                                                      TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "開始より終了が小さくなっています"
                                                      TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "出力ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-SHORIERR               TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-SHORIERR               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZERR
           MOVE    SPA-ERRCD            TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  ZERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Z91"                TO  SPA-MOTOPG
           MOVE    "ZERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "6666"
                   MOVE    "印刷処理中です"
                                            TO  WRK-ZIDMSG
               WHEN    "8888"
                   MOVE    "処理が正常終了しました"
                                            TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z91"                TO  SPA-MOTOPG
           MOVE    "ZID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-ZIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       540-ZID3SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
           WHEN    "1001"
               MOVE    "設定内容に従い帳票を印刷します"
                                       TO  WRK-ZIDMSG
           WHEN    "1002"
               MOVE    "設定内容に従い入力コード帳票を印刷します"
                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  ZID3
           INITIALIZE                      ZID3
           MOVE    SPA-ZIDCD           TO  ZID3-ID1CODE
           MOVE    WRK-ZIDMSG          TO  ZID3-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "Z91"               TO  SPA-MOTOPG
           MOVE    "ZID3"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "ZID3"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-ZID3SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
           INITIALIZE                            STS-AREA-DAY
           IF  WRK-LNKYMD  NOT  =  SPACE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                   MOVE    SGDAY-SDAY          TO  WRK-SYMD
               ELSE
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL   "ORCDBMAIN"                    USING
                                                     MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
