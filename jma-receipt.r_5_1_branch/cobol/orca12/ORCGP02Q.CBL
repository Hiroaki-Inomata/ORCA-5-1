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
       PROGRAM-ID.             ORCGP02Q.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : ＱＲ受付一覧画面（Ｐ０２Ｑ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/02/13    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-QRCDHKN         PIC 9(01).
           03  FLG-PTHKNINF         PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-QROK            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TIME.
               05  WRK-THH             PIC 9(02).
               05  WRK-TMM             PIC 9(02).
               05  WRK-TSS             PIC 9(02).
           03  WRK-THKNID              PIC 9(10).
      *
           03  WRK-PIDMSG              PIC X(80).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH-H       PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM-H       PIC Z9.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    QRコード(被保険者証)
       01  QRCDHKN-REC.
           COPY    "CPQRCDHKN.INC".
      *    患者情報
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    検索文字追加
           COPY    "CPORCSADDSIGN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
      *
       PROCEDURE                   DIVISION    USING
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面編集後、表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU     TO  SPA-WORK
           MOVE    SPA-AREA          TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA    TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02Q"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
               WHEN    "P02Q1"
                   PERFORM 3002-P02Q1-INIT-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者確認画面より
      *****************************************************************
       3002-P02Q1-INIT-SEC            SECTION.
      *
           IF      SPA-QR-SYORI        =   SPACE
      *        ＱＲ選択中の解除
               MOVE    SPA-P02Q-SYSYMD     TO  SPA-QR-REGISTYMD
               MOVE    SPA-P02Q-REGISTID   TO  SPA-QR-REGISTID
               PERFORM 30009-QRCDHKN-CR-SEC
      *
               MOVE    SPA-P02Q-SYSYMD     TO  WRK-SYMD
               INITIALIZE                      SPA-P02Q-AREA
               INITIALIZE                      SPA-QRDATA-AREA
      *        ＳＰＡ編集
               MOVE    WRK-SYMD            TO  SPA-P02Q-SYSYMD
      *        ＱＲ編集
               PERFORM 3001-QRCDHKN-SYORI-SEC
               MOVE    1                   TO  SPA-P02Q-CUR
           ELSE
               PERFORM 210-BACK
           END-IF
           .
       3002-P02Q1-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
      *    ＱＲ選択中の解除
           IF      (SPA-QR-REGISTYMD   NOT =   SPACE )  AND
                   (SPA-QR-REGISTID    NOT =   ZERO  )
               PERFORM 30009-QRCDHKN-CR-SEC
           END-IF
      *
           INITIALIZE                      SPA-P02Q-AREA
           INITIALIZE                      SPA-QRDATA-AREA
      *    ＳＰＡ編集
           MOVE    SPA-SYSYMD          TO  SPA-P02Q-SYSYMD
      *
      *    ＱＲ編集
           PERFORM 3001-QRCDHKN-SYORI-SEC
           IF      SPA-P02Q-MAX        =   ZERO
               MOVE    99                  TO  SPA-P02Q-CUR
           ELSE
               MOVE    1                   TO  SPA-P02Q-CUR
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ編集処理
      *****************************************************************
       3001-QRCDHKN-SYORI-SEC              SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-P02Q-SYSYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  SPA-P02Q-HSYSYMD
      *
           MOVE    ZERO                TO  SPA-P02Q-MAX
           INITIALIZE                      SPA-P02Q-QRLIST
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-P02Q-SYSYMD     TO  QRCDHKN-REGISTYMD
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL  ( FLG-QRCDHKN    =   1  )
                   OR     ( IDX            >=  50 )
               MOVE    ZERO            TO  FLG-QROK
               IF     (SPA-GMN-PTID        =   ZERO )
                   MOVE    1               TO  FLG-QROK
               END-IF
      *        患者番号選択時、性別・生年月日の一致するデータのみ
               IF    (SPA-GMN-PTID     NOT =   ZERO )  AND
                     (SPA-GMN-SEX          =   QRCDHKN-SEX) AND
                     (SPA-NAI-BIRTHDAY     =   QRCDHKN-BIRTHDAYS)
                   MOVE    1               TO  FLG-QROK
               END-IF
               IF      FLG-QROK            =   1
                   ADD    1                    TO  IDX
                   PERFORM 30011-QRCDHKN-HEN-SEC
               END-IF
      *
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           END-PERFORM
           MOVE    IDX                 TO  SPA-P02Q-MAX
      *
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-QRCDHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表編集処理
      *****************************************************************
       30011-QRCDHKN-HEN-SEC              SECTION.
      *
           MOVE    IDX                 TO  SPA-P02Q-TNUM (IDX)
           MOVE    QRCDHKN-REGISTID    TO  SPA-P02Q-TREGISTID (IDX)
      *    受付時間
           MOVE    QRCDHKN-REGISTTIME  TO  WRK-TIME
           MOVE    WRK-THH             TO  WRK-THH-H
           MOVE    WRK-TMM             TO  WRK-TMM-H
           MOVE    WRK-HENTIME         TO  SPA-P02Q-TTIME (IDX)
      *    保険者番号
           MOVE    QRCDHKN-HKNJANUM    TO  SPA-P02Q-THKNJANUM (IDX)
      *    記号番号
           STRING  QRCDHKN-KIGO        DELIMITED  BY  SPACE
                   "・"                DELIMITED  BY  SIZE
                   QRCDHKN-NUM         DELIMITED  BY  SPACE
                                   INTO    SPA-P02Q-TKIGONUM (IDX)
           END-STRING
      *    本人家族区分
           EVALUATE    QRCDHKN-HONKZKKBN
               WHEN    "1"
                   MOVE    "本人"          TO  SPA-P02Q-THONKZKKBN (IDX)
               WHEN    "2"
                   MOVE    "家族"          TO  SPA-P02Q-THONKZKKBN (IDX)
           END-EVALUATE
      *    カナ氏名
           MOVE    QRCDHKN-KANANAME    TO  SPA-P02Q-TKANANAME(IDX)
      *    性別
           EVALUATE    QRCDHKN-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-P02Q-TSEX (IDX)
               WHEN    "2"
                   MOVE    "女"            TO  SPA-P02Q-TSEX (IDX)
           END-EVALUATE
      *    生年月日
           IF      QRCDHKN-BIRTHDAYS   =   SPACE
               MOVE    QRCDHKN-BIRTHDAY    TO  SPA-P02Q-TBIRTHDAY-H(IDX)
               MOVE    9                   TO  SPA-P02Q-TJYOTAIFLG (IDX)
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    QRCDHKN-BIRTHDAYS   TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  SPA-P02Q-TBIRTHDAY-H
                                                                  (IDX)
               MOVE    QRCDHKN-BIRTHDAYS   TO  SPA-P02Q-TBIRTHDAY (IDX)
           END-IF
      *    氏名
           MOVE    QRCDHKN-NAME        TO  SPA-P02Q-TNAME(IDX)
      *    患者番号
           IF      QRCDHKN-PTID    NOT =   ZERO
               PERFORM 300112-PTNUM-HEN-SEC
           END-IF
      *    状態
           IF      QRCDHKN-ERRCD   NOT =   SPACE
               MOVE    9               TO  SPA-P02Q-TJYOTAIFLG (IDX)
               MOVE    "エラー"        TO  SPA-P02Q-TJYOTAI (IDX)
           ELSE
               IF     (QRCDHKN-PTID        =   ZERO )  OR
                      (SPA-P02Q-TPTID (IDX)    =  ZERO)
                   PERFORM 300111-JYOTAI-SYORI-SEC
               ELSE
                   MOVE    1               TO  SPA-P02Q-TJYOTAIFLG (IDX)
                   MOVE    "確定"          TO  SPA-P02Q-TJYOTAI (IDX)
      *            確定の保険ＩＤ
                   PERFORM 300112-HKNID-HEN-SEC
               END-IF
           END-IF
           .
       30011-QRCDHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲ解除処理
      *****************************************************************
       30009-QRCDHKN-CR-SEC              SECTION.
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-QR-REGISTYMD    TO  QRCDHKN-REGISTYMD
           MOVE    SPA-QR-REGISTID     TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    ZERO                TO  QRCDHKN-MOD-FLG
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "P02Q QRCDHKN KEY UPD ERR:"  MCP-RC
                       ",KEY:" QRCDHKN-KEY
               END-IF
           END-IF
           .
       30009-QRCDHKN-CR-EXT.
           EXIT.
      *****************************************************************
      *    患者番号編集処理
      *****************************************************************
       300112-PTNUM-HEN-SEC              SECTION.
      *
      *    患者番号変換(管理）を検索し、患者IDより患者NOを取得
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    QRCDHKN-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  SPA-P02Q-TPTNUM (IDX)
               MOVE    QRCDHKN-PTID    TO  SPA-P02Q-TPTID  (IDX)
           END-IF
           .
       300112-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    状態編集処理
      *****************************************************************
       300111-JYOTAI-SYORI-SEC              SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    QRCDHKN-BIRTHDAYS   TO  PTINF-BIRTHDAY
           MOVE    QRCDHKN-SEX         TO  PTINF-SEX
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-CHK
           PERFORM   UNTIL  ( FLG-PTINF    =   1  )
               ADD     1                   TO  IDY
               IF     (QRCDHKN-KANANAME    NOT =   SPACE )  AND
                      (QRCDHKN-NAME            =   SPACE )
                   IF     (PTINF-KANANAME  =   QRCDHKN-KANANAME )
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDY                 =   ZERO
               MOVE    2               TO  SPA-P02Q-TJYOTAIFLG (IDX)
               MOVE    "新規"          TO  SPA-P02Q-TJYOTAI  (IDX)
           ELSE
               MOVE    3               TO  SPA-P02Q-TJYOTAIFLG (IDX)
               MOVE    "確認"          TO  SPA-P02Q-TJYOTAI  (IDX)
           END-IF
           .
       300111-JYOTAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定の保険ＩＤ処理
      *****************************************************************
       300112-HKNID-HEN-SEC              SECTION.
      *
      *    保険情報検索
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    QRCDHKN-PTID        TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
           MOVE    ZERO                TO  WRK-THKNID
           PERFORM UNTIL  (FLG-PTHKNINF    =   1   )
                      OR  (FLG-OK          =   1   )
               IF     (PTHKN-SAKJOKBN      NOT =   SPACE )  OR
                      (PTHKN-HKNNUM(1:1)       =   "9"   )
                   CONTINUE
               ELSE
                   IF     (PTHKN-HKNJANUM      =   QRCDHKN-HKNJANUM) 
                      AND (PTHKN-KIGO          =   QRCDHKN-KIGO )
                      AND (PTHKN-NUM           =   QRCDHKN-NUM )
                      AND (PTHKN-HONKZKKBN     =   QRCDHKN-HONKZKKBN )
                       IF     (PTHKN-TEKSTYMD  <=  SPA-SYSYMD )  AND
                              (PTHKN-TEKEDYMD  >=  SPA-SYSYMD )
                           MOVE    PTHKN-HKNID     TO  SPA-P02Q-THKNID
                                                                 (IDX)
                           MOVE    1               TO  FLG-OK
                       ELSE
                           MOVE    PTHKN-HKNID     TO  WRK-THKNID
                       END-IF
                   ELSE
                       IF     (PTHKN-TEKSTYMD  <=  SPA-SYSYMD )  AND
                              (PTHKN-TEKEDYMD  >=  SPA-SYSYMD )
                           MOVE    1                   TO  FLG-OK2
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    当日有効は別の保険のみ
           IF     (FLG-OK              =   ZERO )  AND
                  (FLG-OK2             =   1    )
               MOVE    ZERO            TO  SPA-P02Q-THKNID (IDX)
           END-IF
      *    当日有効な保険がなく、保険情報が一致する
           IF     (FLG-OK              =   ZERO )  AND
                  (FLG-OK2             =   ZERO )  AND
                  (WRK-THKNID      NOT =   ZERO )
               MOVE    WRK-THKNID      TO  SPA-P02Q-THKNID (IDX)
           END-IF
           .
       300112-HKNID-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
      *            削除
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4031-DELETE-SEC
                   END-IF
               WHEN    "0102"
      *            新規患者
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    SPA-P02Q-SELNUM     TO  IDX
                       INITIALIZE              SPA-QRDATA-AREA
                       MOVE    "1"             TO  SPA-QR-SYORI
                       PERFORM 490-KAKUTEI-SYORI-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P02Q
           MOVE    SPA-P02Q-HSYSYMD    TO  P02Q-REGISTYMD
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-P02Q-MAX
               MOVE    SPA-P02Q-TNUM   (IDX)   TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  P02Q-TNUM  (IDX)
               MOVE    SPA-P02Q-TTIME  (IDX)   TO  P02Q-TTIME (IDX)
               MOVE    SPA-P02Q-THKNJANUM (IDX)
                                               TO  P02Q-THKNJANUM (IDX)
               MOVE    SPA-P02Q-TKIGONUM (IDX)
                                               TO  P02Q-TKIGONUM (IDX)
               MOVE    SPA-P02Q-THONKZKKBN (IDX)
                                               TO  P02Q-THONKZKKBN (IDX)
               MOVE    SPA-P02Q-TKANANAME  (IDX)
                                               TO  P02Q-TKANANAME (IDX)
               MOVE    SPA-P02Q-TSEX   (IDX)   TO  P02Q-TSEX   (IDX)
               MOVE    SPA-P02Q-TBIRTHDAY-H(IDX)
                                               TO  P02Q-TBIRTHDAY (IDX)
               MOVE    SPA-P02Q-TNAME  (IDX)   TO  P02Q-TNAME  (IDX)
               MOVE    SPA-P02Q-TPTNUM (IDX)   TO  P02Q-TPTNUM (IDX)
               MOVE    SPA-P02Q-TJYOTAI(IDX)   TO  P02Q-TJYOTAI (IDX)
               IF      IDX                 =   SPA-P02Q-SELNUM
                   MOVE    "T"                 TO  P02Q-SELECT
                                                               (IDX)
               ELSE
                   MOVE    "F"                 TO  P02Q-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-P02Q-MAX        TO  P02Q-COUNT
      *
           MOVE    SPA-P02Q-SELNUM     TO  P02Q-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-P02Q-CUR
               WHEN    01
                   MOVE    "SELNUM"    TO  MCP-WIDGET
               WHEN    08
                   MOVE    "B08"       TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B10"       TO  MCP-WIDGET
               WHEN    12
                   MOVE    "B12"       TO  MCP-WIDGET
               WHEN    03
                   MOVE    "B03"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 403-DELETE-SYORI-SEC
      *    前日
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 405-BACKDAY-SEC
      *    次日
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 406-NEXTDAY-SEC
      *    新規患者
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 408-SINKI-SYORI-SEC
      *    患者確認
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-P02Q1-SYORI-SEC
      *    患者確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 412-KAKUTEI-SYORI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
           MOVE    ZERO                TO  SPA-P02Q-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        リスト選択
               WHEN    ANY             ALSO    "QRLIST"
                   PERFORM 4001-QRLIST-SEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4001-QRLIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-P02Q-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02Q-MAX
               IF      P02Q-SELECT (IDX)   =   "T"
                   MOVE    IDX             TO  SPA-P02Q-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-P02Q-SELNUM    NOT =   ZERO
               PERFORM 4102-SELNUM-CHK-SEC
           END-IF
      *
           .
       4001-QRLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4102-SELNUM-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02Q-SELNUM         TO  SPA-P02Q-SELNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       4102-SELNUM-CHK-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-P02Q-CUR
      *
           IF     (SPA-P02Q-SELNUM     =   ZERO )  OR
                  (SPA-P02Q-SELNUM     >   SPA-P02Q-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-P02Q-SELNUM     TO  IDX
               EVALUATE    SPA-P02Q-TJYOTAIFLG (IDX)
                   WHEN    9
      *                変換エラー
                       MOVE    03             TO  SPA-P02Q-CUR
                   WHEN    1
      *                確定
                       MOVE    12             TO  SPA-P02Q-CUR
                   WHEN    2
      *                新規
                       MOVE    08             TO  SPA-P02Q-CUR
                   WHEN    OTHER
      *                確認
                       MOVE    10             TO  SPA-P02Q-CUR
               END-EVALUATE
           END-IF
           .
      *
       4102-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-P02Q-SELNUM
           MOVE    1                   TO  SPA-P02Q-CUR
           .
      *
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       403-DELETE-SYORI-SEC             SECTION.
      *
           IF     (SPA-P02Q-SELNUM     =   ZERO )  OR
                  (SPA-P02Q-SELNUM     >   SPA-P02Q-MAX)
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    "0101"              TO  SPA-PIDCD
           END-IF
           .
      *
       403-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DELETE-SEC             SECTION.
      *
           MOVE    SPA-P02Q-SELNUM     TO  IDX
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-P02Q-SYSYMD     TO  QRCDHKN-REGISTYMD
           MOVE    SPA-P02Q-TREGISTID (IDX)
                                       TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    "1"                 TO  QRCDHKN-DELKBN
               MOVE    SMCNDATE-YMD        TO  QRCDHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  QRCDHKN-UPHMS
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "P02Q QRCDHKN KEY UPD ERR:"  MCP-RC
                       ",KEY:" QRCDHKN-KEY
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-P02Q-SELNUM
           IF      SPA-ERRCD           =   SPACE
      *    ＱＲ編集
               PERFORM 3001-QRCDHKN-SYORI-SEC
               IF      SPA-P02Q-MAX        =   ZERO
                   MOVE    99                  TO  SPA-P02Q-CUR
               ELSE
                   MOVE    1                   TO  SPA-P02Q-CUR
               END-IF
           END-IF
      *
           .
       4031-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日処理
      *****************************************************************
        405-BACKDAY-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-P02Q-SELNUM
           MOVE    1                   TO  SPA-P02Q-CUR
      *    １日前
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    SPA-P02Q-SYSYMD  TO  LNK-DAY6-KIJUN
           MOVE    "1"              TO  LNK-DAY6-ZOGENPTN
           MOVE    -1               TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF      STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  SPA-P02Q-SYSYMD
           END-IF
      *    ＱＲ編集
           PERFORM 3001-QRCDHKN-SYORI-SEC
           IF      SPA-P02Q-MAX        =   ZERO
               MOVE    99                  TO  SPA-P02Q-CUR
           ELSE
               MOVE    1                   TO  SPA-P02Q-CUR
           END-IF
           .
      *
        405-BACKDAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    次日処理
      *****************************************************************
        406-NEXTDAY-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-P02Q-SELNUM
           MOVE    1                   TO  SPA-P02Q-CUR
      *    １日前
           MOVE    SPACE            TO  STS-AREA-DAY
           INITIALIZE                   STS-AREA-DAY
           INITIALIZE                   LNK-DAY6-AREA
           MOVE    "61"             TO  LNK-DAY6-IRAI
           MOVE    SPA-P02Q-SYSYMD  TO  LNK-DAY6-KIJUN
           MOVE    "1"              TO  LNK-DAY6-ZOGENPTN
           MOVE    1                TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"    USING   STS-AREA-DAY
                                        LNK-DAY6-AREA
           IF      STS-DAY-RC1          =   ZERO
               MOVE    LNK-DAY6-KEISAN  TO  SPA-P02Q-SYSYMD
           END-IF
      *    ＱＲ編集
           PERFORM 3001-QRCDHKN-SYORI-SEC
           IF      SPA-P02Q-MAX        =   ZERO
               MOVE    99                  TO  SPA-P02Q-CUR
           ELSE
               MOVE    1                   TO  SPA-P02Q-CUR
           END-IF
           .
      *
        406-NEXTDAY-EXT.
           EXIT.
      *****************************************************************
      *     新規患者処理
      *****************************************************************
        408-SINKI-SYORI-SEC             SECTION.
      *
           IF     (SPA-P02Q-SELNUM     =   ZERO )  OR
                  (SPA-P02Q-SELNUM     >   SPA-P02Q-MAX)
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    SPA-P02Q-SELNUM     TO  IDX
               IF      SPA-P02Q-TJYOTAIFLG (IDX)   =   9
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
               IF      SPA-P02Q-TJYOTAIFLG (IDX)   =   1
                   MOVE    "0102"          TO  SPA-PIDCD
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  ( SPA-PIDCD          =   SPACE )
               INITIALIZE                  SPA-QRDATA-AREA
               MOVE    "1"                 TO  SPA-QR-SYORI
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
      *
        408-SINKI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *     患者確認画面処理
      *****************************************************************
       410-P02Q1-SYORI-SEC             SECTION.
      *
           IF     (SPA-P02Q-SELNUM     =   ZERO )  OR
                  (SPA-P02Q-SELNUM     >   SPA-P02Q-MAX)
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               IF      SPA-P02Q-TJYOTAIFLG (IDX)   =   9
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *
           MOVE    SPA-P02Q-SELNUM     TO  IDX
           MOVE    SPA-P02Q-TREGISTID (IDX)
                                       TO  SPA-P02Q-REGISTID
      *    初期画面編集
           INITIALIZE                      SPA-P02Q1-AREA
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02Q1"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    "P02Q"              TO  SPA-MOTOPG
           MOVE    "P02Q1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P02Q1"             TO  MCP-WINDOW
           MOVE    "SELNUM"            TO  MCP-WIDGET
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           END-IF
           .
      *
       410-P02Q1-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者確定処理
      *****************************************************************
       412-KAKUTEI-SYORI-SEC             SECTION.
      *
           IF     (SPA-P02Q-SELNUM     =   ZERO )  OR
                  (SPA-P02Q-SELNUM     >   SPA-P02Q-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-P02Q-SELNUM     TO  IDX
               IF      SPA-P02Q-TJYOTAIFLG (IDX)  =   9
                   MOVE    "0002"          TO  SPA-ERRCD
               ELSE
                   IF      SPA-P02Q-TPTID (IDX)    =   ZERO
                       MOVE    "0003"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        患者確定
               INITIALIZE                  SPA-QRDATA-AREA
               MOVE    "2"                 TO  SPA-QR-SYORI
               MOVE    SPA-P02Q-TPTID (IDX)    TO  SPA-QR-PTID
               MOVE    SPA-P02Q-TPTNUM(IDX)    TO  SPA-QR-PTNUM
               MOVE    SPA-P02Q-THKNID(IDX)    TO  SPA-QR-HKNID
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
      *
       412-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者確定処理
      *****************************************************************
       490-KAKUTEI-SYORI-SEC           SECTION.
      *
           MOVE    SPA-P02Q-SYSYMD     TO  SPA-QR-REGISTYMD
           MOVE    SPA-P02Q-TREGISTID (IDX)
                                       TO  SPA-QR-REGISTID
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-QR-REGISTYMD    TO  QRCDHKN-REGISTYMD
           MOVE    SPA-QR-REGISTID     TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    9                   TO  QRCDHKN-MOD-FLG
               MOVE    SMCNDATE-YMD        TO  QRCDHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  QRCDHKN-UPHMS
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "P02Q QRCDHKN KEY UPD ERR:"  MCP-RC
                       ",KEY:" QRCDHKN-KEY
               END-IF
           END-IF
      *
           PERFORM 210-BACK
           .
      *
       490-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02Q"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    ZERO                TO  SPA-FLG-END
           MOVE    "P02"               TO  SPA-P02-GAMEN
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P02Q"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING  "変換エラーのデータです。"
                           "削除のみ行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "患者が確定されていません。"
                           "患者確認を行って下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "ＱＲデータ更新ができませんでした。"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02Q"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PIDMSG
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   STRING  "選択した情報を削除します。"
                           "よろしいですか？"  DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "保険情報・性別・生年月日で同一の患者が"
                           "あります。新規患者でよろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P02Q"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    QRコード(被保険者証)読込
      *****************************************************************
       900-QRCDHKN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  QRCDHKN-REC
               MOVE    ZERO            TO  FLG-QRCDHKN
           ELSE
               INITIALIZE                  QRCDHKN-REC
               MOVE    1               TO  FLG-QRCDHKN
           END-IF
           .
       900-QRCDHKN-READ-EXT.
           EXIT.
      *****************************************************************
      **    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
