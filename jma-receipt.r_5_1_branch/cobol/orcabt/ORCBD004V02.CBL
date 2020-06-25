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
       PROGRAM-ID.             ORCBD004V02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 収納一覧表（ＨＣＭ０７）外来 期間指定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/02/27    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 00.00.01     NACL-森脇    03/04/25  NYUGAIKBNキーに追加
      *                                     使用するHOSPIDの変更
      * 00.00.02     NACL-森脇    03/05/02  金額をカンマ編集
      * 00.00.03     NACL-森脇    03/06/05  印刷区分追加
      * 00.00.04     NACL-森脇    03/12/11  処理区分表示修正
      * 03.05.01     NACL-森脇    07/06/08  グループ診療対応
      * 04.08.01     NACL-森脇    14/07/28  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      * 
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
grpsys*DD  COPY    "CPCOMMONDAT2.INC"
grpsys*DD                          REPLACING   //RECE01PARA//
grpsys*DD                          BY          //RECEERR//.
grpsys*DD  03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *----(00.00.03)--UPD-START---
      *     COPY    "HCM07.INC".
           COPY    "HCM07V02.INC"  REPLACING   //HCM07V02//
                                   BY          //HCM07//.
      *----(00.00.03)--UPD-END-----
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYU             PIC 9(01).
           03  FLG-SMEI            PIC 9(01).
           03  FLG-SMEI2           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(06).
           03  CNT-DENP            PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID     PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM    PIC 9(02).
      *----(04.08.01)--UPD-START---
grpsys*     03  RECEERR             PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-SYORIKBN   PIC X(01).
           03  WRK-PARA-SORTKBN    PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-MAE-INPUT       PIC X(300).
           03  WRK-OUT-INPUT       PIC X(300).
           03  WRK-OUT-MAX         PIC 9(05).
           03  WRK-KIKAN-G.
               05  WRK-KIKANSTYMD  PIC X(22).
               05  WRK-KIKANSTTIME.
                   07  WRK-KIKANSTTIME-HH
                                   PIC X(04).
                   07  WRK-KIKANSTTIME-MM
                                   PIC X(04).
               05  WRK-KIKANEDYMD  PIC X(22).
               05  WRK-KIKANEDTIME.
                   07  WRK-KIKANEDTIME-HH
                                   PIC X(04).
                   07  WRK-KIKANEDTIME-MM
                                   PIC X(04).
      *
      *----(00.00.02)--UPD-START---
           03  WRK-Z9              PIC ---,---,--9.
      *----(00.00.02)--UPD-END-----
      *
           03  WRK-SRYKA           PIC X(20).
           03  WRK-SYORIYMD        PIC X(24).
           03  WRK-PAGE            PIC ZZ9.
      *     03  WRK-HSRYMD          PIC X(12).
      *
           03  WRK-NAI-TBL.
               05  WRK-NAI-G       OCCURS  30.
                   07  WRK-PTNUM       PIC X(20).
                   07  WRK-NAME        PIC X(36).
                   07  WRK-SEX         PIC X(2).
                   07  WRK-BIRTHDAY    PIC X(22).
      *----(00.00.03)--UPD-START---
                   07  WRK-DENPNUM     PIC 9(07).
                   07  WRK-DENPNUM-X   REDEFINES  WRK-DENPNUM
                                       PIC X(07).
      *----(00.00.03)--UPD-END-----
                   07  WRK-SRYMD       PIC X(12).
                   07  WRK-SEIKYU      PIC S9(07).
                   07  WRK-RYOSYU      PIC S9(07).
                   07  WRK-MISYU       PIC S9(07).
                   07  WRK-JYOTAI      PIC X(10).
      *
           03  WRK-SUM-TBL         OCCURS  4.
               05  WRK-GSEIKYU     PIC S9(08).
               05  WRK-GRYOSYU     PIC S9(08).
               05  WRK-GMISYU      PIC S9(08).
           03  WRK-STDAILYKEY.
               05  WRK-STDAILYKEY-YMD  PIC X(08).
               05  WRK-STDAILYKEY-TIME PIC X(04).
           03  WRK-EDDAILYKEY.
               05  WRK-EDDAILYKEY-YMD  PIC X(08).
               05  WRK-EDDAILYKEY-TIME PIC X(04).
           03  WRK-STR                 PIC X(300).
           03  WRK-MAIN-PATHNAME   PIC X(64).
           03  WRK-CREYMD          PIC X(30).
      *
       01  KEY-AREA.
           03  KEY-NEW-TBL.
               05  KEY-N-SRYKA     PIC X(02).
               05  KEY-N-PTID      PIC 9(10).
               05  KEY-N-DENPNUM   PIC 9(07).
           03  KEY-OLD-TBL.
               05  KEY-O-SRYKA     PIC X(02).
               05  KEY-O-PTID      PIC 9(10).
               05  KEY-O-DENPNUM   PIC 9(07).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    締め時刻情報
           COPY    "CPSK3005.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    収納マスタ
       01  SYU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納明細履歴マスタ
       01  SMEI2-REC.
           COPY    "CPSYUMEI.INC"  REPLACING   //SMEI//
                                   BY          //SMEI2//.
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    文字列変換
           COPY    "CPORCXKANACONV.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
grpsys     COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ****************************************************************
       PROCEDURE               DIVISION
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
                   UNTIL   FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  KEY-AREA
grpsys     INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           PERFORM 100-DBOPEN-SEC
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
grpsys*DD                                      WRK-PARA-HOSPID
grpsys                                         WRK-PARA-HOSPNUM
grpsys                                         RECEERR
                                               WRK-PARA-SYORIKBN
                                               WRK-PARA-SORTKBN
grpsys     END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBD004V02"   TO  JOB-PGID
           MOVE    "収納一覧表（期間指定）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
grpsys*DD  MOVE    "RECEERR"       TO  RECEERR-FILE-ID
grpsys*DD  MOVE    LNK-PRTKANRI-TERMID
grpsys*DD                          TO  RECEERR-TERMID
      *
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  WRK-SYSYMD
      *
      *    締め時刻情報
           INITIALIZE                  SYS-3005-REC
           MOVE    "3005"          TO  SYS-3005-KANRICD
           MOVE    "*"             TO  SYS-3005-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-3005-STYUKYMD
                                       SYS-3005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-3005-HOSPNUM
           MOVE    SYS-3005-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-3005-REC
      *
               MOVE    SYS-3005-STSRYYMD   TO  WRK-STDAILYKEY-YMD
               MOVE    SYS-3005-STTIME     TO  WRK-STDAILYKEY-TIME
               MOVE    SYS-3005-EDSRYYMD   TO  WRK-EDDAILYKEY-YMD
               MOVE    SYS-3005-EDTIME     TO  WRK-EDDAILYKEY-TIME
      *
               MOVE    SYS-3005-STSRYYMD   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-KIKANSTYMD
      *
               MOVE    SYS-3005-STTIME     TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  WRK-KIKANSTTIME
      *
               MOVE    SYS-3005-EDSRYYMD   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               IF    ( SYS-3005-STSRYYMD (1:4)
                                           =   SYS-3005-EDSRYYMD (1:4))
                   MOVE    WRK-HENYMDG (11:)   TO  WRK-KIKANEDYMD
               ELSE
                   MOVE    WRK-HENYMDG (5:)    TO  WRK-KIKANEDYMD
               END-IF
      *
               MOVE    SYS-3005-EDTIME     TO  WRK-MAE-INPUT
               PERFORM 800-ZENKAKU-HEN-SEC
               MOVE    WRK-OUT-INPUT       TO  WRK-KIKANEDTIME
      *
           ELSE
               MOVE    "締め処理にて対象期間を登録してください"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *----(00.00.03)--ADD-START---
           EVALUATE    WRK-PARA-SYORIKBN
           WHEN    "0"
           WHEN    "1"
           WHEN    "2"
               CONTINUE
           WHEN    SPACE
               MOVE    "0"             TO  WRK-PARA-SYORIKBN
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-RECEERR
               STRING  "処理区分が誤りです：["     DELIMITED   BY  SIZE
                       WRK-PARA-SYORIKBN           DELIMITED   BY  SIZE
                       "]"                         DELIMITED   BY  SIZE
               INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-EVALUATE
      *----(00.00.03)--UPD-END-----
      *
           IF    ( WRK-PARA-SYORIKBN   =   "2" )
               MOVE    SPACE           TO  WRK-PARA-SORTKBN
           END-IF
      *
           IF    ( WRK-PARA-SORTKBN    =   "1" )
               MOVE    "key13"         TO  WRK-MAIN-PATHNAME
           ELSE
               MOVE    "key10"         TO  WRK-MAIN-PATHNAME
           END-IF
      *
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           STRING  "作成日："              DELIMITED   BY  SIZE
                   WRK-HENYMDG             DELIMITED   BY  SIZE
           INTO    WRK-CREYMD
           END-STRING
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票印刷開始処理
           PERFORM 900-DBSELECT-SEC
      *
      *    キーセット
           IF    ( WRK-PARA-SORTKBN    =   "1" )
               MOVE    "00"        TO  KEY-N-SRYKA
           ELSE
               MOVE    SMEI-SRYKA  TO  KEY-N-SRYKA
           END-IF
           MOVE    SMEI-PTID       TO  KEY-N-PTID
           MOVE    SMEI-DENPNUM    TO  KEY-N-DENPNUM
      *
           PERFORM UNTIL   FLG-END     =   1
      *
               MOVE    KEY-N-SRYKA     TO  KEY-O-SRYKA
               INITIALIZE                  WRK-SUM-TBL(2)
      *
               IF    ( WRK-PARA-SORTKBN    =   "1" )
                   CONTINUE
               ELSE
      *            診療科情報
                   INITIALIZE                  SYS-1005-REC
                   MOVE    "1005"          TO  SYS-1005-KANRICD
                   MOVE    KEY-N-SRYKA     TO  SYS-1005-KBNCD
                   MOVE    WRK-SYSYMD      TO  SYS-1005-STYUKYMD
                                               SYS-1005-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
                   MOVE    SYS-1005-REC    TO  MCPDATA-REC
                   PERFORM 800-SYSKANRI-KEY10-SEL-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1005-REC
                       MOVE    SYS-1005-SRYKANAME
                                               TO  WRK-SRYKA
                   END-IF
               END-IF
      *
               PERFORM UNTIL ( FLG-END         =   1 )
                       OR    ( KEY-N-SRYKA NOT =   KEY-O-SRYKA )
      *            帳票編集<見出し>処理
                   PERFORM 310-HEAD-HEN-SEC
      *            帳票編集<明細>処理
                   PERFORM 320-BODY-HEN-SEC
      *            帳票編集<合計>処理
                   PERFORM 330-TERM-HEN-SEC
      *            帳票印刷処理
                   PERFORM 390-PRINT-OUT-SEC
               END-PERFORM
      *
           END-PERFORM
      *
           PERFORM 900-DBCLOSE-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           INITIALIZE                  WRK-SUM-TBL(1)
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
      *
           MOVE    SPACE           TO  HCM07
      *
           IF    ( SYS-3005-STSRYYMD   =   SYS-3005-EDSRYYMD )
               STRING "期間："             DELIMITED   BY  SIZE
                      WRK-KIKANSTYMD       DELIMITED   BY  SIZE
                      WRK-KIKANSTTIME-HH   DELIMITED   BY  SIZE
                      "："                 DELIMITED   BY  SIZE
                      WRK-KIKANSTTIME-MM   DELIMITED   BY  SIZE
                      "〜"                 DELIMITED   BY  SIZE
                      WRK-KIKANEDTIME-HH   DELIMITED   BY  SIZE
                      "："                 DELIMITED   BY  SIZE
                      WRK-KIKANEDTIME-MM   DELIMITED   BY  SIZE
               INTO   HCM07-KIKAN
               END-STRING
           ELSE
               STRING "期間："             DELIMITED   BY  SIZE
                      WRK-KIKANSTYMD       DELIMITED   BY  SIZE
                      WRK-KIKANSTTIME-HH   DELIMITED   BY  SIZE
                      "："                 DELIMITED   BY  SIZE
                      WRK-KIKANSTTIME-MM   DELIMITED   BY  SIZE
                      "〜"                 DELIMITED   BY  SIZE
                      WRK-KIKANEDYMD       DELIMITED   BY  SPACE
                      WRK-KIKANEDTIME-HH   DELIMITED   BY  SIZE
                      "："                 DELIMITED   BY  SIZE
                      WRK-KIKANEDTIME-MM   DELIMITED   BY  SIZE
               INTO   HCM07-KIKAN
               END-STRING
           END-IF
      *
           MOVE    WRK-SRYKA       TO  HCM07-SRYKA
      *
           MOVE    CNT-PAGE        TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  HCM07-PAGE
      *
      *----(00.00.03)--ADD-START---
           EVALUATE    WRK-PARA-SYORIKBN
           WHEN    "0"
               MOVE    "（収納明細別）"    TO  HCM07-SYORIKBN
           WHEN    "1"
               MOVE    "（伝票別）"        TO  HCM07-SYORIKBN
           WHEN    "2"
               MOVE    "（患者別）"        TO  HCM07-SYORIKBN
           END-EVALUATE
      *----(00.00.03)--ADD-END-----
      *
           MOVE    WRK-CREYMD      TO  HCM07-CREYMD
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
           PERFORM UNTIL ( FLG-END         =   1 )
                   OR    ( KEY-N-SRYKA NOT =   KEY-O-SRYKA )
                   OR    ( CNT-LINE        =   30 )
      *
               IF        ( KEY-N-SRYKA NOT =   KEY-O-SRYKA )
                   OR    ( KEY-N-PTID  NOT =   KEY-O-PTID )
                   OR    ( CNT-LINE        =   ZERO )
                   PERFORM 3201-BODY-PTINF-HEN-SEC
               END-IF
      *
      *----(00.00.03)--UPD-START---
      *        帳票編集<明細行>処理
               PERFORM 3201-BODY-NAIYO-HEN-SEC
      *----(00.00.03)--UPD-END-----
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集患者処理
      *****************************************************************
       3201-BODY-PTINF-HEN-SEC     SECTION.
      *
      *    患者情報読み込み
           INITIALIZE                  PTINF-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  PTINF-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *    患者番号変換情報読み込み
           INITIALIZE                  PTNUM-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  PTNUM-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    KEY-N-PTID      TO  PTNUM-PTID
           PERFORM 900-PTNUM-READ-SEC
      *
      *    患者番号
           MOVE    PTNUM-PTNUM     TO  WRK-PTNUM(1)
      *
      *    氏名
           MOVE    PTINF-NAME      TO  WRK-NAME(1)
      *
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"    TO  WRK-SEX(1)
               WHEN    "2"
                   MOVE    "女"    TO  WRK-SEX(1)
           END-EVALUATE
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-BIRTHDAY(1)
      *
           MOVE    KEY-N-PTID      TO  KEY-O-PTID
      *
           .
       3201-BODY-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-BODY-NAIYO-HEN-SEC     SECTION.
      *
      *----(00.00.03)--UPD-START---
           PERFORM UNTIL ( FLG-END             =   1 )
                   OR    ( KEY-N-SRYKA     NOT =   KEY-O-SRYKA )
                   OR    ( KEY-N-PTID      NOT =   KEY-O-PTID )
                   OR    ( CNT-LINE        =   30 )
      *
      *        収納読み込み
               INITIALIZE                  SYU-REC
grpsys*DD      MOVE    WRK-PARA-HOSPID TO  SYU-HOSPID
grpsys         MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
               MOVE    "2"             TO  SYU-NYUGAIKBN
               MOVE    KEY-N-PTID      TO  SYU-PTID
               MOVE    KEY-N-DENPNUM   TO  SYU-DENPNUM
               PERFORM 900-SYU-READ-SEC
      *
      *        伝票履歴チェック
               PERFORM 900-SMEI2-SELECT-SEC
               PERFORM UNTIL    FLG-SMEI2  =   1
                   COMPUTE WRK-GSEIKYU(4)  =   WRK-GSEIKYU(4)
                                           +   SMEI2-SKYMONEY
                   COMPUTE WRK-GRYOSYU(4)  =   WRK-GRYOSYU(4)
                                           +   SMEI2-NYUHEN-MONEY
                   PERFORM 900-SMEI2-FETCH-SEC
               END-PERFORM
               PERFORM 900-SMEI2-CLOSE-SEC
      *
      *        帳票編集<明細行>処理
               PERFORM 32012-BODY-NAIYO-HEN-SEC
      *
               IF      WRK-PARA-SYORIKBN   =   "2"
                   MOVE    SMEI-PTID       TO  KEY-N-PTID
                   MOVE    SMEI-DENPNUM    TO  KEY-N-DENPNUM
               ELSE
                   COMPUTE IDY         =   30  -   CNT-LINE
      *
                   IF        ( CNT-DENP    >  IDY )
                       MOVE    30          TO  CNT-LINE
                   ELSE
      *                帳票編集<明細行>セット処理
                       PERFORM 3202-BODY-NAIYO-SET-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF          WRK-PARA-SYORIKBN       =   "2"
               COMPUTE IDY         =   30  -   CNT-LINE
      *
               IF        ( CNT-DENP    >  IDY )
                   MOVE    30          TO  CNT-LINE
               ELSE
      *            帳票編集<明細行>セット処理
                   PERFORM 3202-BODY-NAIYO-SET-SEC
               END-IF
           END-IF
      *
           .
       3201-BODY-NAIYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       32012-BODY-NAIYO-HEN-SEC     SECTION.
      *
           PERFORM UNTIL ( FLG-END             =   1 )
                   OR    ( KEY-N-SRYKA     NOT =   KEY-O-SRYKA )
                   OR    ( KEY-N-PTID      NOT =   KEY-O-PTID )
                   OR    ( KEY-N-DENPNUM   NOT =   SMEI-DENPNUM )
      *
               IF      CNT-DENP            =   30
                   GO  TO  32012-BODY-NAIYO-HEN-EXT
               END-IF
      *
               EVALUATE    WRK-PARA-SYORIKBN
               WHEN    "1"
               WHEN    "2"
                   MOVE    1                   TO  CNT-DENP
               WHEN    "0"
                   ADD     1                   TO  CNT-DENP
               END-EVALUATE
      *
      *        伝票番号
               IF        ( WRK-DENPNUM-X(CNT-DENP) =   SPACE )
                   OR    ( WRK-DENPNUM-X(CNT-DENP) =   ZERO )
                   MOVE    SMEI-DENPNUM    TO  WRK-DENPNUM-X(CNT-DENP)
      *            診療月日
                   IF      SYU-SRYYMD      NOT =   WRK-EDDAILYKEY-YMD
      *                診療月日編集
                       MOVE    SYU-SRYYMD          TO  WRK-SYMD
                       PERFORM 31012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG(11:12)
                                           TO  WRK-SRYMD(CNT-DENP)
                   ELSE
                       MOVE    SPACE       TO  WRK-SRYMD(CNT-DENP)
                   END-IF
               ELSE
                   IF      WRK-DENPNUM-X(CNT-DENP) NOT =   SMEI-DENPNUM
                       MOVE    "*******"   TO  WRK-DENPNUM-X(CNT-DENP)
                       MOVE    "*******"   TO  WRK-SRYMD(CNT-DENP)
                   END-IF
               END-IF
      *
               IF          WRK-PARA-SYORIKBN       =   "0"
      *            処理区分
                   EVALUATE    SMEI-JOUTAIKBN
                   WHEN    SPACE
                       MOVE    "請求額なし"
                                           TO  WRK-JYOTAI(CNT-DENP)
                   WHEN    "1"
      *----(00.00.04)--UPD-START---
      *                 IF   ( SYU-SKYMONEY
      *                                 >   SMEI-NYUHEN-MONEY )
                       IF   ( SMEI-SKYMONEY
                                       >   SMEI-NYUHEN-MONEY )
      *----(00.00.04)--UPD-END-----
                           MOVE    "請求・未収"
                                           TO  WRK-JYOTAI(CNT-DENP)
                       ELSE
                           MOVE    "請求・入金"
                                           TO  WRK-JYOTAI(CNT-DENP)
                       END-IF
                   WHEN    "2"
                       MOVE    "入金"      TO  WRK-JYOTAI(CNT-DENP)
                   WHEN    "3"
                       MOVE    "再計算"    TO  WRK-JYOTAI(CNT-DENP)
                   WHEN    "4"
                       MOVE    "返金"      TO  WRK-JYOTAI(CNT-DENP)
                   WHEN    "5"
                       MOVE    "請求取消"  TO  WRK-JYOTAI(CNT-DENP)
                   WHEN    "6"
                       MOVE    "入金取消"  TO  WRK-JYOTAI(CNT-DENP)
                   WHEN    "8"
                       MOVE    "診療訂正"  TO  WRK-JYOTAI(CNT-DENP)
                   END-EVALUATE
               END-IF
      *
               ADD     SMEI-SKYMONEY       TO  WRK-GSEIKYU(4)
      *
               ADD     SMEI-NYUHEN-MONEY   TO  WRK-GRYOSYU(4)
      *
               COMPUTE WRK-GMISYU(4)       =   WRK-GSEIKYU(4)
                                           -   WRK-GRYOSYU(4)
      *
               EVALUATE    WRK-PARA-SYORIKBN
               WHEN    "1"
               WHEN    "2"
      *            請求金額
                   ADD     SMEI-SKYMONEY   TO  WRK-SEIKYU(CNT-DENP)
      *            領収金額
                   ADD     SMEI-NYUHEN-MONEY
                                           TO  WRK-RYOSYU(CNT-DENP)
               WHEN    "0"
      *            請求金額
                   MOVE    SMEI-SKYMONEY   TO  WRK-SEIKYU(CNT-DENP)
      *            領収金額
                   MOVE    SMEI-NYUHEN-MONEY
                                           TO  WRK-RYOSYU(CNT-DENP)
               END-EVALUATE
      *
      *        未収金額
               MOVE    WRK-GMISYU(4)       TO  WRK-MISYU(CNT-DENP)
      *
               PERFORM 900-DBFETCH-SEC
      *
           END-PERFORM
           .
       32012-BODY-NAIYO-HEN-EXT.
           EXIT.
      *
      *----(00.00.03)--UPD-END----
      *****************************************************************
      *    帳票編集<明細>セット処理
      *****************************************************************
       3202-BODY-NAIYO-SET-SEC     SECTION.
      *
           MOVE    ZERO            TO  IDZ
      *
           PERFORM UNTIL   IDZ     =   CNT-DENP
               ADD     1                   TO  IDZ
               ADD     1                   TO  CNT-LINE
               MOVE    WRK-PTNUM(IDZ)      TO  HCM07-PTNUM(CNT-LINE)
               MOVE    WRK-NAME(IDZ)       TO  HCM07-NAME(CNT-LINE)
               MOVE    WRK-SEX(IDZ)        TO  HCM07-SEX(CNT-LINE)
               MOVE    WRK-BIRTHDAY(IDZ)   TO  HCM07-BIRTHDAY(CNT-LINE)
               MOVE    WRK-DENPNUM-X(IDZ)  TO  HCM07-DENPNUM(CNT-LINE)
               MOVE    WRK-SRYMD(IDZ)      TO  HCM07-SRYMD(CNT-LINE)
               MOVE    WRK-SEIKYU(IDZ)     TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM07-SEIKYU(CNT-LINE)
               MOVE    WRK-RYOSYU(IDZ)     TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM07-RYOSYU(CNT-LINE)
               MOVE    WRK-MISYU(IDZ)      TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM07-MISYU(CNT-LINE)
               MOVE    WRK-JYOTAI(IDZ)     TO  HCM07-JYOTAI(CNT-LINE)
      *
               COMPUTE WRK-GSEIKYU (1)
                   =   WRK-GSEIKYU (1)     +   WRK-SEIKYU(IDZ)
               COMPUTE WRK-GSEIKYU (2)
                   =   WRK-GSEIKYU (2)     +   WRK-SEIKYU(IDZ)
               COMPUTE WRK-GSEIKYU (3)
                   =   WRK-GSEIKYU (3)     +   WRK-SEIKYU(IDZ)
               COMPUTE WRK-GRYOSYU (1)
                   =   WRK-GRYOSYU (1)     +   WRK-RYOSYU(IDZ)
               COMPUTE WRK-GRYOSYU (2)
                   =   WRK-GRYOSYU (2)     +   WRK-RYOSYU(IDZ)
               COMPUTE WRK-GRYOSYU (3)
                   =   WRK-GRYOSYU (3)     +   WRK-RYOSYU(IDZ)
           END-PERFORM
      *
           COMPUTE WRK-GMISYU (1)
               =   WRK-GMISYU (1)      +   WRK-GMISYU(4)
           COMPUTE WRK-GMISYU (2)
               =   WRK-GMISYU (2)      +   WRK-GMISYU(4)
           COMPUTE WRK-GMISYU (3)
               =   WRK-GMISYU (3)      +   WRK-GMISYU(4)
      *
           IF    ( WRK-PARA-SORTKBN    =   "1" )
               MOVE    "00"        TO  KEY-N-SRYKA
           ELSE
               MOVE    SMEI-SRYKA  TO  KEY-N-SRYKA
           END-IF
           MOVE    SMEI-PTID       TO  KEY-N-PTID
           MOVE    SMEI-DENPNUM    TO  KEY-N-DENPNUM
      *
           MOVE    ZERO            TO  CNT-DENP
           INITIALIZE                  WRK-NAI-TBL
           INITIALIZE                  WRK-SUM-TBL(4)
      *
           IF      FLG-SMEI        =   1
               MOVE    1           TO  FLG-END
           END-IF
           .
       3202-BODY-NAIYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計>処理
      *****************************************************************
       330-TERM-HEN-SEC            SECTION.
      *
           MOVE    1               TO  IDX
           PERFORM 331-TOTAL-HENSYU-SEC
      *
           IF      KEY-N-SRYKA     NOT =   KEY-O-SRYKA
               MOVE    2               TO  IDX
               PERFORM 331-TOTAL-HENSYU-SEC
           END-IF
      *
           IF      FLG-END             =   1
               MOVE    3               TO  IDX
               PERFORM 331-TOTAL-HENSYU-SEC
           END-IF
           .
       330-TERM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       331-TOTAL-HENSYU-SEC        SECTION.
      *
           EVALUATE    IDX
               WHEN    1
                   MOVE    "頁内合計"      TO  HCM07-TTLMSG(1)
               WHEN    2
                   MOVE    "科合計"        TO  HCM07-TTLMSG(2)
               WHEN    3
                   MOVE    "総合計"        TO  HCM07-TTLMSG(3)
           END-EVALUATE
      *
           MOVE    WRK-GSEIKYU(IDX)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM07-SEIKYUTTL(IDX)
           MOVE    WRK-GRYOSYU(IDX)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM07-RYOSYUTTL(IDX)
           MOVE    WRK-GMISYU(IDX)     TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM07-MISYUTTL(IDX)
      *
      *    IF    ( IDX    =    3 )
      *        PERFORM 3311-TOTAL-MES-HEN-SEC
      *    END-IF
      *
           .
       331-TOTAL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    領収金額計編集処理
      *****************************************************************
       3311-TOTAL-MES-HEN-SEC      SECTION.
      *
           MOVE    WRK-GRYOSYU(IDX)    TO  WRK-Z9
           MOVE    SPACE               TO  WRK-STR
           MOVE    1                   TO  IDX1
           UNSTRING    WRK-Z9
                   DELIMITED   BY  ALL SPACE
                   INTO    WRK-STR
                   WITH    POINTER IDX1
           END-UNSTRING
           MOVE    WRK-Z9 (IDX1 : )    TO  WRK-MAE-INPUT
           PERFORM 800-ZENKAKU-HEN-SEC
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "領収金額合計　"    DELIMITED   BY  SIZE
                   WRK-OUT-INPUT       DELIMITED   BY  SPACE
                   "円"                DELIMITED   BY  SIZE
           INTO    JOB-YOBI
           END-STRING
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
      *
           .
       3311-TOTAL-MES-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
      *----(00.00.03)--UPD-START---
      *     MOVE   "HCM07.red"      TO  SPRT-PRTID
           MOVE   "HCM07V02.red"   TO  SPRT-PRTID
      *----(00.00.03)--UPD-END-----
           MOVE    "収納一覧表（期間指定）" 
                                   TO  SPRT-TITLE
           MOVE    HCM07           TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG     REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
057400*
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
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
grpsys         MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           INITIALIZE                          SMEI-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID         TO  SMEI-HOSPID
grpsys     MOVE    SPA-HOSPNUM             TO  SMEI-HOSPNUM
           MOVE    "2"                     TO  SMEI-NYUGAIKBN
           MOVE    WRK-STDAILYKEY          TO  SMEI-STDAILYKEY
           MOVE    WRK-EDDAILYKEY          TO  SMEI-EDDAILYKEY
           MOVE    SMEI-REC                TO  MCPDATA-REC
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    WRK-MAIN-PATHNAME       TO  MCP-PATHNAME
           MOVE    "DBSELECT"              TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SMEI
           END-IF
      *
           IF      FLG-SMEI            =   1
               MOVE    1               TO  FLG-END
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納を検索処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    WRK-MAIN-PATHNAME       TO  MCP-PATHNAME
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SMEI
               MOVE    MCPDATA-REC     TO  SMEI-REC
           ELSE
               MOVE    1               TO  FLG-SMEI
               INITIALIZE                  SMEI-REC
           END-IF
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBCLOSE-SEC             SECTION.
      *
           MOVE    "tbl_syumei"            TO  MCP-TABLE
           MOVE    WRK-MAIN-PATHNAME       TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター読込処理
      *****************************************************************
       900-SMEI2-SELECT-SEC        SECTION.
      *
           INITIALIZE                  SMEI2-REC
grpsys*DD  MOVE    WRK-PARA-HOSPID TO  SMEI2-HOSPID
grpsys     MOVE    SPA-HOSPNUM     TO  SMEI2-HOSPNUM
           MOVE    SMEI-PTID       TO  SMEI2-PTID
           MOVE    SMEI-DENPNUM    TO  SMEI2-DENPNUM
           MOVE    SMEI-DENPEDANUM TO  SMEI2-DENPEDANUM
           MOVE    SMEI-NYUGAIKBN  TO  SMEI2-NYUGAIKBN
           MOVE    SMEI2-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SMEI2-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-SMEI2
           END-IF
           .
       900-SMEI2-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細を検索処理
      *****************************************************************
       900-SMEI2-FETCH-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SMEI2
               MOVE    MCPDATA-REC     TO  SMEI2-REC
           ELSE
               MOVE    1               TO  FLG-SMEI2
           END-IF
           .
       900-SMEI2-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-SMEI2-CLOSE-SEC         SECTION.
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-SMEI2-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込処理
      *****************************************************************
       900-SYU-READ-SEC            SECTION.
      *
           MOVE    SYU-REC         TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYU-REC
               MOVE    ZERO            TO  FLG-SYU
           ELSE
               MOVE    1               TO  FLG-SYU
               INITIALIZE                  SYU-REC
           END-IF
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換マスター読込処理
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-KEY10-SEL-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       800-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"   USING   MCPAREA
grpsys*DD                              MCPDATA-REC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"   USING   MCPAREA
grpsys*DD                              MCPDATA-REC
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
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"   USING   MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"   USING   MCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC.
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　切断処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "MONFUNC"           USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
