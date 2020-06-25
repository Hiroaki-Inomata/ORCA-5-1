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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGXE01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : QRコードテストプロ
      *  コンポーネント名  : QRコードマスタ登録
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/01/21    NACL-竹田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
       01  SPA-XE01.
           03  SPA-XE01-AREA.
               05  SPA-GMN-CUR             PIC 9(03).
      *
               05  SPA-GMN-SIDO-KBN        PIC X(03).
               05  SPA-GMN-HKNNUM          PIC X(08).
               05  SPA-GMN-KIGONUM         PIC X(40).
               05  SPA-GMN-HONKZKKBN       PIC X(01).
               05  SPA-GMN-KANANAME        PIC X(80).
               05  SPA-GMN-SEX             PIC X(01).
               05  SPA-GMN-BIRTHDAY        PIC X(07).
               05  SPA-GMN-NIN             PIC X(100).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-QRCDHKN         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF         PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK-PTHKN        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDZ2                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-REGISTID         PIC 9(05).
      *
           03  WRK-KOUMOKU             PIC X(100).
           03  WRK-PTID                PIC 9(10).
      *
           03  WRK-KIGONUM-MAE         PIC X(100).
           03  WRK-KIGO                PIC X(80).
           03  WRK-NUM                 PIC X(80).
           03  WRK-KIGO-H              PIC X(80).
           03  WRK-NUM-H               PIC X(80).
      *
           03  WRK-NIN-TBL.
               05  WRK-NIN-DATA        PIC X(100)   OCCURS   5.
           03  WRK-MO-CNT              PIC 9(04).
      *
           03  WRK-MCP-PATHNAME        PIC X(60).
           03  WRK-MCP-WIDGET          PIC X(60).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    QRコード(被保険者証)
       01  QRCDHKN-REC.
           COPY    "CPQRCDHKN.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    検索文字追加
           COPY    "CPORCSADDSIGN.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
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
           COPY    "XE01.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
      *    MOVE    SPA-WORK        TO  SPA-N41KYOUTU
           MOVE    SPA-FREE        TO  SPA-XE01
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
      *    MOVE    SPA-XE01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA         TO  SPA-COMMON
           MOVE    SPA-XE01         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *??  エラー画面より
           IF      SPA-MOTOPG          =   "XEERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "XE01"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           IF      LNK-KYOUTU      NOT =  SPACE
               MOVE    LNK-KYOUTU      TO  SPA-AREA
           END-IF
      *
      *    内部画面より
      *    EVALUATE    SPA-MOTOPG
      *!       WHEN    "XE01"
                   PERFORM 3001-SPA-SET-SEC
      *!   END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3001-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                 SPA-XE01
           MOVE    1                  TO  SPA-GMN-CUR
      *    医療機関識別番号セット SPA-HOSPNUM
           CALL    "ORCSHOSPNUM"   USING
                                       MCPAREA
                                       SPA-AREA
      *
           DISPLAY "spa-set"  " SPA-HOSPNUM:" SPA-HOSPNUM
           .
       3001-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 412-TOROKU-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    XE01-SEIDO-KBN      TO  SPA-GMN-SIDO-KBN
           MOVE    XE01-HKNNUM         TO  SPA-GMN-HKNNUM
           MOVE    XE01-KIGONUM        TO  SPA-GMN-KIGONUM
           MOVE    XE01-HONKZKKBN      TO  SPA-GMN-HONKZKKBN
           MOVE    XE01-KANANAME       TO  SPA-GMN-KANANAME
           MOVE    XE01-SEX            TO  SPA-GMN-SEX
           MOVE    XE01-BIRTHDAY       TO  SPA-GMN-BIRTHDAY
           MOVE    XE01-NIN            TO  SPA-GMN-NIN
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *    必須入力チェック
           IF      SPA-GMN-SIDO-KBN    =   SPACE
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-HKNNUM      =   SPACE  )
               MOVE    2               TO  SPA-GMN-CUR
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-KIGONUM      =   SPACE  )
               MOVE    3               TO  SPA-GMN-CUR
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-HONKZKKBN   =   SPACE  )
               MOVE    4               TO  SPA-GMN-CUR
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-SEX      =   SPACE  )
               MOVE    6               TO  SPA-GMN-CUR
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-BIRTHDAY      =   SPACE  )
               MOVE    7               TO  SPA-GMN-CUR
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "M02"               TO  SPA-SAKIPG
           MOVE    "XE01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       412-TOROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    412-TOROKU-EXT
           END-IF
      *
      *    データ編集処理
           PERFORM 420-DATA-HENSYU-SEC
      *    追加処理
           IF      SPA-ERRCD           =   SPACE
               MOVE    SMCNDATE-YMD        TO  QRCDHKN-CREYMD
               MOVE    SMCNDATE-YMD        TO  QRCDHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  QRCDHKN-UPHMS
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1000"          TO  SPA-ERRCD
                   DISPLAY "XE01 QRCDHKN INS ERR:" MCP-RC
               ELSE
                   INITIALIZE                  SPA-XE01
               END-IF
           END-IF
      *
           .
       412-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ更新処理
      *****************************************************************
       420-DATA-HENSYU-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    システム日付
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
                   MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    最大連番
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  QRCDHKN-REGISTYMD
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    受付最大件数
           MOVE    QRCDHKN-REGISTID   TO  WRK-REGISTID
      *
           MOVE    SPACE               TO  QRCDHKN-REC
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  QRCDHKN-REGISTYMD
      *    受付ＩＤ
           IF      WRK-REGISTID       =   99999
      *        99999 以上は登録不可
               MOVE    "9999"              TO  SPA-ERRCD
               GO      TO   420-DATA-HENSYU-EXT
           END-IF
           ADD     1                   TO  WRK-REGISTID
           MOVE    WRK-REGISTID        TO  QRCDHKN-REGISTID
      *受付時間
           MOVE    SMCNDATE-HMS        TO  QRCDHKN-REGISTTIME
      *制度区分(必須)
           PERFORM 4201-SEIDOKBN-HEN-SEC
      *保険者番号(必須)
           PERFORM 4201-HKNNUM-HEN-SEC
      *記号番号(必須)
           PERFORM 4201-KIGONUM-HEN-SEC
      *本人家族区分(必須)
           PERFORM 4201-HONKZKKBN-HEN-SEC
      *カナ氏名(任意)
           PERFORM 4201-KANANAME-HEN-SEC
      *性別(必須)
           PERFORM 4201-SEX-HEN-SEC
      *生年月日(必須)
           PERFORM 4201-BIRTHDAY-HEN-SEC
      *
      *その他項目
           PERFORM 4201-NIN-HEN-SEC
      *患者ＩＤ決定
           IF      QRCDHKN-ERRCD       =   SPACE
               PERFORM 4201-PTID-CHK-SEC
           END-IF
      *
           .
       420-DATA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    JISコードをEUCコードへ変換処理
      *****************************************************************
       500-JISHEN-SEC          SECTION.
      *
           .
       500-JISHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    制度区分(必須)編集処理
      *****************************************************************
       4201-SEIDOKBN-HEN-SEC          SECTION.
      *
           MOVE    SPA-GMN-SIDO-KBN    TO  WRK-KOUMOKU
           PERFORM 500-JISHEN-SEC
           MOVE    WRK-KOUMOKU         TO  SPA-GMN-SIDO-KBN
      *    チェックなし
           MOVE    SPA-GMN-SIDO-KBN    TO  QRCDHKN-SEIDOKBN
           .
      *
       4201-SEIDOKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号(必須)編集処理
      *****************************************************************
       4201-HKNNUM-HEN-SEC          SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-HKNNUM      TO  KANACHK-MAE-INPUT
           MOVE    8                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
               EVALUATE    KANACHK-MAX
                   WHEN    8
                       IF      SPA-GMN-HKNNUM(1:4) =   "0100"
      *                    政管
                           MOVE    SPA-GMN-HKNNUM(5:4)
                                                   TO  QRCDHKN-HKNJANUM
                       ELSE
                           IF      SPA-GMN-HKNNUM(1:2) =   "00"
      *                        国保
                               MOVE    SPA-GMN-HKNNUM(3:)
                                                   TO  QRCDHKN-HKNJANUM
                           ELSE
                               MOVE    SPA-GMN-HKNNUM
                                                   TO  QRCDHKN-HKNJANUM
                           END-IF
                       END-IF
                   WHEN    OTHER
                       MOVE    SPA-GMN-HKNNUM    TO  QRCDHKN-HKNJANUM
                       MOVE    "02"              TO  QRCDHKN-ERRCD
               END-EVALUATE
           ELSE
               MOVE    SPA-GMN-HKNNUM    TO  QRCDHKN-HKNJANUM
               MOVE    "02"              TO  QRCDHKN-ERRCD
           END-IF
           .
      *
       4201-HKNNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号番号(必須)編集処理
      *****************************************************************
       4201-KIGONUM-HEN-SEC          SECTION.
      *
           MOVE    SPA-GMN-KIGONUM     TO  WRK-KOUMOKU
           PERFORM 500-JISHEN-SEC
           MOVE    WRK-KOUMOKU         TO  SPA-GMN-KIGONUM
      *
           MOVE    ZERO                TO  FLG-ERR
           MOVE    SPACE               TO  WRK-KIGO
           MOVE    SPACE               TO  WRK-NUM
           MOVE    SPACE               TO  WRK-KIGO-H
           MOVE    SPACE               TO  WRK-NUM-H
      *    半角カナを全角へ
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-KIGONUM     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-KIGONUM-MAE
      *    全角空白までを記号とする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   KANACHK-MAX
               IF      WRK-KIGONUM-MAE(IDX:2)  =   "　"
                   COMPUTE IDX2        =   IDX     +   2
                   MOVE    WRK-KIGONUM-MAE(IDX2:)  TO  WRK-NUM
                   MOVE    KANACHK-MAX     TO  IDX
               ELSE
                   MOVE    WRK-KIGONUM-MAE(IDX:1)  TO  WRK-KIGO(IDX:1)
               END-IF
           END-PERFORM
           IF      WRK-KIGO            =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-KIGO            TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-KIGO
               IF     (KANACHK-RC          =   ZERO  )
                   MOVE    KANACHK-OUT-INPUT   TO  WRK-KIGO
               ELSE
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
      *
      *    番号 全角変換
           IF      WRK-NUM             =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-NUM             TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-NUM
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT   TO  WRK-NUM
               ELSE
                   MOVE    1                   TO  FLG-ERR
               END-IF
           END-IF
      *    データ編集
           MOVE    WRK-KIGONUM-MAE   TO  QRCDHKN-KIGONUM
           MOVE    WRK-KIGO          TO  QRCDHKN-KIGO
           MOVE    WRK-NUM           TO  QRCDHKN-NUM
           IF      FLG-ERR           =   1
               MOVE    "03"              TO  QRCDHKN-ERRCD
           END-IF
           .
       4201-KIGONUM-HEN-EXIT.
           EXIT.
      *
      *****************************************************************
      *    本人家族区分(必須)編集処理
      *****************************************************************
       4201-HONKZKKBN-HEN-SEC          SECTION.
      *
           MOVE    SPA-GMN-HONKZKKBN   TO  QRCDHKN-HONKZKKBN
           IF      SPA-GMN-HONKZKKBN   =   "1"  OR  "2"
               CONTINUE
           ELSE
               IF      QRCDHKN-ERRCD   =   SPACE
                   MOVE    "04"            TO  QRCDHKN-ERRCD
               END-IF
           END-IF
           .
       4201-HONKZKKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ氏名(必須)編集処理
      *****************************************************************
       4201-KANANAME-HEN-SEC          SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  WRK-KOUMOKU
           PERFORM 500-JISHEN-SEC
           MOVE    WRK-KOUMOKU         TO  SPA-GMN-KANANAME
      *
           IF      SPA-GMN-KANANAME    =   SPACE
               MOVE    SPACE               TO  QRCDHKN-KANANAME
           ELSE
      *        全角変換
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-KANANAME    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  QRCDHKN-KANANAME
               IF     (KANACHK-RC      NOT =   ZERO  )  AND
                      (QRCDHKN-ERRCD       =   SPACE )
                   MOVE    "05"              TO  QRCDHKN-ERRCD
               END-IF
           END-IF
           .
       4201-KANANAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    性別(必須)編集処理
      *****************************************************************
       4201-SEX-HEN-SEC          SECTION.
      *
           MOVE    SPA-GMN-SEX         TO  QRCDHKN-SEX
           IF      SPA-GMN-SEX         =   "1"  OR  "2"
               CONTINUE
           ELSE
               IF      QRCDHKN-ERRCD   =   SPACE
                   MOVE    "06"            TO  QRCDHKN-ERRCD
               END-IF
           END-IF
           .
       4201-SEX-HEN-EXT.
           EXIT.
      *****************************************************************
      *    生年月日(必須)編集処理
      *****************************************************************
       4201-BIRTHDAY-HEN-SEC          SECTION.
      *
      *    日付チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-BIRTHDAY    TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           MOVE    SPA-GMN-BIRTHDAY    TO  QRCDHKN-BIRTHDAY
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-SDAY          TO  QRCDHKN-BIRTHDAYS
           ELSE
               IF      QRCDHKN-ERRCD   =   SPACE
                   MOVE    "07"            TO  QRCDHKN-ERRCD
               END-IF
           END-IF
           .
       4201-BIRTHDAY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    その他項目編集処理
      *****************************************************************
       4201-NIN-HEN-SEC          SECTION.
      *
      *    １桁目の','は削除
           IF      SPA-GMN-NIN(1:1)    =   ","
               MOVE    SPA-GMN-NIN(2:)     TO  WRK-KOUMOKU
           ELSE
               MOVE    SPA-GMN-NIN         TO  WRK-KOUMOKU
           END-IF
      *
           INITIALIZE                      WRK-NIN-TBL
           MOVE    1                   TO  WRK-MO-CNT
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   5
               UNSTRING                WRK-KOUMOKU
                   DELIMITED  BY  ","
                   INTO                WRK-NIN-DATA(IDX)
                   WITH POINTER        WRK-MO-CNT
               END-UNSTRING
           END-PERFORM
      *
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL   IDX         >   5
               IF      WRK-NIN-DATA(IDX)   NOT =   SPACE
      *TEST
      *      DISPLAY "IDX:" IDX 
      *      DISPLAY "WRK-NIN-DATA:" WRK-NIN-DATA(IDX)
      *TEST
                   EVALUATE    IDX
      *            保険者独自の管理番号(任意)
                   WHEN    1
                       PERFORM 42011-KANRINUM-HEN-SEC
      *            漢字氏名(任意)
                   WHEN    2
                       PERFORM 42011-NAME-HEN-SEC
      *            給付割合(任意)
                   WHEN    3
                       PERFORM 42011-KYUFU-HEN-SEC
      *            再発行コード(任意)
                   WHEN    4
                       PERFORM 42011-SAIHAKKU-HEN-SEC
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
      *
       4201-NIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険者独自の管理番号(任意)処理
      *****************************************************************
       42011-KANRINUM-HEN-SEC         SECTION.
      *
           MOVE    WRK-NIN-DATA (IDX)  TO  WRK-KOUMOKU
           PERFORM 500-JISHEN-SEC
           MOVE    WRK-KOUMOKU         TO  WRK-NIN-DATA (IDX)
      *    半角カナチェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-NIN-DATA (IDX)  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  QRCDHKN-KANRINUM
           .
       42011-KANRINUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    漢字氏名(任意)処理
      *****************************************************************
       42011-NAME-HEN-SEC         SECTION.
      *
           MOVE    WRK-NIN-DATA (IDX)  TO  WRK-KOUMOKU
           PERFORM 500-JISHEN-SEC
           MOVE    WRK-KOUMOKU         TO  WRK-NIN-DATA (IDX)
      *    全角変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-NIN-DATA (IDX)  TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  QRCDHKN-NAME
           IF     (KANACHK-RC      NOT =   ZERO  )  AND
                  (QRCDHKN-ERRCD       =   SPACE )
               MOVE    "09"              TO  QRCDHKN-ERRCD
           END-IF
           .
       42011-NAME-HEN-EXT.
           EXIT.
      *****************************************************************
      *    給付割合(任意)処理
      *****************************************************************
       42011-KYUFU-HEN-SEC         SECTION.
      *
           MOVE    WRK-NIN-DATA (IDX)(1:2) TO  QRCDHKN-KYURATE
      *
           .
       42011-KYUFU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    再発行コード(任意)処理
      *****************************************************************
       42011-SAIHAKKU-HEN-SEC         SECTION.
      *
           MOVE    WRK-NIN-DATA (IDX) TO  QRCDHKN-SAIHAKKU
           .
       42011-SAIHAKKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者ＩＤ決定処理
      *****************************************************************
       4201-PTID-CHK-SEC         SECTION.
      *
      *    カナ氏名、性別、生年月日から患者情報の検索
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    QRCDHKN-BIRTHDAYS   TO  PTINF-BIRTHDAY
           MOVE    QRCDHKN-SEX         TO  PTINF-SEX
           IF      QRCDHKN-KANANAME    =   SPACE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           ELSE
      *        LIKE検索
               INITIALIZE                  ORCSADDSIGNAREA
               MOVE    QRCDHKN-KANANAME    TO  SADDSIGN-STR
               CALL    "ORCSADDSIGN"       USING
                                           ORCSADDSIGNAREA
               MOVE    SADDSIGN-STR        TO  PTINF-KANANAME
      *
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  IDZ2
           MOVE    ZERO                TO  WRK-PTID
           PERFORM   UNTIL  ( FLG-PTINF    =   1  )
                     OR     ( FLG-OK       =   1  )
      *        性別・生年月日一致
               IF     (QRCDHKN-NAME        =   SPACE )  AND
                      (QRCDHKN-KANANAME    =   SPACE )
                   MOVE    PTINF-PTID          TO  WRK-PTID
                   ADD     1                   TO  IDZ
                   PERFORM 42011-PTHKNINF-CHK-SEC
               END-IF
      *        カナ氏名・性別・生年月日一致
               IF     (QRCDHKN-NAME        =   SPACE )  AND
                      (QRCDHKN-KANANAME    NOT =   SPACE )
                   IF     (PTINF-KANANAME  =   QRCDHKN-KANANAME)
                     AND  (PTINF-SEX       =   QRCDHKN-SEX      )
                     AND  (PTINF-BIRTHDAY  =   QRCDHKN-BIRTHDAYS)
                       MOVE    PTINF-PTID          TO  WRK-PTID
                       ADD     1                   TO  IDZ
                       PERFORM 42011-PTHKNINF-CHK-SEC
                   END-IF
               END-IF
      *        氏名・性別・生年月日一致
               IF     (QRCDHKN-NAME        NOT =   SPACE )  AND
                      (QRCDHKN-KANANAME    NOT =   SPACE )
                   IF     (PTINF-KANANAME  =   QRCDHKN-KANANAME)
                     AND  (PTINF-NAME      =   QRCDHKN-NAME    )
                     AND  (PTINF-SEX       =   QRCDHKN-SEX     )
                     AND  (PTINF-BIRTHDAY  =   QRCDHKN-BIRTHDAYS)
                       MOVE    PTINF-PTID          TO  WRK-PTID
                       ADD     1                   TO  IDZ
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-IF
      *        漢字氏名・性別・生年月日一致
               IF     (QRCDHKN-NAME        NOT =   SPACE )  AND
                      (QRCDHKN-KANANAME        =   SPACE )
                   IF     (PTINF-NAME      =   QRCDHKN-NAME    )
                     AND  (PTINF-SEX       =   QRCDHKN-SEX     )
                     AND  (PTINF-BIRTHDAY  =   QRCDHKN-BIRTHDAYS)
                       MOVE    PTINF-PTID          TO  WRK-PTID
                       ADD     1                   TO  IDZ
                       PERFORM 42011-PTHKNINF-CHK-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-OK              =   1  )
      *        患者ＩＤ決定（保険情報一致、漢字氏名から生年月日一致）
               MOVE    WRK-PTID            TO  QRCDHKN-PTID
           ELSE
      *        対象が１件で、氏名が一致
               IF     (IDZ                 =   1  )    AND
                     ((QRCDHKN-NAME        NOT =   SPACE )  OR
                      (QRCDHKN-KANANAME    NOT =   SPACE ))
      *            患者ＩＤ決定
                   MOVE    WRK-PTID            TO  QRCDHKN-PTID
               END-IF
           END-IF
      *
           .
       4201-PTID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者情報から決定処理
      *****************************************************************
       42011-PTHKNINF-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  PTHKNINF-REC
           INITIALIZE                      PTHKNINF-REC
           MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
           MOVE    PTINF-PTID          TO  PTHKN-PTID
           MOVE    QRCDHKN-HKNJANUM    TO  PTHKN-HKNJANUM
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-SEC
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  FLG-OK-PTHKN
           PERFORM   UNTIL  ( FLG-PTHKNINF =   1  )
                     OR     ( FLG-OK       =   1  )
               IF     (PTHKN-HONKZKKBN     =   QRCDHKN-HONKZKKBN)
                  AND (PTHKN-KIGO          =   QRCDHKN-KIGO     )
                  AND (PTHKN-NUM           =   QRCDHKN-NUM     )
                   MOVE    1               TO  FLG-OK-PTHKN
               END-IF
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    同じ記号番号
           IF      FLG-OK-PTHKN        =   1
             IF     (QRCDHKN-NAME        =   SPACE )  AND
                    (QRCDHKN-KANANAME    =   SPACE )
               MOVE    SPACE               TO  PTHKNINF-REC
               INITIALIZE                      PTHKNINF-REC
               MOVE    PTINF-HOSPNUM       TO  PTHKN-HOSPNUM
               MOVE    PTINF-PTID          TO  PTHKN-PTID
               MOVE    QRCDHKN-HKNJANUM    TO  PTHKN-HKNJANUM
               MOVE    QRCDHKN-HONKZKKBN   TO  PTHKN-HONKZKKBN
               MOVE    QRCDHKN-KIGO        TO  PTHKN-KIGO
               MOVE    QRCDHKN-NUM         TO  PTHKN-NUM
      *
               MOVE    PTHKNINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-PTHKNINF-READ-SEC
               ELSE
                   INITIALIZE                  PTHKNINF-REC
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
               MOVE    ZERO                TO  FLG-CHK
               PERFORM   UNTIL  ( FLG-PTHKNINF =   1  )
                         OR     ( FLG-CHK      =   1  )
                   IF     (PTHKN-PTID      NOT =   PTINF-PTID )
                      MOVE    1                TO  FLG-CHK
                   END-IF
                   MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-PTHKNINF-READ-SEC
               END-PERFORM
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               IF      FLG-CHK             =   ZERO
                   MOVE    1               TO  FLG-OK
               END-IF
             ELSE
                 MOVE    1               TO  FLG-OK
             END-IF
           END-IF
           .
       42011-PTHKNINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        PERFORM 510-ERRSET-SEC
      *        GO      TO      500-SET-SCREEN-EXT
      *    END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "XE01"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  XE01
      *
           INITIALIZE                      XE01
           MOVE    SPA-GMN-SIDO-KBN    TO  XE01-SEIDO-KBN
           MOVE    SPA-GMN-HKNNUM      TO  XE01-HKNNUM
           MOVE    SPA-GMN-KIGONUM     TO  XE01-KIGONUM
           MOVE    SPA-GMN-HONKZKKBN   TO  XE01-HONKZKKBN
           MOVE    SPA-GMN-KANANAME    TO  XE01-KANANAME
           MOVE    SPA-GMN-SEX         TO  XE01-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  XE01-BIRTHDAY
           MOVE    SPA-GMN-NIN    TO  XE01-NIN
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR     =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SEIDO_KBN"          TO  MCP-WIDGET
               WHEN    02
                   MOVE    "HKNNUM"          TO  MCP-WIDGET
               WHEN    03
                   MOVE    "KIGONUM"           TO  MCP-WIDGET
               WHEN    04
                   MOVE    "HONKZKKBN"            TO  MCP-WIDGET
               WHEN    05
                   MOVE    "KANANAME"        TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SEX"           TO  MCP-WIDGET
               WHEN    07
                   MOVE    "BIRTHDAY"             TO  MCP-WIDGET
               WHEN    08
                   MOVE    "NIN"          TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"          TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SEIDO_KBN"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "HKNNUM"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "KIGONUM"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "HONKZKKBN"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "KANANAME"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "SEX"
                   MOVE    7              TO  SPA-GMN-CUR
               WHEN    "BIRTHDAY"
                   MOVE    8              TO  SPA-GMN-CUR
               WHEN    "NIN"
                   MOVE    99             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
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
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
      **   MOVE    SPACE               TO  XDERR
      *    MOVE    SPA-ERRCD           TO  XDERR-ERRCODE
      *    MOVE    SPA-ERRMSG          TO  XDERR-ERRMSG
      *    MOVE    "B01"               TO  MCP-WIDGET
      *
      *    MOVE    "XE01"               TO  SPA-MOTOPG
      *    MOVE    "XEERR"            TO  SPA-SAKIPG
      *
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *    MOVE    "XEERR"            TO  MCP-WINDOW
      *
      *    PERFORM 900-PUT-WINDOW
      *
      *    MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
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
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
