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
       PROGRAM-ID.             ORCHCN31.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬情一覧印刷
      *  コンポーネント名  : 薬情一覧印刷（ＨＣＮ３１）６件表示用
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/1O/30   NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      * 04.05.00     NACL-森脇     09/06/22  小数点以下５桁に変更
      * 04.05.01     NACL-森脇     09/11/20  在宅・コメント対応
      * 04.05.02     NACL-森脇     10/01/26  コメント２、単位対応
      * 04.06.00     NACL-多々納   11/02/28  用法コメント対応
      * 04.08.00     NACL-多々納   14/07/30  一時ディレクトリ対応
      *  04.08.01    NACL-森脇     14/08/11  画像イメージ作成機能追加
      *  04.08.03    NACL-森脇     16/11/22  服用方法コード対応
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      **** SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    帳票領域
           COPY    "HCN301.INC".
           02 HCN301-LAYER-OPTION  PIC X(193).
      *
       01  FILE-NAME               PIC X(512).
      *
      *    空白の画像
       01  KUHAKU-NAME.
           03  FILLER              PIC X(20)   VALUE
                                               "data/transparent.jpg".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-YAKUJYO         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TNSP            PIC 9(01).
     *
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
     *
           03  FLG-KBN             PIC 9(01).
     *
           03  FLG-NAIYO           PIC 9(01).
           03  FLG-KEIJYO          PIC 9(01).
           03  FLG-KONO            PIC 9(01).
           03  FLG-CYUI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(01).
           03  CNT-PARA            PIC 9(04).
           03  CNT-KAISU           PIC 9(01).
           03  CNT-KENSU           PIC 9(04).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX-STR             PIC 9(04).
           03  IDX-STR1            PIC 9(04).
           03  IDX-KET             PIC 9(04).
      *
           03  IDX-GYO             PIC 9(04).
           03  IDX-GYO1            PIC 9(04).
      *
           03  IDX-KBN             PIC 9(01).
           03  IDX-LAYER           PIC 9(03).
           03  IDX-KBN2            PIC 9(04).
      *
           03  IDX-NAIYO           PIC 9(04).
           03  IDX-KEIJYO          PIC 9(04).
           03  IDX-KONO            PIC 9(04).
           03  IDX-CYUI            PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HENLAYER        PIC X(168).
      *
           03  WRK-SSTKIJUNCD      PIC X(05).
           03  WRK-HENKBN-G.
               05  WRK-HENKBN      PIC 9(08)V9(05)
                                               OCCURS  5.
      *
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
      *
           03  WRK-YKZNAME         PIC X(100).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-TANINAME        PIC X(24).
           03  WRK-WARI            PIC 9(08).
           03  WRK-KAISU           PIC 9(01).
           03  WRK-ZENSUMAE        PIC X(40).
           03  WRK-ZENSUATO        PIC X(40).
           03  WRK-ZENSURYO        PIC 9(08)V9(05).
      *
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-HENSURYO        PIC X(14).
           03  WRK-SURYO           PIC 9(08)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(08).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z9              PIC Z9.
      *
      *    全角を行毎に編集
       01  WRK-HENSYU-AREA.
           03  WRK-HENSYU-OCC                  OCCURS   66.
               05  WRK-HENSYU      PIC X(40).
           03  WRK-HENSYUDATA      PIC X(4000).
           03  GYO-MAX             PIC 9(04).
           03  KOU-MAX             PIC 9(04).
      *
       01  WRK-YKJ-AREA.
           03  WRK-YKJ-HEAD.
               05  WRK-YKJ-SYORIBI     PIC X(22).
               05  WRK-YKJ-SYURUI      PIC X(12).
               05  WRK-YKJ-PTNUM       PIC X(20).
               05  WRK-YKJ-KANANAME    PIC X(44).
               05  WRK-YKJ-NAME        PIC X(88).
               05  WRK-YKJ-JYUSYO      PIC X(100).
               05  WRK-YKJ-HSPNAME     PIC X(100).
               05  WRK-YKJ-TEL         PIC X(15).
               05  WRK-YKJ-DRNAME      PIC X(26).
               05  WRK-YKJ-COMMENT     PIC X(108)  OCCURS  2.
               05  WRK-YKJ-BTUNAME     PIC X(30).
      *
           03  WRK-YKJ-BODY.
               05  WRK-YKJ-NUM             PIC X(02).
               05  WRK-YKJ-GAZO            PIC X(1024).
               05  WRK-YKJ-YKZNAME-G.
                   07  WRK-YKJ-YKZNAME     PIC X(26)   OCCURS  3.
               05  WRK-YKJ-KONOKOKA-G.
                   07  WRK-YKJ-KONOKOKA    PIC X(30)   OCCURS  66.
               05  WRK-YKJ-KEIJYO-G.
                   07  WRK-YKJ-KEIJYO      PIC X(16)   OCCURS  66.
               05  WRK-YKJ-KBN-G.
                   07  WRK-YKJ-KBN         PIC X(03)   OCCURS  5.
               05  WRK-YKJ-NAIYO-G.
                   07  WRK-YKJ-NAIYO       PIC X(18)   OCCURS  66.
               05  WRK-YKJ-CYUJKO-G.
                   07  WRK-YKJ-CYUJKO      PIC X(40)   OCCURS  66.
               05  WRK-YKJ-TANI-G.
                   07  WRK-YKJ-TANI        PIC X(02)   OCCURS  5.
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-STYLE              PIC X(01).
           03  PARA-TIMESURYO          PIC X(01).
           03  PARA-ZAITAKU            PIC X(01).
           03  PARA-COMMENT            PIC X(01).
           03  PARA-COMMENT2           PIC X(01).
           03  PARA-TANI               PIC X(01).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *----(04.08.01)--ADD-START---
       01  FILE-IMAGE              PIC X(512).
      *
       01  WRK-CONS-AREA.
      *    画像ファイルフォルダ名
           03  WRK-CONS-MEDIMGDIR  PIC X(08)   VALUE   "medimage".
      *
      *    画像イメージ作成パラメタ
           COPY    "CPORCSMIAREA.INC".
      *----(04.08.01)--ADD-END-----
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    薬情マスタ
           COPY    "CPYAKUJYO.INC".
      *
      *    点数付加情報マスタ
       01  TNSP-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
           COPY    "CPORCHCN30.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPONPRTDATA.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ONPRTDATA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *
           MOVE    ONPRTDATA-DATA  TO  ORCHCN30AREA
      *
      *    出力先プリンタ名割り当て情報
           PERFORM 900-1031-READ-SEC
      *
      *    医療機関情報−基本情報
           PERFORM 900-1001-READ-SEC
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *
      *    医療機関情報−所在地、連絡先
           PERFORM 900-1002-READ-SEC
           MOVE    SYS-1002-ADRS   TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL    TO  WRK-HOSPTEL
      *
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    職員情報
           PERFORM 900-1010-READ-SEC
      *
      *    ファイルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    KUHAKU-NAME     TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  FILE-NAME
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *----(04.08.01)--ADD-END-----
      *
      *    プログラムオプション設定
           INITIALIZE                  PARA-AREA
      *    薬剤形状
           MOVE    "STYLE"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-STYLE
      *    服用時点
           MOVE    "TIMESURYO"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-TIMESURYO
      *    在宅区分
           MOVE    "ZAITAKU"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-ZAITAKU
      *    コメント
           MOVE    "COMMENT"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-COMMENT
      *    コメント２
           MOVE    "COMMENT2"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-COMMENT2
      *    単位
           MOVE    "TANI"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-TANI
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC        SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHCN31"      TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME   TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING
                                       SPA-AREA
                                       ORCSPRGOPTIONAREA
           MOVE    SPACE           TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
           END-IF
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    在宅チェック
           MOVE    ZERO            TO  CNT-KENSU
           IF      PARA-ZAITAKU    =   1
               MOVE    ORCHCN30-MAX    TO  CNT-KENSU
           ELSE
               PERFORM VARYING CNT-PARA    FROM 1 BY 1
                       UNTIL   CNT-PARA    >   ORCHCN30-MAX
                   IF      ORCHCN30-ZAIKBN(CNT-PARA) NOT =   4
                       ADD     1       TO  CNT-KENSU
                   END-IF
               END-PERFORM
           END-IF
           IF      CNT-KENSU       =   ZERO
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    帳票ＨＥＡＤ情報編集処理
           PERFORM 210-HEAD-HENSYU-SEC
      *
           MOVE    ZERO            TO  CNT-KENSU
      *    データがなくなるまで
           PERFORM VARYING CNT-PARA    FROM 1 BY 1
                   UNTIL   CNT-PARA    >   ORCHCN30-MAX
               IF      PARA-ZAITAKU    =   1
      *            帳票ＢＯＤＹ情報編集処理
                   PERFORM 220-BODY-HENSYU-SEC
               ELSE
                   IF      ORCHCN30-ZAIKBN(CNT-PARA)   NOT =   4
      *                帳票ＢＯＤＹ情報編集処理
                       PERFORM 220-BODY-HENSYU-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
      *    帳票印刷処理
           PERFORM 400-PRT-WRITE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤ情報編集処理
      *****************************************************************
       210-HEAD-HENSYU-SEC         SECTION.
      *
           INITIALIZE                  WRK-YKJ-AREA
      *
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  CNT-LINE
      *
      *    日付セット
           MOVE    SPA-SRYYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  WRK-YKJ-SYORIBI
      *
      *    種類セット
           MOVE    SPACE           TO  WRK-YKJ-SYURUI
           MOVE    "（"            TO  WRK-YKJ-SYURUI(1:2)
           IF      CNT-KENSU(3:1)    =   ZERO
               MOVE    CNT-KENSU(4:1)    TO  WRK-YKJ-SYURUI(3:1)
               MOVE    "種類）"          TO  WRK-YKJ-SYURUI(4:6)
           ELSE
               MOVE    CNT-KENSU(3:2)    TO  WRK-YKJ-SYURUI(3:2)
               MOVE    "種類）"          TO  WRK-YKJ-SYURUI(5:6)
           END-IF
      *
      *    患者番号
           MOVE    SPA-PTNUM       TO  WRK-YKJ-PTNUM
      *
      *    患者カナ氏名
           MOVE    SPA-KANANAME    TO  WRK-YKJ-KANANAME
      *
      *    患者氏名
           PERFORM 230-NAME-SET-SEC
      *    所在地
           MOVE    WRK-HOSPADRS    TO  WRK-YKJ-JYUSYO
      *    医療機関名称
           MOVE    WRK-HOSPNAME    TO  WRK-YKJ-HSPNAME
      *    電話番号
           MOVE    WRK-HOSPTEL     TO  WRK-YKJ-TEL
      *    職員氏名
           MOVE    SYS-1010-NAME   TO  WRK-YKJ-DRNAME
      *    病棟名
           STRING  ORCHCN30-BTUNAME    DELIMITED  BY  SPACE
                   ORCHCN30-ROOMNO     DELIMITED  BY  SIZE
                   INTO                WRK-YKJ-BTUNAME
           END-STRING
      *
      *    帳票ＨＥＡＤ編集処理
           PERFORM 410-HEAD-SET-SEC
      *
           .
       210-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ情報編集処理
      *****************************************************************
       220-BODY-HENSYU-SEC         SECTION.
      *
           INITIALIZE                  WRK-YKJ-BODY
      *
           ADD     1               TO  CNT-KENSU
      *
      *    番号セット
           MOVE    CNT-KENSU       TO  WRK-Z9
           MOVE    WRK-Z9          TO  WRK-YKJ-NUM
      *
      *    空白の画像
           MOVE    FILE-NAME       TO  WRK-YKJ-GAZO
      *
      *    薬情マスタ検索
           PERFORM 900-YAKUJYO-READ-SEC
      *
           IF      FLG-YAKUJYO     =   ZERO
      *        効能効果セット
               PERFORM 230-EFFECT-SET-SEC
      *        注意事項
               PERFORM 230-CAUTION-SET-SEC
      *        形状編集
               PERFORM 230-KEIJYO-SET-SEC
           END-IF
      *
      *    内容編集、区分編集
           PERFORM 240-NAIYO-SET-SEC
      *
      *    薬剤名セット
           PERFORM 230-YKZNAME-SET-SEC
      *
      *    帳票ＢＯＤＹセット処理
           PERFORM 420-BODY-SET-SEC
      *
           .
       220-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   患者氏名編集処理
      *****************************************************************
       230-NAME-SET-SEC            SECTION.
      *
           MOVE    1               TO  IDX-STR
           MOVE    ZERO            TO  IDX-KET
      *
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL  (IDX-STR     >   50)
                   OR     (SPA-NAME(IDX-STR:)  =   SPACE)
               COMPUTE IDX-KET     =   IDX-KET     +   1
           END-PERFORM
      *
           EVALUATE    IDX-KET
               WHEN    0
                   CONTINUE
               WHEN    1   THRU    30
                   MOVE    SPA-NAME(1:IDX-KET)
                                           TO  WRK-YKJ-NAME(1:IDX-KET)
                   MOVE    "　様"          TO  WRK-YKJ-NAME(IDX-STR:)
               WHEN    31  THRU    40
                   COMPUTE IDX-STR     =   IDX-STR     +   44
                   MOVE    SPA-NAME(1:IDX-KET)
                                           TO  WRK-YKJ-NAME(45:IDX-KET)
                   MOVE    "　様"          TO  WRK-YKJ-NAME(IDX-STR:)
               WHEN    41  THRU    50
                   MOVE    SPA-NAME        TO  WRK-YKJ-NAME
                   INSPECT WRK-YKJ-NAME    REPLACING  ALL "  "  BY  "　"
                   MOVE    "　様"          TO  WRK-YKJ-NAME(85:)
           END-EVALUATE
      *
           .
       230-NAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票薬剤名データ編集処理
      *****************************************************************
       230-YKZNAME-SET-SEC         SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    1               TO  IDX-GYO
           MOVE    3               TO  GYO-MAX
           MOVE    26              TO  KOU-MAX
           IF      YAKUJYO-NAME    =   SPACE
               MOVE    WRK-YKZNAME     TO  WRK-HENSYUDATA
           ELSE
               MOVE    YAKUJYO-NAME    TO  WRK-HENSYUDATA
           END-IF
           PERFORM 250-HENSYU-SYORI-SEC
      *
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX-GYO)
                                       TO  WRK-YKJ-YKZNAME(IDX-GYO)
           END-PERFORM
      *
           .
       230-YKZNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票効能効果データ編集処理
      *****************************************************************
       230-EFFECT-SET-SEC          SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    1               TO  IDX-GYO
           MOVE    62              TO  GYO-MAX
           MOVE    30              TO  KOU-MAX
           MOVE    YAKUJYO-EFFECT  TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           MOVE    ZERO            TO  IDX-GYO1
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               COMPUTE IDX-GYO1    =   IDX-GYO     +   4
               MOVE    WRK-HENSYU(IDX-GYO)
                                       TO  WRK-YKJ-KONOKOKA(IDX-GYO1)
           END-PERFORM
      *
           .
       230-EFFECT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票注意事項データ編集処理
      *****************************************************************
       230-CAUTION-SET-SEC         SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    1               TO  IDX-GYO
           MOVE    66              TO  GYO-MAX
           MOVE    40              TO  KOU-MAX
           MOVE    YAKUJYO-CAUTION TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX-GYO)
                                       TO  WRK-YKJ-CYUJKO(IDX-GYO)
           END-PERFORM
      *
           .
       230-CAUTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票形状データセット処理
      *****************************************************************
       230-KEIJYO-SET-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
      *    画像が存在すればセットする
           IF      YAKUJYO-PHOTO   NOT =   SPACE
      *----(04.08.01)--ADD-START---
      *        データからイメージファイル作成
               INITIALIZE                  ORCSMIAREA
               MOVE    YAKUJYO-SRYCD   TO  ORCSMI-SRYCD
               MOVE    SGETTEMP-TEMPDIR
                                       TO  ORCSMI-TEMPDIR
               STRING  YAKUJYO-PHOTO       DELIMITED BY SPACE
                       CNT-KENSU           DELIMITED BY SPACE
                                   INTO    ORCSMI-FILENAME
               END-STRING
               CALL    "ORCSMILOAD"        USING
                                           ORCSMIAREA
                                           SPA-AREA
      *
               MOVE    SPACE           TO  FILE-IMAGE
               STRING  SGETTEMP-TEMPDIR    DELIMITED BY SPACE
                       WRK-CONS-MEDIMGDIR  DELIMITED BY SPACE
                       "/"                 DELIMITED BY SIZE
                       YAKUJYO-PHOTO       DELIMITED BY SPACE
                       CNT-KENSU           DELIMITED BY SPACE
                                   INTO    FILE-IMAGE
               END-STRING
      *----(04.08.01)--ADD-END-----
      *----(04.08.01)--UPD-START---
               INITIALIZE                  ORCSFILECHKAREA
      *         MOVE    YAKUJYO-PHOTO   TO  FILECHK-NAME
               MOVE    FILE-IMAGE      TO  FILECHK-NAME
               CALL    "ORCSFILECHK"       USING
                                           ORCSFILECHKAREA
               IF      FILECHK-RC      =   ZERO
      *             MOVE    YAKUJYO-PHOTO   TO  WRK-YKJ-GAZO
                   MOVE    FILE-IMAGE      TO  WRK-YKJ-GAZO
                   MOVE    1               TO  FLG-OK
               END-IF
      *----(04.08.01)--UPD-END-----
           END-IF
      *
           IF      FLG-OK          =   ZERO
               PERFORM 231-KEIJYO-SET1-SEC
           ELSE
               PERFORM 231-KEIJYO-SET2-SEC
           END-IF
      *
           .
       230-KEIJYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票形状データセット１処理
      *****************************************************************
       231-KEIJYO-SET1-SEC         SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    1               TO  IDX-GYO
           MOVE    16              TO  KOU-MAX
      *
      *    プログラムオプションで切り替え
           EVALUATE    PARA-STYLE
               WHEN    SPACE
               WHEN    0
               WHEN    2
                   MOVE    11              TO  GYO-MAX
               WHEN    1
               WHEN    3
                   MOVE    66              TO  GYO-MAX
           END-EVALUATE
      *
      *    色編集
           MOVE    YAKUJYO-COLOR   TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
      *    形編集
           MOVE    YAKUJYO-SHAPE   TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
      *    記号編集
           IF      YAKUJYO-MARK    NOT =   SPACE
      *        記号チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    YAKUJYO-MARK    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-SYUBETU =   1
      *            半角なら文字数を減らす
                   MOVE    17              TO  KOU-MAX
               END-IF
      *        記号編集
               MOVE    YAKUJYO-MARK    TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
           END-IF
      *
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX-GYO)
                                       TO  WRK-YKJ-KEIJYO(IDX-GYO)
           END-PERFORM
      *
           .
       231-KEIJYO-SET1-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票形状データセット２処理
      *****************************************************************
       231-KEIJYO-SET2-SEC         SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    7               TO  IDX-GYO
           MOVE    16              TO  KOU-MAX
      *
      *    プログラムオプションで切り替え
           EVALUATE    PARA-STYLE
               WHEN    SPACE
               WHEN    0
               WHEN    1
                   GO  TO  231-KEIJYO-SET2-EXT
               WHEN    2
                   MOVE    11              TO  GYO-MAX
               WHEN    3
                   MOVE    66              TO  GYO-MAX
           END-EVALUATE
      *
      *    色編集
           MOVE    YAKUJYO-COLOR   TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
      *    記号編集
           IF      YAKUJYO-MARK    NOT =   SPACE
      *        記号チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    YAKUJYO-MARK    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF      KANACHK-SYUBETU =   1
      *            半角なら文字数を減らす
                   MOVE    17              TO  KOU-MAX
               END-IF
      *        記号編集
               MOVE    YAKUJYO-MARK    TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
           END-IF
      *
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX-GYO)
                                       TO  WRK-YKJ-KEIJYO(IDX-GYO)
           END-PERFORM
      *
           .
       231-KEIJYO-SET2-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データセット処理
      *****************************************************************
       240-NAIYO-SET-SEC           SECTION.
      *
           INITIALIZE                  WRK-HENSYU-AREA
           MOVE    3               TO  IDX-GYO
           MOVE    66              TO  GYO-MAX
           MOVE    18              TO  KOU-MAX
           MOVE    SPACE           TO  WRK-ZENSUMAE
           MOVE    SPACE           TO  WRK-ZENSUATO
           MOVE    0               TO  WRK-ZENSURYO
      *
      *    用法編集
           IF      ORCHCN30-YSRYCD-NAME(CNT-PARA)  =   SPACE
               MOVE    ORCHCN30-YSRYCD(CNT-PARA)   TO  WRK-SRYCD
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-NAME            TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
           ELSE
      *----(04.08.03)--ADD-START---
               MOVE    ORCHCN30-YSRYCD(CNT-PARA)   TO  WRK-SRYCD
               PERFORM 900-TENSU-READ-SEC
      *----(04.08.03)--ADD-END-----
               MOVE    ORCHCN30-YSRYCD-NAME(CNT-PARA)
                                           TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
           END-IF
      *
      *    回数カウント
           MOVE    SPACE           TO  WRK-SSTKIJUNCD
           MOVE    ZERO            TO  CNT-KAISU
           MOVE    ZERO            TO  WRK-HENKBN-G
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL   IDX-KBN     >   5
               IF      TNS-SSTKIJUNCD(IDX-KBN) =   1
                   COMPUTE CNT-KAISU   =   CNT-KAISU   +   1
                   MOVE    "1"         TO  WRK-SSTKIJUNCD(IDX-KBN:1)
               END-IF
           END-PERFORM
      *
      *    点数マスタ薬剤検索
           MOVE    ORCHCN30-SRYCD(CNT-PARA)
                                   TO  WRK-SRYCD
           PERFORM 900-TENSU-READ-SEC
           MOVE    TNS-NAME        TO  WRK-YKZNAME
           MOVE    TNS-TANINAME    TO  WRK-TANINAME
      *
      *    点数付加情報検索
           PERFORM 900-TNSP-READ-SEC
      *
      *    換算単位があるが値がない場合は単位のみ変換
           IF    ( TNSP-KANZANTANICD   NOT =   SPACE ) AND
                 ( TNSP-KANZANTANICD   NOT =   "000" ) AND
                 ( TNSP-KANZANCHI  =   ZERO )
               MOVE    TNSP-KANZANTANINAME TO  WRK-TANINAME
           END-IF
      *
      *    区分編集
           IF      CNT-KAISU       >   ZERO
               PERFORM 240-KBN-SET-SEC
           END-IF
      *
      *    全数量編集
           IF      TNSP-ALLSURYOKBN    NOT =   "0"
               PERFORM 241-NAIYO-SURYO-SET-SEC
           END-IF
      *
      *    その他の用法
           IF      ORCHCN30-YOHOU-G(CNT-PARA)  NOT =   SPACE
               PERFORM 241-NAIYO-YOUHOU-SET-SEC
           END-IF
      *    コメント
      *D   IF      PARA-COMMENT        =   1
               IF      ORCHCN30-COMMENT-G(CNT-PARA)    NOT =   SPACE
                   PERFORM 241-NAIYO-COMMENT-SET-SEC
               END-IF
      *D   END-IF
      *
      *    日数編集
           IF    ( ORCHCN30-ZAIKBN(CNT-PARA)   NOT =   3 ) AND
                 ( ORCHCN30-ZAIKBN(CNT-PARA)   NOT =   4 ) AND
                 ( TNSP-SOURYOHENKBN           NOT =   "1" )
               PERFORM 241-NAIYO-NISSU-SET-SEC
           END-IF
      *
           IF    ( PARA-TIMESURYO  =   "1" )   AND
                (( PARA-COMMENT2   =   "2" )   OR
                (( PARA-COMMENT2   =   "1" )   AND
                 ( WRK-YKJ-KBN-G   =   SPACE )))
      *        １日分が割切れていなかったらエラー
               COMPUTE WRK-SURYO   =   WRK-HENKBN(1)
                                   +   WRK-HENKBN(2)
                                   +   WRK-HENKBN(3)
                                   +   WRK-HENKBN(4)
                                   +   WRK-HENKBN(5)
               IF      ORCHCN30-SURYO(CNT-PARA)    =   WRK-SURYO
                   MOVE    5           TO  IDX-KBN
                   PERFORM 241-NAIYO-FUKUYOU-SET-SEC
                   PERFORM VARYING  IDX-KBN    FROM    1   BY  1
                           UNTIL  ( IDX-KBN    >   4 )
                       PERFORM 241-NAIYO-FUKUYOU-SET-SEC
                   END-PERFORM
               END-IF
           END-IF
      *
           IF     WRK-YKJ-KBN-G    NOT =   SPACE
      *        回数編集
               IF      CNT-KAISU       >   ZERO
                   PERFORM 241-NAIYO-KAISU-SET-SEC
               END-IF
           END-IF
      *
      *    換算前があればセット
           IF      WRK-ZENSUMAE        NOT =   SPACE
               MOVE    WRK-ZENSUMAE    TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
           END-IF
      *
      *    全数があればセット
           IF      WRK-ZENSUATO        NOT =   SPACE
               MOVE    WRK-ZENSUATO    TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
           END-IF
      *
      *    内容にセット
           PERFORM VARYING IDX-GYO     FROM    1   BY  1
                   UNTIL   IDX-GYO     >   GYO-MAX
               MOVE    WRK-HENSYU(IDX-GYO)
                                       TO  WRK-YKJ-NAIYO(IDX-GYO)
           END-PERFORM
      *
           .
       240-NAIYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容服用時点用法編集処理
      *****************************************************************
       241-NAIYO-FUKUYOU-SET-SEC   SECTION.
      *
           IF      WRK-HENKBN(IDX-KBN) =   0
               GO  TO  241-NAIYO-FUKUYOU-SET-EXT
           END-IF
      *
           MOVE    WRK-HENKBN(IDX-KBN) TO  WRK-SURYO
      *    数量Ｚ編集
           PERFORM 250-ZHEN-SURYO-SET-SEC
           MOVE    SPACE       TO WRK-HENSYUDATA
           EVALUATE    IDX-KBN
               WHEN    1
                   MOVE    "朝："  TO  WRK-HENSYUDATA(1:4)
               WHEN    2
                   MOVE    "昼："  TO  WRK-HENSYUDATA(1:4)
               WHEN    3
                   MOVE    "夕："  TO  WRK-HENSYUDATA(1:4)
               WHEN    4
                   MOVE    "寝："  TO  WRK-HENSYUDATA(1:4)
               WHEN    5
                   MOVE    "起："  TO  WRK-HENSYUDATA(1:4)
           END-EVALUATE
      *    全角変換
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-HENSURYO    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           STRING  KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       DELIMITED BY SIZE
                   WRK-TANINAME        DELIMITED BY SPACE
                                 INTO  WRK-HENSYUDATA(5:)
           END-STRING
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       241-NAIYO-FUKUYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データ日数編集処理
      *****************************************************************
       241-NAIYO-NISSU-SET-SEC     SECTION.
      *
      *    全角変換
           MOVE    ORCHCN30-KAISU(CNT-PARA)    TO  WRK-ZZ9
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  IDX-STR1
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL   IDX-STR     >   3
               IF      WRK-ZZ9(IDX-STR:1)  NOT =   SPACE
                   COMPUTE IDX-STR1    =   IDX-STR1    +   1
                   MOVE    WRK-ZZ9(IDX-STR:1)
                                       TO  WRK-HENSURYO(IDX-STR1:1)
               END-IF
           END-PERFORM
      *
      *    全角変換
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-HENSURYO    TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
      *
           MOVE    SPACE           TO  WRK-HENSYUDATA
           EVALUATE    ORCHCN30-ZAIKBN(CNT-PARA)
               WHEN    1
                   STRING  KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               DELIMITED  BY  SIZE
                           "日分"              DELIMITED  BY  SIZE
                           INTO                WRK-HENSYUDATA
                   END-STRING
               WHEN    2
                   STRING  KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               DELIMITED  BY  SIZE
                           "回分"              DELIMITED  BY  SIZE
                           INTO                WRK-HENSYUDATA
                   END-STRING
           END-EVALUATE
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       241-NAIYO-NISSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データ回数編集処理
      *****************************************************************
       241-NAIYO-KAISU-SET-SEC     SECTION.
      *
      *    全角変換
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    CNT-KAISU       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
      *
      *    数量文字セット
           MOVE    SPACE           TO  WRK-HENSYUDATA
           STRING  "１日"              DELIMITED  BY  SIZE
                   KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                       DELIMITED  BY  SIZE
                   "回上記量"          DELIMITED  BY  SIZE
                   INTO                WRK-HENSYUDATA
           END-STRING
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       241-NAIYO-KAISU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票内容データ数量編集処理
      *****************************************************************
       241-NAIYO-SURYO-SET-SEC     SECTION.
      *
      *    全数量
           IF      WRK-ZENSURYO    >   0
               IF   ( ORCHCN30-ZAIKBN(CNT-PARA)    NOT =   3 ) AND
                    ( TNSP-SOURYOHENKBN            NOT =   "1" )
                   COMPUTE WRK-SURYO   =   WRK-ZENSURYO
                                       *   ORCHCN30-KAISU(CNT-PARA)
               ELSE
                   COMPUTE WRK-SURYO   =   WRK-ZENSURYO
                                       *   ORCHCN30-SRYKAISU(CNT-PARA)
               END-IF
           ELSE
               IF   ( ORCHCN30-ZAIKBN(CNT-PARA)    NOT =   3 ) AND
                    ( TNSP-SOURYOHENKBN            NOT =   "1" )
                   COMPUTE WRK-SURYO   =   ORCHCN30-SURYO(CNT-PARA)
                                       *   ORCHCN30-KAISU(CNT-PARA)
               ELSE
                   MOVE    ORCHCN30-SURYO(CNT-PARA)    TO  WRK-SURYO
               END-IF
      *        換算値で数量を割る
               COMPUTE WRK-WARI    =   WRK-SURYO
                                   /   TNSP-KANZANCHI
      *        割り切れればセット
               IF      WRK-SURYO       =   WRK-WARI
                                       *   TNSP-KANZANCHI
                   MOVE    WRK-WARI        TO  WRK-SURYO
                   MOVE    TNSP-KANZANTANINAME TO  WRK-TANINAME
               END-IF
           END-IF
      *
      *    数量Ｚ編集
           PERFORM 250-ZHEN-SURYO-SET-SEC
      *     IF      WRK-SURYO-X(14:)    =   SPACE
      *        全角変換
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    WRK-HENSURYO    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
      *        数量文字セット
               MOVE    SPACE           TO  WRK-HENSYUDATA
               STRING  "全"                DELIMITED  BY  SIZE
                       KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           DELIMITED  BY  SIZE
                       WRK-TANINAME        DELIMITED  BY  SPACE
                       INTO                WRK-HENSYUDATA
               END-STRING
               MOVE    WRK-HENSYUDATA  TO  WRK-ZENSUATO
      *     END-IF
      *
      *    換算されていれば元の数量をセット
           IF      TNS-TANINAME    NOT =   WRK-TANINAME
               MOVE    ORCHCN30-SURYO(CNT-PARA)    TO  WRK-SURYO
      *        数量Ｚ編集
               PERFORM 250-ZHEN-SURYO-SET-SEC
      *         IF      WRK-SURYO-X(14:)    =   SPACE
      *            全角変換
                   MOVE    SPACE           TO  ORCSKANACHKAREA
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    WRK-HENSURYO    TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
      *            １日分数量文字セット
                   MOVE    SPACE           TO  WRK-HENSYUDATA
                   STRING  "（"                DELIMITED  BY  SIZE
                           KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               DELIMITED  BY  SIZE
                           TNS-TANINAME        DELIMITED  BY  SPACE
                           "）"                DELIMITED  BY  SIZE
                                   INTO        WRK-HENSYUDATA
                   END-STRING
                   MOVE    WRK-HENSYUDATA  TO  WRK-ZENSUMAE
      *         END-IF
           END-IF
      *
           .
       241-NAIYO-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   その他の用法処理
      *****************************************************************
       241-NAIYO-YOUHOU-SET-SEC   SECTION.
      *
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL  (IDX-KBN     >   5 )
                   OR     (ORCHCN30-YOHOU(CNT-PARA IDX-KBN) =   SPACE )
               IF      ORCHCN30-YOHOU-NAME (CNT-PARA IDX-KBN)
                                           =   SPACE
      *            点数マスタ検索
                   MOVE    ORCHCN30-YOHOU (CNT-PARA IDX-KBN)
                                           TO  WRK-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   IF      FLG-TENSU           =   ZERO
                       MOVE    TNS-NAME        TO  WRK-HENSYUDATA
                       PERFORM 250-HENSYU-SYORI-SEC
                   END-IF
               ELSE
                   MOVE    ORCHCN30-YOHOU-NAME (CNT-PARA IDX-KBN)
                                               TO  WRK-HENSYUDATA
                   PERFORM 250-HENSYU-SYORI-SEC
               END-IF
           END-PERFORM
      *
           .
       241-NAIYO-YOUHOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   コメントコード処理
      *****************************************************************
       241-NAIYO-COMMENT-SET-SEC   SECTION.
      *
           PERFORM VARYING IDX-KBN2    FROM    1   BY  1
                   UNTIL  (IDX-KBN2     >   10 )
                   OR     (ORCHCN30-COMMENT-CD(CNT-PARA IDX-KBN2)
                                       =   SPACE )
      *----(04.08.03)--ADD-START---
               IF    ( WRK-HENKBN-G    NOT =   ZERO ) AND
                     ( FLG-KBN         NOT =   2 ) AND
                     ( ORCHCN30-COMHENKBN(CNT-PARA IDX-KBN2) =   "1" )
                   COMPUTE IDX-KBN2    =   IDX-KBN2    +   1
               END-IF
      *----(04.08.03)--ADD-END-----
      *      コメント表示あり、または、用法コメント
           IF     (PARA-COMMENT        =   "1"  )
            OR    (ORCHCN30-COMMENT-CD(CNT-PARA IDX-KBN2)(1:3)
                                       =   "001"  )
               IF      ORCHCN30-COMMENT-MEI(CNT-PARA IDX-KBN2)
                                           =   SPACE
      *            点数マスタ検索
                   MOVE    ORCHCN30-COMMENT-CD(CNT-PARA IDX-KBN2)
                                           TO  WRK-SRYCD
                   PERFORM 900-TENSU-READ-SEC
                   IF      FLG-TENSU           =   ZERO
                       MOVE    TNS-NAME        TO  WRK-HENSYUDATA
                       PERFORM 250-HENSYU-SYORI-SEC
                   ELSE
                       MOVE    "データなし"    TO  WRK-HENSYUDATA
                       PERFORM 250-HENSYU-SYORI-SEC
                   END-IF
               ELSE
                   MOVE    ORCHCN30-COMMENT-MEI(CNT-PARA IDX-KBN2)
                                           TO  WRK-HENSYUDATA
                   PERFORM 250-HENSYU-SYORI-SEC
               END-IF
           END-IF
      *
           END-PERFORM
      *
           .
       241-NAIYO-COMMENT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票区分データ編集処理
      *****************************************************************
       240-KBN-SET-SEC             SECTION.
      *
      *    在宅なら抜ける
           IF    ( ORCHCN30-ZAIKBN(CNT-PARA)   =   4 ) OR
                 ( ORCHCN30-ZAIKBN(CNT-PARA)   =   3 ) OR
                 ( TNSP-SOURYOHENKBN           =   "1" )
               GO  TO  240-KBN-SET-EXT
           END-IF
      *
      *    数量を１日分にする
           IF      ORCHCN30-SRYKAISU(CNT-PARA)  >   ZERO
               COMPUTE WRK-SURYO   = ( ORCHCN30-SURYO(CNT-PARA)
                                   /   ORCHCN30-SRYKAISU(CNT-PARA))
           ELSE
               MOVE    ORCHCN30-SURYO(CNT-PARA) TO  WRK-SURYO
           END-IF
      *
      *    用量コード設定分セット
           MOVE    CNT-KAISU       TO  WRK-KAISU
           MOVE    ZERO            TO  WRK-HENKBN-G
           MOVE    ZERO            TO  FLG-KBN
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL ( IDX-KBN     >   5 )
                   OR    ( FLG-KBN     =   1 )
      *        用量コードがあった場合
               IF      ORCHCN30-YOURYOU(CNT-PARA IDX-KBN)    >   ZERO
      *            指定してない服用時点ならエラー
                   IF      WRK-SSTKIJUNCD(IDX-KBN:1)   NOT  =   "1"
                       MOVE    1               TO  FLG-KBN
                   ELSE
      *                用量の方が多い場合エラー
                       IF      WRK-SURYO       <
                                   ORCHCN30-YOURYOU(CNT-PARA IDX-KBN)
                           MOVE    1               TO  FLG-KBN
                       ELSE
                           MOVE   ORCHCN30-YOURYOU(CNT-PARA IDX-KBN)
                                               TO  WRK-HENKBN(IDX-KBN)
      *                    用量コード使用分は計算にいれない
                           COMPUTE WRK-SURYO   =   WRK-SURYO
                               -   ORCHCN30-YOURYOU(CNT-PARA IDX-KBN)
                           COMPUTE WRK-KAISU   =   WRK-KAISU   -   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *----(04.08.03)--ADD-START---
      *    エラー、 数量なくなれば処理抜ける
           IF    ( FLG-KBN  =   1 ) OR
                (( WRK-KAISU   NOT =   ZERO )  AND
                 ( WRK-SURYO       =   ZERO )) OR
                (( WRK-KAISU       =   ZERO )  AND
                 ( WRK-SURYO   NOT =   ZERO ))
               MOVE    ZERO            TO  WRK-HENKBN-G
               GO  TO  240-KBN-SET-EXT
           END-IF
      *    帳票区分データ服用用法コード編集処理
           PERFORM 241-KBN-INPUTSURYOU-SET-SEC
      *----(04.08.03)--ADD-END-----
      *----(04.08.03)--UPD-START---
      *    エラー、 数量なくなれば処理抜ける
           IF    ( FLG-KBN  =   1 ) OR
                (( WRK-KAISU   NOT =   ZERO )  AND
                 ( WRK-SURYO       =   ZERO )) OR
                (( WRK-KAISU       =   ZERO )  AND
                 ( WRK-SURYO   NOT =   ZERO ))
               MOVE    ZERO            TO  WRK-HENKBN-G
               GO  TO  240-KBN-SET-EXT
           END-IF
      *----(04.08.03)--UPD-END-----
      *
      *    入力分をセット
           COMPUTE WRK-SURYO   =   WRK-SURYO   /   WRK-KAISU
           MOVE    ZERO            TO  FLG-KBN
           PERFORM VARYING  IDX-KBN    FROM    1   BY  1
                   UNTIL  ( IDX-KBN    >   5 )
                   OR     ( FLG-KBN    =   1 )
      *        服用時点コードで設定していない場合は割ったものをセット
               IF      WRK-SSTKIJUNCD(IDX-KBN:1)   =   "1"
                   IF      WRK-HENKBN(IDX-KBN)     =   0
                       MOVE    WRK-SURYO       TO  WRK-HENKBN(IDX-KBN)
                   END-IF
               END-IF
      *        換算値があれば変換
               IF      TNSP-KANZANCHI  >   ZERO
                   COMPUTE WRK-WARI    =   WRK-HENKBN(IDX-KBN)
                                       /   TNSP-KANZANCHI
      *            割り切れればセット
                   IF      WRK-HENKBN(IDX-KBN)
                               = ( WRK-WARI    *   TNSP-KANZANCHI )
                       MOVE    WRK-WARI        TO  WRK-HENKBN(IDX-KBN)
                       MOVE    TNSP-KANZANTANINAME TO  WRK-TANINAME
                   ELSE
                       MOVE    1               TO  FLG-KBN
                   END-IF
               END-IF
           END-PERFORM
      *    エラーなら処理抜ける
           IF      FLG-KBN  =   1
               MOVE    ZERO            TO  WRK-HENKBN-G
               GO  TO  240-KBN-SET-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-YKJ-KBN-G
           MOVE    SPACE           TO  WRK-YKJ-TANI-G
           MOVE    ZERO            TO  FLG-KBN
           PERFORM VARYING  IDX-KBN    FROM    1   BY  1
                   UNTIL  ( IDX-KBN    >   5 )
                   OR     ( FLG-KBN    =   1 )
               IF      WRK-HENKBN(IDX-KBN)     NOT =   0
                   MOVE    WRK-HENKBN(IDX-KBN) TO  WRK-SURYO
      *            数量Ｚ編集
                   PERFORM 250-ZHEN-SURYO-SET-SEC
                   IF    ( PARA-TIMESURYO      NOT =   1 ) AND
                         ( WRK-HENSURYO(2:1)   =   "." )
                       COMPUTE WRK-ZENSURYO    =   WRK-ZENSURYO
                                               +   1
                       MOVE    "１"        TO  WRK-YKJ-KBN(IDX-KBN)
                       MOVE    "包"        TO  WRK-TANINAME
                       MOVE    2           TO  FLG-KBN
                   ELSE
                       IF      WRK-HENSURYO(4:)    NOT =   SPACE
                           MOVE    1               TO  FLG-KBN
                       ELSE
                           COMPUTE WRK-ZENSURYO    =   WRK-ZENSURYO
                                                   +   WRK-SURYO
                           IF      WRK-HENSURYO(2:)    NOT =   SPACE
                               MOVE    WRK-HENSURYO(1:3)
                                               TO  WRK-YKJ-KBN(IDX-KBN)
                           ELSE
      *                        全角変換
                               MOVE    SPACE       TO  ORCSKANACHKAREA
                               INITIALIZE              ORCSKANACHKAREA
                               MOVE    "2"         TO  KANACHK-SYORI
                               MOVE    WRK-HENSURYO(1:1)
                                                   TO  KANACHK-MAE-INPUT
                               CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-YKJ-KBN(IDX-KBN)
                           END-IF
                       END-IF
                   END-IF
                   IF   (( WRK-TANINAME    =   "包" )  AND
                         ( PARA-TANI   =   "2" OR  "3" )) OR
                        (( WRK-TANINAME    =   "ｇ" )  AND
                         ( PARA-TANI   =   "1" OR  "3" ))
                       MOVE    WRK-TANINAME  TO  WRK-YKJ-TANI(IDX-KBN)
                   END-IF
               END-IF
           END-PERFORM
      *    エラーならクリアする
           IF      FLG-KBN         =   1
               MOVE    SPACE           TO  WRK-YKJ-KBN-G
               MOVE    SPACE           TO  WRK-YKJ-TANI-G
               MOVE    ZERO            TO  WRK-ZENSURYO
           END-IF
      *
           .
       240-KBN-SET-EXT.
           EXIT.
      *----(04.08.03)--ADD-START---
      *****************************************************************
      *    帳票区分データ服用用法コード編集処理
      *****************************************************************
       241-KBN-INPUTSURYOU-SET-SEC SECTION.
      *
           IF      WRK-HENKBN-G    NOT =   ZERO
               GO  TO  241-KBN-INPUTSURYOU-SET-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-KBN
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL ( IDX-KBN     >   5 )
                   OR    ( FLG-KBN     =   1 )
      *        服用方法コードがあった場合
               IF      ORCHCN30-INPUTSURYOU(CNT-PARA IDX-KBN)  >   ZERO
      *            指定してない服用時点ならエラー
                   IF    WRK-SSTKIJUNCD(IDX-KBN:1)     NOT  =   "1"
                       MOVE    1               TO  FLG-KBN
                   ELSE
      *                服用方法コードの方が多い場合エラー
                       IF      WRK-SURYO       <
                               ORCHCN30-INPUTSURYOU(CNT-PARA IDX-KBN)
                           MOVE    1               TO  FLG-KBN
                       ELSE
                           MOVE
                               ORCHCN30-INPUTSURYOU(CNT-PARA IDX-KBN)
                                               TO  WRK-HENKBN(IDX-KBN)
      *                    服用方法コード使用分は計算にいれない
                           COMPUTE WRK-SURYO   =   WRK-SURYO
                           -   ORCHCN30-INPUTSURYOU(CNT-PARA IDX-KBN)
                           COMPUTE WRK-KAISU   =   WRK-KAISU   -   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       241-KBN-INPUTSURYOU-SET-EXT.
           EXIT.
      *----(04.08.03)--ADD-END-----
      *****************************************************************
      *   帳票区分データ用量コード編集処理
      *****************************************************************
       241-KBN-YOURYOU-SET-SEC    SECTION.
      *
      *    指定してない服用時点ならエラー
           IF      WRK-SSTKIJUNCD(IDX-KBN:1)   NOT  =   "1"
               MOVE    1               TO  FLG-KBN
               GO  TO  241-KBN-YOURYOU-SET-EXT
           END-IF
      *
      *    用量の方が多い場合エラー
           IF      WRK-SURYO   <   ORCHCN30-YOURYOU(CNT-PARA IDX-KBN)
               MOVE    1               TO  FLG-KBN
               GO  TO  241-KBN-YOURYOU-SET-EXT
           END-IF
      *
           MOVE   ORCHCN30-YOURYOU(CNT-PARA IDX-KBN)
                                       TO WRK-HENKBN(IDX-KBN)
      *
      *    用量コード使用分は計算にいれない
           COMPUTE WRK-SURYO   =   WRK-SURYO
                               -   ORCHCN30-YOURYOU(CNT-PARA IDX-KBN)
           COMPUTE WRK-KAISU   =   WRK-KAISU   -   1
      *
           .
       241-KBN-YOURYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票Ｚ編集処理
      *****************************************************************
       250-ZHEN-SURYO-SET-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
      *    数量小数点以下編集
           MOVE    ZERO            TO  FLG-OK
           PERFORM VARYING IDX-STR     FROM    5   BY  -1
                   UNTIL   IDX-STR     <   1
               IF      WRK-SURYO-S2(IDX-STR:1) NOT =   ZERO
                   MOVE    1           TO  FLG-OK
               END-IF
               IF      FLG-OK      =   1
                   MOVE    WRK-SURYO-S2(IDX-STR:1)
                                        TO  WRK-SURYO-Z3(IDX-STR:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."             TO  WRK-SURYO-Z2
               IF  WRK-SURYO-X(1:8)    =   SPACE
                   MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  IDX-STR1
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL   IDX-STR     >   14
               IF      WRK-SURYO-X(IDX-STR:1)  NOT =   SPACE
                   COMPUTE IDX-STR1    =   IDX-STR1    +   1
                   MOVE  WRK-SURYO-X(IDX-STR:1)
                                           TO  WRK-HENSURYO(IDX-STR1:1)
               END-IF
           END-PERFORM
      *
           .
       250-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-HENSYU-SYORI-SEC        SECTION.
      *
      *    初期値セット
           MOVE    1               TO  IDX-STR
           MOVE    1               TO  IDX-KET
      *
      *    最大行を超えるか編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   4000 )
                   OR     (IDX-GYO     >   GYO-MAX )
                   OR     (WRK-HENSYUDATA(IDX-STR:)    =   SPACE)
      *        改行なら最大字セット
               IF      WRK-HENSYUDATA(IDX-STR:1)   =   X"0A"
                   MOVE    KOU-MAX         TO  IDX-KET
                   ADD     1               TO  IDX-STR
               ELSE
      *            全角セット
                   IF      WRK-HENSYUDATA(IDX-STR:1)    >=  X"A1"
                       MOVE    WRK-HENSYUDATA(IDX-STR:2)
                                   TO  WRK-HENSYU(IDX-GYO)(IDX-KET:2)
                       ADD     2               TO  IDX-STR
                       ADD     2               TO  IDX-KET
      *            半角セット
                   ELSE
                       MOVE    WRK-HENSYUDATA(IDX-STR:1)
                                   TO  WRK-HENSYU(IDX-GYO)(IDX-KET:1)
                       ADD     1               TO  IDX-STR
                       ADD     1               TO  IDX-KET
                   END-IF
               END-IF
      *        最大字で改行
               IF         (IDX-KET         >=  KOU-MAX)
                   OR    ((IDX-STR         <=  4000)
                   AND    (WRK-HENSYUDATA(IDX-STR:) =   SPACE))
                   ADD     1               TO  IDX-GYO
                   MOVE    1               TO  IDX-KET
      *            改行なら次へ
                   IF      WRK-HENSYUDATA(IDX-STR:1)   =   X"0A"
                       COMPUTE IDX-STR =   IDX-STR     +   1
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       250-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       400-PRT-WRITE-SEC           SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC30"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE        TO  HC01PARA-CNT
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    レイヤー情報セット
           PERFORM UNTIL   CNT-LINE    >=  6
               ADD     1               TO  CNT-LINE
               ADD     1               TO  IDX-LAYER
               MOVE    " -L ST"        TO  WRK-HENLAYER(IDX-LAYER:6)
               ADD     6               TO  IDX-LAYER
               MOVE    CNT-LINE        TO  WRK-HENLAYER(IDX-LAYER:1)
               ADD     1               TO  IDX-LAYER
               MOVE    " -L WK"        TO  WRK-HENLAYER(IDX-LAYER:6)
               ADD     6               TO  IDX-LAYER
               MOVE    CNT-LINE        TO  WRK-HENLAYER(IDX-LAYER:1)
               ADD     1               TO  IDX-LAYER
               MOVE    " -L ED"        TO  WRK-HENLAYER(IDX-LAYER:6)
               ADD     6               TO  IDX-LAYER
               MOVE    CNT-LINE        TO  WRK-HENLAYER(IDX-LAYER:1)
           END-PERFORM
      *
      *    レイヤーオプションセット
           IF      IDX-LAYER       NOT =   ZERO
               STRING  "MonpeLayerIn"  DELIMITED  BY  SIZE
                       WRK-HENLAYER(1:IDX-LAYER)
                                       DELIMITED  BY  SIZE
                       "MonpeLayerOut" DELIMITED  BY  SIZE
                       INTO            HCN301-LAYER-OPTION
               END-STRING
           END-IF
      *
      *    印刷用データ出力
           MOVE    HCN301          TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCN301.red"    TO  MKPRT-DIA
           MOVE    "pdf"           TO  MKPRT-TYPE
      *****MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    ASGNPARA        TO  MKPRT-DAT
           MOVE    SYS-1031-PRTNM(10)
                                   TO  MKPRT-PRTNM
           MOVE   "お薬情報"       TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       410-HEAD-SET-SEC            SECTION.
      *
           INITIALIZE                  HCN301
      *
           MOVE    ZERO            TO  CNT-LINE
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE        TO  WRK-ZZ9
           MOVE    WRK-ZZ9         TO  HCN301-PAGE
      *
           MOVE    WRK-YKJ-SYORIBI TO  HCN301-SYORIBI
           MOVE    WRK-YKJ-SYURUI  TO  HCN301-SYURUI
           MOVE    WRK-YKJ-PTNUM   TO  HCN301-ID
           MOVE    WRK-YKJ-KANANAME
                                   TO  HCN301-KANANAME
      *
           IF      WRK-YKJ-NAME(35:)   =   SPACE
               MOVE    WRK-YKJ-NAME(1:34)
                                       TO  HCN301-NAME
           ELSE
               MOVE    WRK-YKJ-NAME    TO  HCN301-NAME2
               INSPECT HCN301-NAME2    REPLACING ALL "  " BY "　"
           END-IF
      *
           MOVE    WRK-YKJ-JYUSYO  TO  HCN301-JYUSYO
           INSPECT HCN301-JYUSYO       REPLACING ALL "  " BY "　"
      *
           MOVE    WRK-YKJ-HSPNAME TO  HCN301-HSPNAME
           INSPECT HCN301-HSPNAME       REPLACING ALL "  " BY "　"
      *
           MOVE    WRK-YKJ-TEL     TO  HCN301-TEL
           MOVE    WRK-YKJ-DRNAME  TO  HCN301-DRNAME
           MOVE    WRK-YKJ-BTUNAME TO  HCN301-BTUNAME
      *
           PERFORM VARYING IDX-GYO FROM    1   BY 1
                   UNTIL   IDX-GYO     >   2
              MOVE    WRK-YKJ-COMMENT(IDX-GYO)
                                       TO  HCN301-COMMENT(IDX-GYO)
           END-PERFORM
      *
           PERFORM VARYING IDX-GYO FROM    1   BY 1
                   UNTIL   IDX-GYO >   6
               MOVE    FILE-NAME       TO  HCN301-GAZO(IDX-GYO)
           END-PERFORM
      *
           MOVE    SPACE           TO  WRK-HENLAYER
           MOVE    ZERO            TO  IDX-LAYER
      *
           .
       410-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹセット処理
      *****************************************************************
       420-BODY-SET-SEC            SECTION.
      *
      *    ページ件数を超えていたら改ページ
           IF      CNT-LINE    =   6
               PERFORM 400-PRT-WRITE-SEC
               PERFORM 410-HEAD-SET-SEC
           END-IF
      *
           COMPUTE CNT-LINE   =    CNT-LINE    +   1
      *
           MOVE    WRK-YKJ-NUM     TO  HCN301-NUM(CNT-LINE)
           MOVE    WRK-YKJ-GAZO    TO  HCN301-GAZO(CNT-LINE)
      *
           MOVE    WRK-YKJ-YKZNAME-G
                                   TO  HCN301-YKZNAME-G(CNT-LINE)
           MOVE    WRK-YKJ-KBN-G   TO  HCN301-KBN-G(CNT-LINE)
           MOVE    WRK-YKJ-NAIYO-G(1:198)
                                   TO  HCN301-NAIYO-G(CNT-LINE)
           MOVE    WRK-YKJ-KEIJYO-G(1:176)
                                   TO  HCN301-KEIJYO-G(CNT-LINE)
           MOVE    WRK-YKJ-KONOKOKA-G(1:330)
                                   TO  HCN301-KONOKOKA-G(CNT-LINE)
           MOVE    WRK-YKJ-CYUJKO-G(1:440)
                                   TO  HCN301-CYUJKO-G(CNT-LINE)
           MOVE    WRK-YKJ-TANI-G  TO  HCN301-TANI-G(CNT-LINE)
      *
           MOVE    ZERO            TO  FLG-NAIYO
           MOVE    ZERO            TO  FLG-KEIJYO
           MOVE    ZERO            TO  FLG-KONO
           MOVE    ZERO            TO  FLG-CYUI
           MOVE    199             TO  IDX-NAIYO
           MOVE    177             TO  IDX-KEIJYO
           MOVE    331             TO  IDX-KONO
           MOVE    441             TO  IDX-CYUI
      *
      *    文字数がオーバーしていたら次行にセット
           PERFORM 430-BODY-NEXT-CHK-SEC
           PERFORM UNTIL ( FLG-NAIYO   =   1 )
                   AND   ( FLG-KEIJYO  =   1 )
                   AND   ( FLG-KONO    =   1 )
                   AND   ( FLG-CYUI    =   1 )
               PERFORM 430-BODY-NEXT-SET-SEC
               PERFORM 430-BODY-NEXT-CHK-SEC
           END-PERFORM
      *
           .
       420-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ次行チェック処理
      *****************************************************************
       430-BODY-NEXT-CHK-SEC       SECTION.
      *
      *    薬剤形状チェック
           IF    ( IDX-KEIJYO      >=  1056 ) OR
                 ( WRK-YKJ-KEIJYO-G(IDX-KEIJYO:)   =   SPACE )
               MOVE    1           TO  FLG-KEIJYO
           END-IF
      *    用量用法チェック
           IF    ( IDX-NAIYO       >=  1188 )   OR
                 ( WRK-YKJ-NAIYO-G(IDX-NAIYO:)     =   SPACE )
               MOVE    1           TO  FLG-NAIYO
           END-IF
      *    効能効果チェック
           IF    ( IDX-KONO        >=  1980 )  OR
                 ( WRK-YKJ-KONOKOKA-G(IDX-KONO:)   =   SPACE )
               MOVE    1           TO  FLG-KONO
           END-IF
      *    注意事項チェック
           IF    ( IDX-CYUI        >=  2640 )  OR
                 ( WRK-YKJ-CYUJKO-G(IDX-CYUI:)     =   SPACE )
               MOVE    1           TO  FLG-CYUI
           END-IF
      *
           .
       420-BODY-NEXT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ次行セット処理
      *****************************************************************
       430-BODY-NEXT-SET-SEC       SECTION.
      *
      *    ページ件数を超えていたら改ページ
           IF      CNT-LINE    =   6
               PERFORM 400-PRT-WRITE-SEC
               PERFORM 410-HEAD-SET-SEC
           ELSE
      *        レイヤーオプションセット
               ADD     1               TO  IDX-LAYER
               MOVE    " -L ED"        TO  WRK-HENLAYER(IDX-LAYER:6)
               ADD     6               TO  IDX-LAYER
               MOVE    CNT-LINE        TO  WRK-HENLAYER(IDX-LAYER:1)
           END-IF
      *
           COMPUTE CNT-LINE   =    CNT-LINE    +   1
      *
      *    レイヤーオプションセット
           ADD     1               TO  IDX-LAYER
           MOVE    " -L ST"        TO  WRK-HENLAYER(IDX-LAYER:6)
           ADD     6               TO  IDX-LAYER
           MOVE    CNT-LINE        TO  WRK-HENLAYER(IDX-LAYER:1)
      *
      *    薬剤形状、用量用法セット
           IF    ( FLG-NAIYO       =   1 )  AND
                 ( FLG-KEIJYO      =   1 )
               CONTINUE
           ELSE
      *        薬剤形状をセットする
               IF      FLG-KEIJYO      NOT =   1
                   MOVE    WRK-YKJ-KEIJYO-G(IDX-KEIJYO:176)
                                       TO  HCN301-KEIJYO-G(CNT-LINE)
                   COMPUTE IDX-KEIJYO  =   IDX-KEIJYO  +   176
               END-IF
      *        用量用法をセットする
               IF      FLG-NAIYO       NOT =   1
                   MOVE    WRK-YKJ-NAIYO-G(IDX-NAIYO:198)
                                       TO  HCN301-NAIYO-G(CNT-LINE)
                   COMPUTE IDX-NAIYO   =   IDX-NAIYO   +   198
               END-IF
           END-IF
      *
      *    効能効果セット
           IF      FLG-KONO        =   1
               CONTINUE
           ELSE
      *        効能効果をセットする
               MOVE    WRK-YKJ-KONOKOKA-G(IDX-KONO:330)
                                       TO  HCN301-KONOKOKA-G(CNT-LINE)
               COMPUTE IDX-KONO    =   IDX-KONO    +   330
           END-IF
      *
      *    注意事項セット
           IF      FLG-CYUI        =   1
               CONTINUE
           ELSE
      *        注意事項をセットする
               MOVE    WRK-YKJ-CYUJKO-G(IDX-CYUI:440)
                                       TO  HCN301-CYUJKO-G(CNT-LINE)
               COMPUTE IDX-CYUI    =   IDX-CYUI    +   440
           END-IF
      *
           .
       430-BODY-NEXT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
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
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬情マスタ読み込み
      *****************************************************************
       900-YAKUJYO-READ-SEC        SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
           MOVE    SPA-HOSPNUM     TO  YAKUJYO-HOSPNUM
           MOVE    ORCHCN30-SRYCD(CNT-PARA)
                                   TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_yakujyo"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
               ELSE
                   MOVE    1               TO  FLG-YAKUJYO
               END-IF
           ELSE
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-YAKUJYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-SRYYMD      TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD      TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
      *R02.2
      *    ユーザー単位反映
           IF    ( FLG-TENSU       =   ZERO )
             AND ( TNS-SRYCD(1:1)  =   "6"
                                   OR  "7"  )
      *        点数付加情報検索
               PERFORM 900-TNSP-READ-SEC
               IF    ( FLG-TNSP            =   ZERO )
                AND  ( TNSP-USERTANICD NOT =   SPACE)
                   MOVE    TNSP-USERTANICD     TO  TNS-TANICD
                   MOVE    TNSP-USERTANINAME   TO  TNS-TANINAME
               END-IF
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加情報読み込み
      *****************************************************************
       900-TNSP-READ-SEC           SECTION.
      *
           INITIALIZE                  TNSP-REC
           MOVE    WRK-SRYCD       TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD      TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD      TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNSP-HOSPNUM
           MOVE    TNSP-REC        TO  MCPDATA-REC
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TNSP
                   MOVE    MCPDATA-REC     TO  TNSP-REC
               ELSE
                   MOVE    1               TO  FLG-TNSP
               END-IF
           ELSE
               MOVE    1               TO  FLG-TNSP
           END-IF
      *
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-TNSP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−基本情報読み込み
      *****************************************************************
       900-1001-READ-SEC           SECTION.
      *
      *    医療機関情報−基本情報読み込み
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           .
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−所在地、連絡先読み込み
      *****************************************************************
       900-1002-READ-SEC           SECTION.
      *
      *    医療機関情報−所在地、連絡先読み込み
           MOVE    SPACE           TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1002-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           .
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報読み込み
      *****************************************************************
       900-1010-READ-SEC           SECTION.
      *
      *    職員情報読み込み
           MOVE    SPACE           TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    ORCHCN30-DRCD   TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
           .
       900-1010-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名割り当て情報読み込み
      *****************************************************************
       900-1031-READ-SEC           SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYS-1031-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    SPA-SRYYMD      TO  ORCSPRTNM-SRYYMD
           MOVE    "1"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           .
       900-1031-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(13) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(13)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2
                                       TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(13)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SRYYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SRYYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
      *
           .
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込開始処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
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
      *
      *****************************************************************
      *    ＤＢ読込検索処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込終了処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *
