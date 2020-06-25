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
       PROGRAM-ID.             ORCBG006S.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 薬剤使用量統計（月報）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/04/19    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    NACL-森脇    04/08/31  条件修正
      *  02.01.01    NACL-森脇    05/01/27  高速化による変更
      *  03.05.01    NACL-森脇    07/06/05  グループ診療対応
      *  04.02.01    NACL-森脇    08/05/27  残量廃棄対応
      *  04.03.01    NACL-森脇    08/07/29  パラメタに入外区分追加
      *  04.05.01    NACL-森脇    09/08/07  パラメタに期間範囲追加
      *  04.05.02    NACL-森脇    08/09/18  病棟別集計対応
      *  04.05.03    NACL-森脇    09/12/15  ＣＳＶ出力対応
      *  04.08.01    NACL-森脇    14/07/23  一時ファイルディレクトリ設定
      *  04.08.02    NACL-森脇    16/03/17  金額種別４に対応
      *  04.08.03    NACL-森脇    16/09/01  持参薬対象外
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計用ファイル
           SELECT  TOKEI-FILE          ASSIGN    TOKEIPARA
                                       ORGANIZATION    IS  INDEXED
                                       ACCESS  MODE    IS  DYNAMIC
                                       RECORD  KEY     IS  TOKEI-KEY
                                       FILE    STATUS  IS  STS-TOKEI.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE        ASSIGN  RECEERR
                                       FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-NYUGAIKBN     PIC X(01).
               05  TOKEI-INGAIKBN      PIC 9(01).
               05  TOKEI-YKZKBN        PIC 9(01).
               05  TOKEI-KANANAME      PIC X(200).
               05  TOKEI-SRYCD         PIC X(09).
      *         05  TOKEI-ENDYMD        PIC X(08).
               05  TOKEI-TEN           PIC 9(07)V9(02).
               05  TOKEI-SRYKA         PIC X(02).
           03  TOKEI-NAME              PIC X(200).
           03  TOKEI-TANI              PIC X(10).
           03  TOKEI-SRYSURYO          PIC 9(10)V9(05).
      *----(04.08.02)--ADD-START---
           03  TOKEI-TENSIKIBETU       PIC 9(01).
      *----(04.08.02)--ADD-END-----
           03  TOKEI-ENDYMD            PIC X(08).
           03  TOKEI-KOME              PIC X(01).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER                  PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCMG006.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SRYKBN              PIC 9(01).
           03  FLG-KOME                PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                PIC 9(02).
           03  CNT-PAGE                PIC 9(03).
           03  CNT-PAGE2               PIC 9(03).
           03  CNT-GPAGE               PIC 9(03).
           03  CNT-TOKEI               PIC 9(05).
           03  CNT-KOUHATU             PIC 9(05).
           03  CNT-SRYKA               PIC 9(05).
           03  CNT-MAX                 PIC 9(03).
           03  CNT-KETA                PIC 9(02).
           03  CNT-KETA2               PIC 9(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY              PIC 9(02).
               05  SYS-MM              PIC 9(02).
               05  SYS-DD              PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(03).
           03  IDX-D                   PIC 9(04).
      *
           03  IDW1                    PIC 9(04).
           03  IDW2                    PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-TOUKEIERR           PIC X(200).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
      *
           03  WRK-KIKAN-AREA.
               05  WRK-KIKAN           PIC X(50).
               05  WRK-STR             PIC 9(02).
               05  WRK-END             PIC 9(02).
               05  WRK-KAISU           PIC 9(07).
      *
           03  WRK-SRYCD               PIC X(9).
           03  WRK-YKZKBN              PIC X(01).
           03  WRK-INGAIKBN            PIC X(01).
           03  WRK-NYUGAIKBN           PIC X(01).
      *
           03  WRK-GSURYO              PIC 9(10)V9(05).
      *
           03  WRK-CYUSRYCD            PIC X(9).
           03  WRK-CYUSURYO            PIC 9(05)V9(05).
      *
           03  WRK-HENSURYO            PIC X(16).
           03  WRK-SURYO               PIC 9(10)V9(05).
           03  WRK-SURYO-R             REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1        PIC 9(10).
               05  WRK-SURYO-S2        PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1        PIC ZZZZZZZZZ9.
               05  WRK-SURYO-Z2        PIC X(01).
               05  WRK-SURYO-Z3        PIC 9(05).
      *
           03  WRK-HENTEN              PIC X(10).
      *----(04.08.02)--UPD-START---
           03  WRK-HENTENSU            PIC X(10).
           03  WRK-TEN                 PIC 9(07)V9(03).
           03  WRK-TENSU               PIC ZZZZZZ9.999.
      *----(04.08.02)--UPD-START---
      *
           03  WRK-HENKIN              PIC X(16).
           03  WRK-KIN                 PIC 9(10)V9(05).
           03  WRK-KIN-R               REDEFINES   WRK-KIN.
               05  WRK-KIN-S1          PIC 9(10).
               05  WRK-KIN-S2          PIC 9(05).
           03  WRK-KIN-X.
               05  WRK-KIN-Z1          PIC ZZZZZZZZZ9.
               05  WRK-KIN-Z2          PIC X(01).
               05  WRK-KIN-Z3          PIC 9(05).
      *
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-Z9                  PIC Z9.
      *
           03  WRK-WARIAI              PIC 9(07)V9(02).
           03  WRK-SRYKBN              PIC X(02).
      *
           03  WRK-YMD.
               05  WRK-SYSYMDWH        PIC X(22).
               05  WRK-YMD-MSG1        PIC X(4).
           03  WRK-DAI.
               05  WRK-DAI-MSG1        PIC X(36).
               05  WRK-YKZKBNWH        PIC X(12).
               05  WRK-INGAIKBNWH      PIC X(8).
           03  WRK-PAGE.
               05  WRK-PAGE1           PIC X(03).
               05  WRK-PAGE-MSG1       PIC X(01).
               05  WRK-PAGE2           PIC X(03).
      *
           03  WRK-TAI-AREA.
               05  WRK-TAI-TBL         OCCURS  100  TIMES.
                 07  WRK-TAI-SRYKA     PIC X(02).
                 07  WRK-TAI-NAIYO     PIC X(14).
           03  WRK-NAI-AREA.
               05  WRK-NAI-TBL         OCCURS  25  TIMES.
                 07  WRK-NAI-SRYCD     PIC X(9).
                 07  WRK-NAI-NAME      PIC X(200).
                 07  WRK-NAI-KOME      PIC X(01).
                 07  WRK-NAI-TEN       PIC X(10).
                 07  WRK-NAI-TANI      PIC X(10).
                 07  WRK-NAI-SURYO     PIC X(16)
                                       OCCURS  100  TIMES.
                 07  WRK-NAI-GAKU      PIC X(16)
                                       OCCURS  100  TIMES.
      *
      *    ＣＳＶデータ編集用
           03  WRK-REC             PIC X(20000).
           03  WRK-REC-LENGTH      PIC 9(05).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA        PIC X(20000).
               05  WRK-NUM         PIC -------------9.99999. 
               05  WRK-NAGASA      PIC 9(04).
               05  WRK-RECSTR      PIC 9(02). 
               05  WRK-SYOSUU      PIC 9(01).
               05  WRK-RECEND      PIC 9(01).
      *
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI          PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO          PIC X(01)   VALUE   X"0D".
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(16)   VALUE
                                                   "iyakuhin_toukei_".
               05  WRK-TO-DATA-KIKAN   PIC X(21).
           03  WRK-TO-DATA1.
               05  WRK-TO-DATA-STRYM   PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
           03  WRK-TO-DATA2.
               05  WRK-TO-DATA-STRYMD  PIC X(08).
               05  FILLER              PIC X(01)   VALUE   "-".
               05  WRK-TO-DATA-ENDYMD  PIC X(08).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "診療年月".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "入外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "入外区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "院外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "院外区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "薬剤区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "薬剤区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "診療行為コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "薬剤名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "単位".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "点数".
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-KOUHATU        PIC X(01).
      *
           03  PARA-NAME           PIC X(25).
           03  PARA-NAIYO          PIC X(50).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    診療科目情報情報
           COPY    "CPSK1005.INC".
      *
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療会計マスタ
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
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
      *    指定診療行為件数調サブ２
           COPY    "CPORCSS009.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
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
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *----(04.08.01)--UPD-START---
      *     03  RECEERR                 PIC X(100).
           03  RECEERRX                PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-STRYMD         PIC X(08).
           03  WRK-PARA-ENDYMD         PIC X(08).
           03  WRK-PARA-YKZKBN         PIC 9(01).
           03  WRK-PARA-INGAIKBN       PIC 9(01).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-SYORIKBN       PIC X(01).
           03  WRK-PARA-KIKANFLG       PIC X(01).
           03  WRK-PARA-SRYKBN         PIC X(02).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       WRK-PARA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  TTNS-AREA
           INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           MOVE    WRK-PARA-HOSPNUM TO SPA-HOSPNUM
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    "ORCBG006"      TO  JOB-PGID
           MOVE    "診療科別医薬品使用量統計（月報）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    "BG00601"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  TOKEIPARA-HOSPNUM
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
           IF      WRK-PARA-NYUGAIKBN  =   SPACE
               MOVE    "0"             TO  WRK-PARA-NYUGAIKBN
           END-IF
           IF    ( WRK-PARA-SYORIKBN   =   "1" )
           AND   ( WRK-PARA-NYUGAIKBN  NOT =   "1" )
               MOVE    "外来分は病棟別に集計できません"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  110-PARA-HENSYU-EXT
           END-IF
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 400-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH
      *
           MOVE    "作成"              TO  WRK-YMD-MSG1
      *
      *    表題編集
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    "0"
                   MOVE    "　　診療科別医薬品使用量統計（月報）"
                                           TO  WRK-DAI-MSG1
               WHEN    "2"
                   MOVE    "外来診療科別医薬品使用量統計（月報）"
                                           TO  WRK-DAI-MSG1
               WHEN    "1"
                   IF      WRK-PARA-SYORIKBN   =   "1"
                       MOVE    "　入院病棟別医薬品使用量統計（月報）" 
                                           TO  WRK-DAI-MSG1
                   ELSE
                       MOVE    "入院診療科別医薬品使用量統計（月報）" 
                                           TO  WRK-DAI-MSG1
                   END-IF
           END-EVALUATE
      *
      *    対象年月編集
           MOVE    SPACE           TO  WRK-KIKAN-AREA
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
      *
           IF      WRK-PARA-KIKANFLG  =   "1"
               MOVE    WRK-PARA-STRYMD     TO  WRK-SYMD
               PERFORM 400-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:16)   TO  WRK-KIKAN(1:16)
               MOVE    "分"                TO  WRK-KIKAN(17:2)
               MOVE    WRK-PARA-STRYMD(1:6)    TO  WRK-TO-DATA-STRYM
               MOVE    WRK-TO-DATA1            TO  WRK-TO-DATA-KIKAN
           ELSE
      *        開始日が終了日より大きい場合エラー
               IF      WRK-PARA-STRYMD >   WRK-PARA-ENDYMD
                   MOVE    "日付の範囲が正しくありません"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               ELSE
      *            開始日
                   MOVE    WRK-PARA-STRYMD     TO  WRK-SYMD
                   PERFORM 400-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  WRK-KIKAN(1:22)
                   MOVE    "〜"                TO  WRK-KIKAN(23:2)
      *            終了日
                   MOVE    WRK-PARA-ENDYMD     TO  WRK-SYMD
                   PERFORM 400-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  WRK-KIKAN(25:22)
                   MOVE    "分"                TO  WRK-KIKAN(47:2)
                   MOVE    WRK-PARA-STRYMD TO  WRK-TO-DATA-STRYMD
                   MOVE    WRK-PARA-ENDYMD TO  WRK-TO-DATA-ENDYMD
                   MOVE    WRK-TO-DATA2    TO  WRK-TO-DATA-KIKAN
               END-IF
           END-IF
      *
           PERFORM 100-OPTION-SELECT-SEC
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC       SECTION.
      *
           INITIALIZE                  PARA-AREA
      *
      *    後発医薬品
           MOVE    "KOUHATU"       TO  PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( PARA-NAIYO      =   "0" OR "1" )
               MOVE    PARA-NAIYO  TO  PARA-KOUHATU
           END-IF
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC        SECTION.
      *
           MOVE    SPACE           TO  PARA-NAIYO
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCBG006"      TO  ORCSPRGOPTION-PRGID
           MOVE    PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
      *
           MOVE    ORCSPRGOPTION-OT-PARA (1:1)
                                       TO  PARA-NAIYO
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF      WRK-PARA-SYORIKBN   =   "1"
      *        病棟セット処理
               PERFORM 210-MAIN-BTNUM-SEC
           ELSE
      *        診療科セット処理
               PERFORM 210-MAIN-SRYKA-SEC
           END-IF
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
           OPEN    I-O                 TOKEI-FILE
      *
      *    診療行為集計処理
           PERFORM 220-MAIN-SRY-SEC
      *
           CLOSE                       TOKEI-FILE
      *
           OPEN    INPUT               TOKEI-FILE
      *    集計統計データ順読込み
           PERFORM 900-TOKEI-NEXT-SEC
           IF      FLG-TOKEI   =   ZERO
      *        ＣＳＶ出力処理
               PERFORM 270-CSV-HEAD-SET-SEC
      *        統計印刷処理
               PERFORM 260-TOKEI-SET-SEC
                       UNTIL   FLG-TOKEI   =   1
           END-IF
           CLOSE                       TOKEI-FILE
      *
      *    データをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科セット処理
      *****************************************************************
       210-MAIN-SRYKA-SEC          SECTION.
      *
           INITIALIZE                  WRK-TAI-AREA
           MOVE    ZERO            TO  CNT-SRYKA
      *
      *    診療科目編集
           PERFORM 900-SYS1005-DBSELECT-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( FLG-SYSKANRI    =   1 )
                   OR    ( IDX             >   99  )
               MOVE    SYS-1005-KBNCD      TO  WRK-TAI-SRYKA(IDX)
               MOVE    SYS-1005-SRYKANAME1 TO  WRK-TAI-NAIYO(IDX)
               COMPUTE CNT-SRYKA   =   CNT-SRYKA   +   1
      *        診療科目編集
               PERFORM 900-SYS1005-DBFETCH-SEC
           END-PERFORM
      *    診療科目編集
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           EVALUATE    CNT-SRYKA
               WHEN    1   THRU    4
                   MOVE    5           TO  CNT-MAX
               WHEN    5   THRU    9
                   MOVE    10          TO  CNT-MAX
               WHEN    10  THRU    14
                   MOVE    15          TO  CNT-MAX
               WHEN    15  THRU    19
                   MOVE    20          TO  CNT-MAX
               WHEN    20  THRU    24
                   MOVE    25          TO  CNT-MAX
               WHEN    25  THRU    29
                   MOVE    30          TO  CNT-MAX
               WHEN    30  THRU    34
                   MOVE    35          TO  CNT-MAX
               WHEN    35  THRU    39
                   MOVE    40          TO  CNT-MAX
               WHEN    40  THRU    44
                   MOVE    45          TO  CNT-MAX
               WHEN    45  THRU    49
                   MOVE    50          TO  CNT-MAX
               WHEN    50  THRU    54
                   MOVE    55          TO  CNT-MAX
               WHEN    55  THRU    59
                   MOVE    60          TO  CNT-MAX
               WHEN    60  THRU    64
                   MOVE    65          TO  CNT-MAX
               WHEN    65  THRU    69
                   MOVE    70          TO  CNT-MAX
               WHEN    70  THRU    74
                   MOVE    75          TO  CNT-MAX
               WHEN    75  THRU    79
                   MOVE    80          TO  CNT-MAX
               WHEN    80  THRU    84
                   MOVE    85          TO  CNT-MAX
               WHEN    85  THRU    89
                   MOVE    90          TO  CNT-MAX
               WHEN    90  THRU    94
                   MOVE    95          TO  CNT-MAX
               WHEN    95  THRU    99
                   MOVE    100         TO  CNT-MAX
           END-EVALUATE
      *
           .
       210-MAIN-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟セット処理
      *****************************************************************
       210-MAIN-BTNUM-SEC          SECTION.
      *
           INITIALIZE                  WRK-TAI-AREA
           MOVE    ZERO            TO  CNT-SRYKA
      *
      *    病棟編集
           PERFORM 900-SYS5001-DBSELECT-SEC
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( FLG-SYSKANRI    =   1 )
                   OR    ( IDX             >   99  )
               MOVE    SYS-5001-KBNCD          TO  WRK-TAI-SRYKA(IDX)
               MOVE    SYS-5001-BTU-TANNAME    TO  WRK-TAI-NAIYO(IDX)
               COMPUTE CNT-SRYKA   =   CNT-SRYKA   +   1
      *        病棟編集編集
               PERFORM 900-SYS5001-DBFETCH-SEC
           END-PERFORM
      *    病棟編集編集
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
           EVALUATE    CNT-SRYKA
               WHEN    1   THRU    4
                   MOVE    5           TO  CNT-MAX
               WHEN    5   THRU    9
                   MOVE    10          TO  CNT-MAX
               WHEN    10  THRU    14
                   MOVE    15          TO  CNT-MAX
               WHEN    15  THRU    19
                   MOVE    20          TO  CNT-MAX
               WHEN    20  THRU    24
                   MOVE    25          TO  CNT-MAX
               WHEN    25  THRU    29
                   MOVE    30          TO  CNT-MAX
               WHEN    30  THRU    34
                   MOVE    35          TO  CNT-MAX
               WHEN    35  THRU    39
                   MOVE    40          TO  CNT-MAX
               WHEN    40  THRU    44
                   MOVE    45          TO  CNT-MAX
               WHEN    45  THRU    49
                   MOVE    50          TO  CNT-MAX
               WHEN    50  THRU    54
                   MOVE    55          TO  CNT-MAX
               WHEN    55  THRU    59
                   MOVE    60          TO  CNT-MAX
               WHEN    60  THRU    64
                   MOVE    65          TO  CNT-MAX
               WHEN    65  THRU    69
                   MOVE    70          TO  CNT-MAX
               WHEN    70  THRU    74
                   MOVE    75          TO  CNT-MAX
               WHEN    75  THRU    79
                   MOVE    80          TO  CNT-MAX
               WHEN    80  THRU    84
                   MOVE    85          TO  CNT-MAX
               WHEN    85  THRU    89
                   MOVE    90          TO  CNT-MAX
               WHEN    90  THRU    94
                   MOVE    95          TO  CNT-MAX
               WHEN    95  THRU    99
                   MOVE    100         TO  CNT-MAX
           END-EVALUATE
      *
           .
       210-MAIN-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療行為集計処理
      *****************************************************************
       220-MAIN-SRY-SEC            SECTION.
      *
      *    対象診療年月で検索
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    WRK-PARA-STRYMD(1:6)
                                   TO  SRY-SRYYM
           MOVE    WRK-PARA-ENDYMD(1:6)
                                   TO  SRY-CREYMD
           MOVE    "%"             TO  SRY-NYUGAIKBN
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key17"         TO  MCP-PATHNAME
               PERFORM 900-SRY-DBFETCH-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM UNTIL   FLG-SRYACT  =   1
      *        診療区分チェック
               PERFORM 230-MAIN-CHECK-SEC
               IF      FLG-SRYKBN      =   1
      *            診療会計集計処理
                   PERFORM 230-MAIN-ACCT-SEC
               END-IF
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key17"         TO  MCP-PATHNAME
               PERFORM 900-SRY-DBFETCH-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key17"         TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
            .
       220-MAIN-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分チェック処理
      *****************************************************************
       230-MAIN-CHECK-SEC           SECTION.
      *
           MOVE    0               TO  FLG-SRYKBN
           MOVE   SPACE            TO  WRK-SRYKBN
      *
           EVALUATE    WRK-PARA-SRYKBN
      *         WHEN    ""
               WHEN    "0"
               WHEN    "00"
               WHEN    SPACE
                   MOVE    1               TO  FLG-SRYKBN
               WHEN    "14"
               WHEN    "20"
               WHEN    "30"
               WHEN    "40"
               WHEN    "50"
               WHEN    "54"
               WHEN    "60"
               WHEN    "70"
               WHEN    "80"
                   MOVE    SRY-SRYKBN  TO  WRK-SRYKBN
      *
                   EVALUATE    TRUE
                       WHEN    ( WRK-SRYKBN (1:1)     =  "2" )
                           MOVE    "20"    TO  WRK-SRYKBN
                       WHEN    ( WRK-SRYKBN (1:1)     =  "3" )
                           MOVE    "30"    TO  WRK-SRYKBN
                       WHEN    ( WRK-SRYKBN =  "64" )
                           MOVE    "60"    TO  WRK-SRYKBN
                   END-EVALUATE
                   IF      WRK-PARA-SRYKBN     =   WRK-SRYKBN
                       MOVE    1               TO  FLG-SRYKBN
                   END-IF
           END-EVALUATE
            .
       230-MAIN-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計集計処理
      *****************************************************************
       230-MAIN-ACCT-SEC           SECTION.
      *
      *    該当の診療会計データを検索
           INITIALIZE                  SRYACCT-REC
           MOVE    SPA-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID        TO  ACCT-PTID
           MOVE    SRY-SRYKA       TO  ACCT-SRYKA
           MOVE    SRY-SRYYM       TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN      TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM      TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           MOVE    "tbl_sryacct"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 900-DBCLOSECURSOR-SEC
      *
      *----(04.08.03)--ADD-START---
           IF  ( ACCT-JIHOKBN        = "1" )   AND
               ( ACCT-TEIGENRATE     =  3 OR 4 )
               GO  TO  230-MAIN-ACCT-EXT
           END-IF
      *----(04.08.03)--ADD-END-----
      *
          IF     ( ACCT-ZAIKBN     NOT = 1 )   AND
                 ( ACCT-ZAIKBN     NOT = 2 )
               IF    ( ACCT-ZAITEN     =   ZERO )  AND
                    (( ACCT-SRYKBN     =   "21" )  OR
                     ( ACCT-SRYKBN     =   "22" )  OR
                     ( ACCT-SRYKBN     =   "23" )  OR
                     ( ACCT-SRYKBN     =   "14" ))
                   IF    ( SRY-SRYSYUKBN   =   "213")  OR
                         ( SRY-SRYSYUKBN   =   "223")  OR
                         ( SRY-SRYSYUKBN   =   "233")
                       MOVE    "1"         TO  WRK-INGAIKBN
                   ELSE
                       MOVE    "2"         TO  WRK-INGAIKBN
                   END-IF
               ELSE
                   MOVE    "1"         TO  WRK-INGAIKBN
               END-IF
      *
      *        院内
               IF    ( WRK-INGAIKBN            =   "1" )   AND
                     ( WRK-PARA-INGAIKBN   NOT =   1 )
                   PERFORM 240-MAIN-HEN-SEC
               END-IF
      *        院外
               IF    ( WRK-INGAIKBN            =   "2" )   AND
                     ( WRK-PARA-INGAIKBN   NOT =   0 )     AND
                     ( WRK-PARA-INGAIKBN   NOT =   SPACE )
                   PERFORM 240-MAIN-HEN-SEC
               END-IF
           END-IF
      *
           .
       230-MAIN-ACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主編集処理
      *****************************************************************
       240-MAIN-HEN-SEC            SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX     >   5 )
                   OR    ( SRY-SRYCD(IDX)  =   SPACE )
      *        薬剤を対象とする
               IF      SRY-SRYCD(IDX)(1:1)     =   "6"
                   PERFORM 250-TOKEI-HEN-SEC 
               ELSE
      *            残量廃棄分
                   IF    ( WRK-CYUSRYCD    NOT =   SPACE ) AND
                         ( SRY-SRYCD(IDX)      =   "099309901" )
                       PERFORM 250-TOKEI-CYU-HEN-SEC
                   END-IF
                   MOVE    SPACE       TO  WRK-CYUSRYCD
                   MOVE    ZERO        TO  WRK-CYUSURYO
               END-IF
           END-PERFORM
            .
       240-MAIN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計取得処理
      *****************************************************************
       20011-NYUINACCT-GET-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           PERFORM 900-NYUINACCT-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-NYUINACCT   NOT =   ZERO )
               IF    ( NACCT-ZAISKBKBN =   "5" )
                   INITIALIZE          SS009-AREA
                   MOVE    "1"         TO      SS009-EXGAIHAKU
                   CALL    "ORCSS009"  USING   SS009-AREA
                                               NYUINACCT-REC
                                               SPA-AREA
                   IF    ( SS009-RC    =   ZERO )
               PERFORM 250-TOKEI-BTNUM-HIDUKE-SEC
                   ELSE
                       DISPLAY "ERR SS009-RC:" SS009-RC
                   END-IF
               END-IF
               PERFORM 900-NYUINACCT-KEY9-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       20011-NYUINACCT-GET-EXT.
           EXIT.
      *****************************************************************
      *    統計編集処理（残量廃棄分）
      *****************************************************************
       250-TOKEI-CYU-HEN-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-SRYCD
           MOVE    ZERO            TO  WRK-SURYO
      *
           IF      WRK-PARA-SYORIKBN   NOT =   "1"
           PERFORM 250-TOKEI-HIDUKE-SEC
      *
           IF      WRK-KAISU   =   ZERO
               GO  TO  250-TOKEI-CYU-HEN-EXT
           END-IF
      *
           MOVE    WRK-CYUSRYCD    TO  WRK-SRYCD
           MOVE    WRK-CYUSURYO    TO  WRK-SURYO
      *
           PERFORM 250-TOKEI-SET-SEC
           PERFORM 250-TOKEI-GOKEI-SET-SEC
           ELSE
               MOVE    WRK-CYUSRYCD    TO  WRK-SRYCD
                  MOVE    WRK-CYUSURYO TO  WRK-SURYO
               PERFORM 20011-NYUINACCT-GET-SEC
           END-IF
      *
           .
       250-TOKEI-CYU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計編集処理
      *****************************************************************
       250-TOKEI-HEN-SEC           SECTION.
      *
           MOVE    ZERO            TO  WRK-CYUSURYO
           MOVE    SPACE           TO  WRK-CYUSRYCD
      *
           MOVE    1               TO  WRK-STR
           PERFORM VARYING IDX-D FROM  1 BY 1
                   UNTIL ( IDX-D >     31 )
               IF      ACCT-DAY(1 IDX-D)   NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-STR
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SRY-SRYCD(IDX)  TO  TNS-SRYCD
           MOVE    SRY-SRYYM       TO  TNS-YUKOSTYMD(1:6)
      *     MOVE    "01"            TO  TNS-YUKOSTYMD(7:2)
           MOVE    WRK-STR         TO  TNS-YUKOSTYMD(7:2)
      *    点数マスタ検索
           PERFORM 900-TNS-READ-SEC
      *
      *    点数がない場合は対象外
           IF      TNS-TEN             =   ZERO
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
      *    薬剤区分チェック
           MOVE    ZERO            TO  FLG-OK
           EVALUATE    WRK-PARA-YKZKBN
      *        全体
               WHEN    0
                   IF      TNS-YKZKBN      =   3
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        内服薬
               WHEN    1
                   IF      TNS-YKZKBN      =   1
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        注射薬
               WHEN    2
                   IF      TNS-YKZKBN      =   4
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        外用薬
               WHEN    3
                   IF      TNS-YKZKBN      =   6
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        歯科用薬剤
               WHEN    4
                   IF      TNS-YKZKBN      =   8
                       MOVE    1               TO  FLG-OK
                   END-IF
      *        歯科用特定薬剤
               WHEN    5
                   IF      TNS-YKZKBN      =   9
                       MOVE    1               TO  FLG-OK
                   END-IF
           END-EVALUATE
           IF      FLG-OK              =   ZERO
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
      *    残量廃棄チェック
           IF      TNS-YKZKBN      =   4
               MOVE    ZERO                TO  WRK-SURYO
               MOVE    SRY-SRYSURYO(IDX)   TO  WRK-SURYO
               IF      WRK-SURYO-S2        NOT =   ZERO
                   MOVE    ZERO            TO  WRK-SURYO-S1
                   COMPUTE WRK-CYUSURYO    =   1   -   WRK-SURYO
                   MOVE    SRY-SRYCD(IDX)  TO  WRK-CYUSRYCD
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-SRYCD
           MOVE    ZERO            TO  WRK-SURYO
      *
           IF      WRK-PARA-SYORIKBN   NOT =   "1"
           PERFORM 250-TOKEI-HIDUKE-SEC
      *
           IF      WRK-KAISU   =   ZERO
               GO  TO  250-TOKEI-HEN-EXT
           END-IF
      *
           MOVE    SRY-SRYCD(IDX)      TO  WRK-SRYCD
           MOVE    SRY-SRYSURYO(IDX)   TO  WRK-SURYO
      *
           PERFORM 250-TOKEI-SET-SEC
      *
           PERFORM 250-TOKEI-GOKEI-SET-SEC
           ELSE
           MOVE    SRY-SRYCD(IDX)      TO  WRK-SRYCD
           MOVE    SRY-SRYSURYO(IDX)   TO  WRK-SURYO
               PERFORM 20011-NYUINACCT-GET-SEC
           END-IF
      *

      *
             .
       250-TOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付処理
      *****************************************************************
       250-TOKEI-HIDUKE-SEC        SECTION.
      *
           MOVE    ZERO            TO  WRK-KAISU
      *
           IF      WRK-PARA-KIKANFLG   =   "1"
               MOVE    ACCT-ZAIKAISU   TO  WRK-KAISU
               GO  TO   250-TOKEI-HIDUKE-EXT
           END-IF
      *
           MOVE    WRK-PARA-STRYMD(7:2)    TO  WRK-STR
           MOVE    WRK-PARA-ENDYMD(7:2)    TO  WRK-END
      *
      *    同月内
           IF      WRK-PARA-STRYMD(1:6)    =   WRK-PARA-ENDYMD(1:6)
               PERFORM VARYING IDX-D FROM  WRK-STR BY 1
                       UNTIL ( IDX-D >     WRK-END )
                       OR    ( IDX-D >     31 )
                   IF      ACCT-DAY(1 IDX-D)   NOT =   ZERO
                       COMPUTE WRK-KAISU   =   WRK-KAISU
                                           +   ACCT-DAY(1 IDX-D)
                   END-IF
               END-PERFORM
           ELSE
               EVALUATE    ACCT-SRYYM
      *            開始月
                   WHEN    WRK-PARA-STRYMD(1:6)
                       PERFORM VARYING IDX-D FROM  WRK-STR BY 1
                               UNTIL   IDX-D >     31
                           IF      ACCT-DAY(1 IDX-D)   NOT =   ZERO
                               COMPUTE WRK-KAISU   =   WRK-KAISU
                                                   +   ACCT-DAY(1 IDX-D)
                           END-IF
                       END-PERFORM
      *            終了月
                   WHEN    WRK-PARA-ENDYMD(1:6)
                       PERFORM VARYING IDX-D FROM  1       BY 1
                               UNTIL   IDX-D >     WRK-END
                           IF      ACCT-DAY(1 IDX-D)   NOT =   ZERO
                               COMPUTE WRK-KAISU   =   WRK-KAISU
                                                   +   ACCT-DAY(1 IDX-D)
                           END-IF
                       END-PERFORM
      *            間の月
                   WHEN    OTHER
                       MOVE    ACCT-ZAIKAISU   TO  WRK-KAISU
               END-EVALUATE
           END-IF
      *
           .
       250-TOKEI-HIDUKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付処理
      *****************************************************************
       250-TOKEI-BTNUM-HIDUKE-SEC  SECTION.
      *
           MOVE    ZERO            TO  WRK-KAISU
      *
           IF      WRK-PARA-KIKANFLG   =   "1"
               MOVE    1                       TO  WRK-STR
               MOVE    31                      TO  WRK-END
           ELSE
               MOVE    WRK-PARA-STRYMD(7:2)    TO  WRK-STR
               MOVE    WRK-PARA-ENDYMD(7:2)    TO  WRK-END
           END-IF
      *
      *    同月内
           IF      WRK-PARA-STRYMD(1:6)    =   WRK-PARA-ENDYMD(1:6)
               PERFORM VARYING IDX-D FROM  WRK-STR BY 1
                       UNTIL ( IDX-D >     WRK-END )
                       OR    ( IDX-D >     31 )
                   PERFORM 250-TOKEI-BTUNUM-SET-SEC
               END-PERFORM
           ELSE
               EVALUATE    ACCT-SRYYM
      *            開始月
                   WHEN    WRK-PARA-STRYMD(1:6)
                       PERFORM VARYING IDX-D FROM  WRK-STR BY 1
                               UNTIL   IDX-D >     31
                           PERFORM 250-TOKEI-BTUNUM-SET-SEC
                       END-PERFORM
      *            終了月
                   WHEN    WRK-PARA-ENDYMD(1:6)
                       PERFORM VARYING IDX-D FROM  1       BY 1
                               UNTIL   IDX-D >     WRK-END
                           PERFORM 250-TOKEI-BTUNUM-SET-SEC
                       END-PERFORM
      *            間の月
                   WHEN    OTHER
                       PERFORM VARYING IDX-D FROM  1       BY 1
                               UNTIL   IDX-D >     31
                           PERFORM 250-TOKEI-BTUNUM-SET-SEC
                       END-PERFORM
               END-EVALUATE
           END-IF
      *
           .
       250-TOKEI-BTUNUM-HIDUKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       250-TOKEI-SET-SEC           SECTION.
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    WRK-NYUGAIKBN   TO  TOKEI-NYUGAIKBN
           MOVE    WRK-INGAIKBN    TO  TOKEI-INGAIKBN
           MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
           MOVE    TNS-KANANAME    TO  TOKEI-KANANAME
           MOVE    WRK-SRYCD       TO  TOKEI-SRYCD
           MOVE    ACCT-SRYKA      TO  TOKEI-SRYKA
           MOVE    TNS-TEN         TO  TOKEI-TEN
      *     MOVE    TNS-YUKOEDYMD   TO  TOKEI-ENDYMD
           PERFORM 900-TOKEI-READ-SEC
      *
           IF      FLG-TOKEI       =   1
               IF    ( PARA-KOUHATU    =   1 ) AND
                     ( TNS-KOUHATUKBN  =   1 )
                   STRING  "【後】"        DELIMITED BY SIZE
                          TNS-NAME         DELIMITED BY SIZE
                                   INTO    TOKEI-NAME
                   END-STRING
               ELSE
                   MOVE    TNS-NAME        TO  TOKEI-NAME
               END-IF
      *
               MOVE    TNS-TANINAME    TO  TOKEI-TANI
      *----(04.08.02)--ADD-START---
               MOVE    TNS-TENSIKIBETU TO  TOKEI-TENSIKIBETU
      *----(04.08.02)--ADD-END-----
      *
               MOVE    ZERO            TO  TOKEI-SRYSURYO
               COMPUTE WRK-GSURYO      =   WRK-SURYO
                                       *   WRK-KAISU
               COMPUTE TOKEI-SRYSURYO  =   TOKEI-SRYSURYO
                                       +   WRK-GSURYO
               MOVE    TNS-YUKOEDYMD   TO  TOKEI-ENDYMD
               IF    ( TOKEI-ENDYMD    <   WRK-PARA-ENDYMD )
                   MOVE    "*"         TO  TOKEI-KOME
                   MOVE    1               TO  FLG-KOME
               END-IF
      *
               WRITE   TOKEI-REC
               COMPUTE CNT-TOKEI   =   CNT-TOKEI   +   1
               IF    ( PARA-KOUHATU    =   1 ) AND
                     ( TNS-KOUHATUKBN  =   1 )
                   COMPUTE CNT-KOUHATU     =   CNT-KOUHATU   +   1
               END-IF
           ELSE
               COMPUTE WRK-GSURYO      =   WRK-SURYO
                                       *   WRK-KAISU
               COMPUTE TOKEI-SRYSURYO  =   TOKEI-SRYSURYO
                                       +   WRK-GSURYO
               IF      TNS-YUKOEDYMD   >   TOKEI-ENDYMD
                   MOVE    TNS-YUKOEDYMD   TO  TOKEI-ENDYMD
                   IF    ( TOKEI-ENDYMD    <   WRK-PARA-ENDYMD )
                       MOVE    "*"             TO  TOKEI-KOME
                       MOVE    1               TO  FLG-KOME
                   ELSE
                       MOVE    SPACE           TO  TOKEI-KOME
                       MOVE    ZERO            TO  FLG-KOME
                   END-IF
               END-IF
               REWRITE TOKEI-REC
           END-IF
      *
           .
       250-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       250-TOKEI-BTUNUM-SET-SEC    SECTION.
      *
           IF    ( SS009-BRMNUM-DAY(IDX-D)   =   SPACE )
            OR    ( ACCT-DAY (1 IDX-D)        =   ZERO )
               GO  TO   250-TOKEI-BTUNUM-SET-EXT
           END-IF
      *
      *    統計データ検索
           INITIALIZE                  TOKEI-REC
           MOVE    WRK-NYUGAIKBN   TO  TOKEI-NYUGAIKBN
           MOVE    WRK-INGAIKBN    TO  TOKEI-INGAIKBN
           MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
           MOVE    TNS-KANANAME    TO  TOKEI-KANANAME
           MOVE    WRK-SRYCD       TO  TOKEI-SRYCD
           MOVE    SS009-BRMNUM-DAY(IDX-D)(1:2)
                                   TO  TOKEI-SRYKA
           MOVE    TNS-TEN         TO  TOKEI-TEN
      *     MOVE    TNS-YUKOEDYMD   TO  TOKEI-ENDYMD
           PERFORM 900-TOKEI-READ-SEC
      *
           IF      FLG-TOKEI       =   1
               IF    ( PARA-KOUHATU    =   1 ) AND
                     ( TNS-KOUHATUKBN  =   1 )
                   STRING  "【後】"        DELIMITED BY SIZE
                          TNS-NAME         DELIMITED BY SIZE
                                   INTO    TOKEI-NAME
                   END-STRING
               ELSE
                   MOVE    TNS-NAME        TO  TOKEI-NAME
               END-IF
      *
               MOVE    TNS-TANINAME    TO  TOKEI-TANI
      *----(04.08.02)--ADD-START---
               MOVE    TNS-TENSIKIBETU TO  TOKEI-TENSIKIBETU
      *----(04.08.02)--ADD-END-----
      *
               MOVE    ZERO            TO  TOKEI-SRYSURYO
               COMPUTE WRK-GSURYO      =   WRK-SURYO
                                       *   ACCT-DAY (1 IDX-D)
               COMPUTE TOKEI-SRYSURYO  =   TOKEI-SRYSURYO
                                       +   WRK-GSURYO
               MOVE    TNS-YUKOEDYMD   TO  TOKEI-ENDYMD
               IF    ( TOKEI-ENDYMD    <   WRK-PARA-ENDYMD )
                   MOVE    "*"         TO  TOKEI-KOME
                   MOVE    1               TO  FLG-KOME
               END-IF
      *
               WRITE   TOKEI-REC
               COMPUTE CNT-TOKEI       =   CNT-TOKEI   +   1
               IF    ( PARA-KOUHATU    =   1 ) AND
                     ( TNS-KOUHATUKBN  =   1 )
                   COMPUTE CNT-KOUHATU     =   CNT-KOUHATU   +   1
               END-IF
           ELSE
               COMPUTE WRK-GSURYO      =   WRK-SURYO
                                       *   ACCT-DAY (1 IDX-D)
               COMPUTE TOKEI-SRYSURYO  =   TOKEI-SRYSURYO
                                       +   WRK-GSURYO
               IF      TNS-YUKOEDYMD   >   TOKEI-ENDYMD
                   MOVE    TNS-YUKOEDYMD   TO  TOKEI-ENDYMD
                   IF    ( TOKEI-ENDYMD    <   WRK-PARA-ENDYMD )
                       MOVE    "*"             TO  TOKEI-KOME
                       MOVE    1               TO  FLG-KOME
                   ELSE
                       MOVE    SPACE           TO  TOKEI-KOME
                       MOVE    ZERO            TO  FLG-KOME
                   END-IF
               END-IF
               REWRITE TOKEI-REC
           END-IF
      *
           IF      WRK-GSURYO   NOT =   ZERO
               PERFORM   250-TOKEI-BTUNUM-GOKEI-SET-SEC 
           END-IF
      *
           .
       250-TOKEI-BTUNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       250-TOKEI-GOKEI-SET-SEC           SECTION.
      *
      *    統計データ合計
           INITIALIZE                  TOKEI-REC
           MOVE    WRK-NYUGAIKBN   TO  TOKEI-NYUGAIKBN
           MOVE    WRK-INGAIKBN    TO  TOKEI-INGAIKBN
           MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
           MOVE    "合計"          TO  TOKEI-KANANAME
           MOVE    "999999999"     TO  TOKEI-SRYCD
           MOVE    "99999999"      TO  TOKEI-ENDYMD
           MOVE    ACCT-SRYKA      TO  TOKEI-SRYKA
           MOVE    "合計"          TO  TOKEI-NAME
           MOVE    ZERO            TO  TOKEI-SRYSURYO
           MOVE    ZERO            TO  TOKEI-TEN
           PERFORM 900-TOKEI-READ-SEC
           COMPUTE WRK-GSURYO      =   TNS-TEN *
                                    (  WRK-SURYO *   WRK-KAISU )
           COMPUTE TOKEI-SRYSURYO  =   TOKEI-SRYSURYO
                                   +   WRK-GSURYO
      *
           IF      FLG-TOKEI       =   1
               WRITE   TOKEI-REC
           ELSE
               REWRITE TOKEI-REC
           END-IF
      *
           .
       250-TOKEI-GOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計セット処理
      *****************************************************************
       250-TOKEI-BTUNUM-GOKEI-SET-SEC           SECTION.
      *
      *    統計データ合計
           INITIALIZE                  TOKEI-REC
           MOVE    WRK-NYUGAIKBN   TO  TOKEI-NYUGAIKBN
           MOVE    WRK-INGAIKBN    TO  TOKEI-INGAIKBN
           MOVE    TNS-YKZKBN      TO  TOKEI-YKZKBN
           MOVE    "合計"          TO  TOKEI-KANANAME
           MOVE    "999999999"     TO  TOKEI-SRYCD
           MOVE    "99999999"      TO  TOKEI-ENDYMD
           MOVE    SS009-BRMNUM-DAY(IDX-D)(1:2)
                                   TO  TOKEI-SRYKA
           MOVE    "合計"          TO  TOKEI-NAME
           MOVE    ZERO            TO  TOKEI-SRYSURYO
           MOVE    ZERO            TO  TOKEI-TEN
           PERFORM 900-TOKEI-READ-SEC
           COMPUTE WRK-GSURYO      =   TNS-TEN
                                   * ( WRK-SURYO *   ACCT-DAY (1 IDX-D))
           COMPUTE TOKEI-SRYSURYO  =   TOKEI-SRYSURYO
                                   +   WRK-GSURYO
      *
           IF      FLG-TOKEI       =   1
               WRITE   TOKEI-REC
           ELSE
               REWRITE TOKEI-REC
           END-IF
      *
           .
       250-TOKEI-GOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計印刷処理
      *****************************************************************
       260-TOKEI-SET-SEC           SECTION.
      *
      *    薬剤区分毎に印刷する
           INITIALIZE                  WRK-NAI-AREA
           MOVE    ZERO            TO  WRK-GSURYO
           MOVE    ZERO            TO  CNT-PAGE
           MOVE    ZERO            TO  IDY
      *
           MOVE    TOKEI-INGAIKBN  TO  WRK-INGAIKBN
           MOVE    TOKEI-YKZKBN    TO  WRK-YKZKBN
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           PERFORM UNTIL ( FLG-TOKEI           =   1  )
                   OR    ( TOKEI-INGAIKBN  NOT = WRK-INGAIKBN )
                   OR    ( TOKEI-YKZKBN    NOT = WRK-YKZKBN )
                   OR    ( TOKEI-NYUGAIKBN NOT = WRK-NYUGAIKBN )
      *        統計編集処理
               PERFORM 2601-TOUKEI-HEN-SEC
           END-PERFORM
      *
      *    印刷処理
           PERFORM 2602-PRINT-SEC
      *
             .
       260-TOKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計編集処理
      *****************************************************************
       2601-TOUKEI-HEN-SEC         SECTION.
      *
           ADD     1               TO  IDY
      *
      *    １枚２５明細とする
           IF      IDY             >   25
      *        印刷処理
               PERFORM 2602-PRINT-SEC
               MOVE    1               TO  IDY
               INITIALIZE                  WRK-NAI-AREA
           END-IF
      *
      *    薬剤毎の編集
           IF      TOKEI-SRYCD         NOT =   "999999999"
               MOVE    TOKEI-SRYCD         TO  WRK-NAI-SRYCD (IDY)
               MOVE    TOKEI-NAME          TO  WRK-NAI-NAME  (IDY)
               MOVE    TOKEI-TANI          TO  WRK-NAI-TANI  (IDY)
               MOVE    TOKEI-KOME          TO  WRK-NAI-KOME(IDY)
      *        点数編集
      *----(04.08.02)--UPD-START---
               IF      TOKEI-TENSIKIBETU   =   4
                   COMPUTE WRK-TEN     =   TOKEI-TEN   /   10
                   MOVE    WRK-TEN             TO  WRK-TENSU
                   MOVE    WRK-TENSU(2:10)     TO  WRK-HENTENSU
                   PERFORM 400-ZHEN-TEN-SET-SEC
                   MOVE    WRK-HENTEN          TO  WRK-NAI-TEN(IDY)
               ELSE
                   MOVE    TOKEI-TEN           TO  WRK-TEN
                   MOVE    WRK-TEN             TO  WRK-TENSU
                   MOVE    WRK-TENSU(1:10)     TO  WRK-HENTENSU
                   PERFORM 400-ZHEN-TEN-SET-SEC
                   MOVE    WRK-HENTEN          TO  WRK-NAI-TEN(IDY)
               END-IF
      *----(04.08.02)--UPD-START---
           ELSE
               MOVE    TOKEI-NAME          TO  WRK-NAI-NAME  (IDY)
           END-IF
      *
           MOVE    TOKEI-SRYCD         TO  WRK-SRYCD
           MOVE    TOKEI-TEN           TO  WRK-TEN
           MOVE    ZERO                TO  WRK-GSURYO
           PERFORM UNTIL ( FLG-TOKEI           =   1  )
                   OR    ( TOKEI-YKZKBN    NOT = WRK-YKZKBN )
                   OR    ( TOKEI-INGAIKBN  NOT = WRK-INGAIKBN )
                   OR    ( TOKEI-NYUGAIKBN NOT = WRK-NYUGAIKBN )
                   OR    ( TOKEI-SRYCD     NOT = WRK-SRYCD )
                   OR    ( TOKEI-TEN       NOT = WRK-TEN )
      *        数量編集処理
               PERFORM 26011-SURYOU-HEN-SEC
      *
      *        データ順読込み処理
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
           IF      WRK-SRYCD           NOT =   "999999999"
      *        科合計編集
               MOVE    WRK-GSURYO      TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-NAI-SURYO(IDY CNT-MAX)
      *        合計編集
               MOVE    ZERO            TO  WRK-KIN
               COMPUTE WRK-KIN         =   WRK-TEN
                                       *   WRK-GSURYO
               PERFORM 400-ZHEN-KIN-SET-SEC
               MOVE    WRK-HENKIN      TO  WRK-NAI-GAKU(IDY CNT-MAX)
           ELSE
      *        合計編集
               MOVE    WRK-GSURYO      TO  WRK-KIN
               PERFORM 400-ZHEN-KIN-SET-SEC
               MOVE    WRK-HENKIN      TO  WRK-NAI-GAKU(IDY CNT-MAX)
           END-IF
      *
           IF    ( PARA-KOUHATU    =   1 ) AND
                 ( FLG-TOKEI       =   1 )
      *        １枚２５明細とする
               IF      IDY             =   25
      *            印刷処理
                   PERFORM 2602-PRINT-SEC
                   MOVE    1               TO  IDY
                   INITIALIZE                  WRK-NAI-AREA
               END-IF
               MOVE    "後発品目割合"  TO  WRK-NAI-NAME  (25)
               MOVE    "％"            TO  WRK-NAI-TANI  (25)
               MOVE    0               TO  WRK-WARIAI
               COMPUTE WRK-WARIAI      =   CNT-KOUHATU
                                       /   CNT-TOKEI   *   100
      *----(04.08.02)--UPD-START---
               MOVE    WRK-WARIAI      TO  WRK-TENSU
               MOVE    WRK-TENSU(1:10) TO  WRK-HENTENSU
               PERFORM 400-ZHEN-TEN-SET-SEC
               MOVE    WRK-HENTEN      TO  WRK-NAI-TEN(25)
      *----(04.08.02)--UPD-END-----
           END-IF
      *
             .
       2601-TOUKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票内容テーブルセット
      *****************************************************************
       26011-SURYOU-HEN-SEC        SECTION.
      *
      *    科毎に数量を編集する
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   CNT-SRYKA
               IF      TOKEI-SRYKA     =   WRK-TAI-SRYKA(IDX)
                   IF      TOKEI-SRYCD     NOT =   "999999999"
      *                数量編集
                       MOVE    TOKEI-SRYSURYO  TO  WRK-SURYO
                       PERFORM 400-ZHEN-SURYO-SET-SEC
                       MOVE    WRK-HENSURYO    TO
                                               WRK-NAI-SURYO(IDY IDX)
      *                合計編集
      *----(04.08.02)--UPD-START---
                       IF      TOKEI-TENSIKIBETU   =   4
                           COMPUTE WRK-TEN     =   TOKEI-TEN   /   10
                       ELSE
                           MOVE    TOKEI-TEN       TO  WRK-TEN
                       END-IF
                       COMPUTE WRK-KIN         =   WRK-TEN
                                               *   TOKEI-SRYSURYO
      *----(04.08.02)--UPD-START---
                       PERFORM 400-ZHEN-KIN-SET-SEC
                       MOVE    WRK-HENKIN      TO  WRK-NAI-GAKU(IDY IDX)
                       MOVE    CNT-SRYKA       TO  IDX
                   ELSE
      *                合計編集
                       MOVE    TOKEI-SRYSURYO  TO  WRK-KIN
                       PERFORM 400-ZHEN-KIN-SET-SEC
                       MOVE    WRK-HENKIN      TO  WRK-NAI-GAKU(IDY IDX)
                       MOVE    CNT-SRYKA       TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    数量集計
           COMPUTE WRK-GSURYO      =   WRK-GSURYO
                                   +   TOKEI-SRYSURYO
      *
             .
       26011-SURYOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       2602-PRINT-SEC              SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
           MOVE    ZERO            TO  CNT-PAGE2
           MOVE    ZERO            TO  IDZ
      *
           PERFORM UNTIL   IDZ     =   CNT-MAX
      *        見出編集
               PERFORM 280-PRT-HEAD-SET-SEC
      *        明細編集
               PERFORM 290-PRT-BODY-SET-SEC
      *        印刷処理
               PERFORM 400-PRINT-OUT-SEC
           END-PERFORM
      *
      *    明細編集
           PERFORM VARYING CNT-LINE    FROM    1   BY   1
                   UNTIL ( CNT-LINE    >   25 )
                   OR    ( WRK-NAI-SRYCD(CNT-LINE) =   SPACE )
               PERFORM 270-CSV-BODY-SET-SEC
           END-PERFORM
      *
             .
       2602-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理見出し
      *****************************************************************
       280-PRT-HEAD-SET-SEC        SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           COMPUTE CNT-PAGE2       =   CNT-PAGE2   +   1
           COMPUTE CNT-GPAGE       =   CNT-GPAGE   +   1
      *
           MOVE    SPACE           TO  HCMG006
      *
      *    見出し
           EVALUATE    WRK-YKZKBN
               WHEN    "1"
                   MOVE    "（内服薬）　"  TO  WRK-YKZKBNWH
               WHEN    "4"
                   MOVE    "（注射薬）　"  TO  WRK-YKZKBNWH
               WHEN    "6"
                   MOVE    "（外用薬）　"  TO  WRK-YKZKBNWH
               WHEN    "8"
                   MOVE    "（歯科薬剤）"  TO  WRK-YKZKBNWH
               WHEN    "9"
                   MOVE    "（歯科特定）"  TO  WRK-YKZKBNWH
               WHEN    OTHER
                   MOVE    "＊＊＊＊"      TO  WRK-YKZKBNWH
           END-EVALUATE
           EVALUATE    WRK-INGAIKBN
               WHEN    "1"
                   MOVE    "（院内）"      TO  WRK-INGAIKBNWH
               WHEN    "2"
                   MOVE    "（院外）"      TO  WRK-INGAIKBNWH
           END-EVALUATE
      *
           IF      CNT-MAX         <=  5
               MOVE    CNT-PAGE        TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  WRK-PAGE2
           ELSE
               MOVE    CNT-PAGE        TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  WRK-PAGE1
               MOVE    CNT-PAGE2       TO  WRK-ZZ9
               MOVE    WRK-ZZ9         TO  WRK-PAGE2
               MOVE    "-"             TO  WRK-PAGE-MSG1
           END-IF
      *
           MOVE    WRK-DAI         TO  HCMG006-DAI
           MOVE    WRK-YMD         TO  HCMG006-YMD
           MOVE    WRK-PAGE        TO  HCMG006-PAGE
           MOVE    WRK-KIKAN       TO  HCMG006-KIKAN
      *
           MOVE    "診療行為コード"
                                   TO  HCMG006-TAI-CD
           MOVE    "名称"          TO  HCMG006-TAI-NAME
           MOVE    "単位"          TO  HCMG006-TAI-TANI
           MOVE    "薬価"          TO  HCMG006-TAI-TEN
      *
           IF      FLG-KOME        =   1
               MOVE    "*旧薬価"
                                       TO  HCMG006-LBL
           END-IF
      *
           .
       280-PRT-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理明細
      *****************************************************************
       290-PRT-BODY-SET-SEC        SECTION.
      *
      *    明細
           PERFORM VARYING CNT-LINE    FROM    1   BY   1
                   UNTIL ( CNT-LINE    >   25 )
               MOVE    WRK-NAI-SRYCD(CNT-LINE)
                                       TO  HCMG006-CD(CNT-LINE)
               MOVE    WRK-NAI-NAME(CNT-LINE)(1:40)
                                       TO  HCMG006-NAME(CNT-LINE)
               MOVE    WRK-NAI-TANI(CNT-LINE)
                                       TO  HCMG006-TANI(CNT-LINE)
               MOVE    WRK-NAI-TEN(CNT-LINE)
                                       TO  HCMG006-TEN(CNT-LINE)
               MOVE    WRK-NAI-KOME(CNT-LINE)
                                       TO  HCMG006-KOME(CNT-LINE)
           END-PERFORM
      *
      *    診療科別編集
           PERFORM VARYING IDX         FROM    1   BY  1
                   UNTIL ( IDX         >   5 )
                   OR    ( IDZ         =   CNT-MAX )
               COMPUTE IDZ         =   IDZ +   1
      *        科名編集
               MOVE    WRK-TAI-NAIYO(IDZ)  TO  HCMG006-TAI-NAIYO(IDX)
               IF      IDZ         =   CNT-MAX
                   MOVE    "合計"          TO  HCMG006-TAI-NAIYO(IDX)
               END-IF
      *        数量編集
               PERFORM VARYING CNT-LINE    FROM    1   BY   1
                       UNTIL   CNT-LINE    >   25
                   MOVE    WRK-NAI-SURYO(CNT-LINE IDZ)
                                       TO  HCMG006-SURYO(CNT-LINE IDX)
                   MOVE    WRK-NAI-GAKU(CNT-LINE IDZ)
                                       TO  HCMG006-GAKU(CNT-LINE IDX)
               END-PERFORM
           END-PERFORM
      *
           .
       290-PRT-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       270-CSV-HEAD-SET-SEC        SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
           MOVE    CSV-HEAD-01     TO  WRK-DATA
           MOVE    108             TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM VARYING IDZ         FROM    1   BY  1
                   UNTIL   IDZ         >   CNT-SRYKA
               MOVE    WRK-TAI-NAIYO(IDZ)  TO  WRK-DATA
               MOVE    14              TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    "数量,金額"     TO  WRK-DATA
               MOVE    9               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
           END-PERFORM
      *
           MOVE    "合計,数量,金額"    TO  WRK-DATA
           MOVE    14              TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ（データレコード）編集処理
      *****************************************************************
       270-CSV-BODY-SET-SEC        SECTION.
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
      *    作成年月日
           MOVE    WRK-PARA-STRYMD TO  WRK-SYMD
           MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
           MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
           MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
           MOVE    WRK-HEN-YMD-OT(1:7)
                                   TO  WRK-DATA
           MOVE    7               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    入院区分
           MOVE    WRK-PARA-NYUGAIKBN  TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    "0"
                   MOVE    "入・外"        TO  WRK-DATA
               WHEN    "1"
                   MOVE    "入院"          TO  WRK-DATA
               WHEN    "2"
                   MOVE    "外来"          TO  WRK-DATA
           END-EVALUATE
           MOVE    6               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    院外区分
           MOVE    WRK-INGAIKBN    TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           EVALUATE    WRK-INGAIKBN
               WHEN    "1"
                   MOVE    "院内"          TO  WRK-DATA
               WHEN    "2"
                   MOVE    "院外"          TO  WRK-DATA
           END-EVALUATE
           MOVE    4               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    薬剤区分
           MOVE    WRK-YKZKBN      TO  WRK-DATA
           MOVE    1               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           EVALUATE    WRK-YKZKBN
               WHEN    "1"
                   MOVE    "内服薬"        TO  WRK-DATA
               WHEN    "4"
                   MOVE    "注射薬"        TO  WRK-DATA
               WHEN    "6"
                   MOVE    "外用薬"        TO  WRK-DATA
               WHEN    "8"
                   MOVE    "歯科薬剤"      TO  WRK-DATA
               WHEN    "9"
                   MOVE    "歯科特定"      TO  WRK-DATA
               WHEN    OTHER
                   MOVE    "＊＊＊＊"      TO  WRK-DATA
           END-EVALUATE
           MOVE    8               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療行為コード
           MOVE    WRK-NAI-SRYCD(CNT-LINE)
                                   TO  WRK-DATA
           MOVE    9               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    薬剤名称
           MOVE    WRK-NAI-NAME(CNT-LINE)
                                   TO  WRK-DATA
           MOVE    200             TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    単位
           MOVE    WRK-NAI-TANI(CNT-LINE)
                                   TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    点数
           MOVE    WRK-NAI-TEN(CNT-LINE)
                                   TO  WRK-DATA
           MOVE    10              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *
           PERFORM VARYING IDZ         FROM    1   BY  1
                   UNTIL   IDZ         >   CNT-SRYKA
      *        科、病棟コード
               MOVE    WRK-TAI-SRYKA(IDZ)
                                       TO  WRK-DATA
               MOVE    2               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        数量
               MOVE    WRK-NAI-SURYO(CNT-LINE IDZ)
                                       TO  WRK-DATA
               MOVE    16              TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        金額
               MOVE    WRK-NAI-GAKU(CNT-LINE IDZ)
                                       TO  WRK-DATA
               MOVE    16              TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
           END-PERFORM
      *
      *    科、病棟コード
           MOVE    "99"            TO  WRK-DATA
           MOVE    2               TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    数量
           MOVE    WRK-NAI-SURYO(CNT-LINE CNT-MAX)
                                   TO  WRK-DATA
           MOVE    16              TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    金額
           MOVE    WRK-NAI-GAKU(CNT-LINE CNT-MAX)
                                   TO  WRK-DATA
           MOVE    16              TO  WRK-NAGASA
           MOVE    1               TO  WRK-RECEND
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                       DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           PERFORM 500-TOUKEICSV-SEC
      *
           .
       270-CSV-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       500-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "INS"           TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  STOUKEICSV-SHELLID
           MOVE    WRK-PARA-STRYMD(1:6)
                                   TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "診療科別医薬品使用量統計（合計）ＣＳＶ作成"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       500-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
               CONTINUE
           ELSE                   
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                   DELIMITED   BY  SIZE
                               WRK-DATA(1:IDW2)    DELIMITED   BY  SIZE
                               INTO    WRK-REC
                       END-STRING
                       ADD     IDW2        TO  WRK-REC-LENGTH        
                       MOVE    1           TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-RECEND         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING        
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF    
      *
           INITIALIZE                      WRK-HENSYU-AREA
           MOVE    ZERO                TO  WRK-NUM    
      *
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       20021-GETUMATU-GET-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       20021-GETUMATU-GET-EXT.
            EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       400-SEIWA-HEN-SEC           SECTION.
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
           INSPECT WRK-HENYMDG     REPLACING  ALL "  "  BY  "　"
      *
           .
       400-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-SURYO-SET-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    SPACE           TO  WRK-SURYO-X
      *
      *    点数Ｚ編集
           IF    ( WRK-SURYO-S1    =   ZERO )  AND
                 ( WRK-SURYO-S2    =   ZERO )
               GO  TO  400-ZHEN-SURYO-SET-EXT
           END-IF
      *
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
           MOVE    "."             TO  WRK-SURYO-Z2
           MOVE    WRK-SURYO-S2    TO  WRK-SURYO-Z3
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   16
               IF      WRK-SURYO-X(CNT-KETA2:1)    NOT =   SPACE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE    WRK-SURYO-X(CNT-KETA2:1)
                                           TO  WRK-HENSURYO(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-SURYO-SET-EXT.
           EXIT.
      *----(04.08.02)--UPD-START---
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-TEN-SET-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-HENTEN
           MOVE    ZERO            TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   10
               IF      WRK-HENTENSU(CNT-KETA2:1)   NOT =   SPACE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE  WRK-HENTENSU(CNT-KETA2:1)
                                       TO  WRK-HENTEN(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-TEN-SET-EXT.
           EXIT.
      *----(04.08.02)--UPD-END-----
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-KIN-SET-SEC        SECTION.
      *
           MOVE    SPACE           TO  WRK-HENKIN
           MOVE    SPACE           TO  WRK-KIN-X
      *
      *    点数Ｚ編集
           IF    ( WRK-KIN-S1      =   ZERO )  AND
                 ( WRK-KIN-S2      =   ZERO )
               GO  TO  400-ZHEN-KIN-SET-EXT
           END-IF
      *
           MOVE    WRK-KIN-S1      TO  WRK-KIN-Z1
           MOVE    "."             TO  WRK-KIN-Z2
           MOVE    WRK-KIN-S2      TO  WRK-KIN-Z3
      *
           MOVE    SPACE           TO  WRK-HENKIN
           MOVE    ZERO            TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   16
               IF      WRK-KIN-X(CNT-KETA2:1)    NOT =   SPACE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE    WRK-KIN-X(CNT-KETA2:1)
                                           TO  WRK-HENKIN(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-KIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-STRYMD(1:6)
                                       TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE   "HCMG006.red"        TO  SPRT-PRTID
           MOVE    "薬剤使用量統計（月報）"
                                       TO  SPRT-TITLE
           MOVE    HCMG006             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       400-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF      STS-RECEERR     =   ZERO
               CONTINUE
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
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG006 "     DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-TOKEI       =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG006 IN "      CNT-TOKEI
           DISPLAY "*** ORCBG006 INKA "    CNT-SRYKA
           DISPLAY "*** ORCBG006 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-GPAGE       TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療会計マスタ検索処理
      *****************************************************************
       900-ACCT-DBFETCH-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
       900-ACCT-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為マスタ検索処理
      *****************************************************************
       900-SRY-DBFETCH-SEC         SECTION.
      *
      *    入外区分チェック
           MOVE    ZERO            TO  FLG-OK
           PERFORM UNTIL  ( FLG-OK     =   1 )
                   OR     ( FLG-SRYACT =   1 )
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SRYACT
                   MOVE    MCPDATA-REC     TO  SRYACT-REC
                   MOVE    SPACE           TO  WRK-NYUGAIKBN
                   IF      WRK-PARA-NYUGAIKBN  =   "0"
                       MOVE    1               TO  FLG-OK
                   ELSE
                       MOVE    SRY-NYUGAIKBN   TO  WRK-NYUGAIKBN
                       IF      WRK-PARA-NYUGAIKBN  =   WRK-NYUGAIKBN
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SRYACT
               END-IF
           END-PERFORM
      *
           .
       900-SRY-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1           TO  FLG-TOKEI
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
      *
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC          SECTION.
      *
           READ    TOKEI-FILE      NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
      *
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索
      *****************************************************************
       900-TNS-READ-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
      *     MOVE    WRK-PARA-STRYMD TO  ORC-DBYMD
      *
           MOVE    TNS-SRYCD       TO  TTNS-CHK-SRYCD
           MOVE    TNS-YUKOSTYMD   TO  TTNS-CHK-YMD
      *     MOVE    ORC-DBYMD       TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF      TTNS-FLG        =   ZERO
               INITIALIZE                  TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD  TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                   MOVE    ZERO            TO  FLG-TENSU
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1               TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-DBCLOSECURSOR-SEC
           END-IF
      *
           .
       900-TNS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  TTNS-FLG
           INITIALIZE                  TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                   OR    ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD(TTNS-IDX)
                                           =   TTNS-CHK-SRYCD )    AND
                     ( TTNS-KEY-YUKOSTYMD(TTNS-IDX)
                                           <=  TTNS-CHK-YMD )      AND
                     ( TTNS-KEY-YUKOEDYMD(TTNS-IDX)
                                           >=  TTNS-CHK-YMD )
                   MOVE    1                   TO  TTNS-FLG
                   MOVE    TTNS-DAT(TTNS-IDX)  TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC           SECTION.
      *
           IF      TTNS-CONST-MAX  >   TTNS-MAX
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD(TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD(TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD(TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT(TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ診療科検索
      *****************************************************************
       900-SYS1005-DBSELECT-SEC    SECTION.
      *
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    WRK-PARA-STRYMD TO  SYS-1005-STYUKYMD
                                       SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           .
       900-SYS1005-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マス検索処理
      *****************************************************************
       900-SYS1005-DBFETCH-SEC     SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF     MCP-RC           =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           .
       900-SYS1005-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ病棟検索
      *****************************************************************
       900-SYS5001-DBSELECT-SEC    SECTION.
      *
           INITIALIZE                  SYS-5001-REC
           MOVE    "5001"          TO  SYS-5001-KANRICD
           MOVE    WRK-PARA-STRYMD TO  SYS-5001-STYUKYMD
                                       SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-5001-REC
           END-IF
      *
           .
       900-SYS5001-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     管理マス検索処理
      *****************************************************************
       900-SYS5001-DBFETCH-SEC     SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF     MCP-RC           =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYS-5001-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYS-5001-REC
           END-IF
      *
           .
       900-SYS5001-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY9-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-NYUINACCT
      *
           INITIALIZE                  NYUINACCT-REC
           MOVE    SPA-HOSPNUM     TO  NACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN   TO  NACCT-NYUGAIKBN
           MOVE    SRY-PTID        TO  NACCT-PTID
           MOVE    SRY-SRYYM       TO  NACCT-SRYYM
           MOVE    "5"             TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC   TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY9-FET-SEC  SECTION.
      *
           MOVE    "tbl_nyuinacct" TO  MCP-TABLE
           MOVE    "key9"          TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    ZERO            TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           .
       900-NYUINACCT-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-DBFETCH-SEC
           END-IF
      *
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       900-DBCLOSECURSOR-SEC       SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBCLOSECURSOR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
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
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
