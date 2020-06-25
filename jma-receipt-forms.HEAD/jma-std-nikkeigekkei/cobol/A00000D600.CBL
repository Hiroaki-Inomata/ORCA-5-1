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
      ******************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             A00000D600.
      ******************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次帳票
      *  コンポーネント名  : 日計表明細(診療費請求明細)
      *  管理者            :
      *  作成日付    作業者       記述
      *  04/02/24    @@@@-半田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者       日付      内容
      *  01.00.01    @@@@-門間    10/12/07  コメント履歴削除
      *                                     open-cobol1.0対応
      *  01.00.02    @@@@-田中    11/03/17  患者番号の編集処理を修正
      *  01.00.03    @@@@-田中    11/04/04  患者番号桁数を最大20桁に変更
      *  01.00.04    @@@@-門間    11/04/18  処理速度の改善、コメント履歴削除
      *  01.00.05    @@@@-田中    11/09/29  集計対象日を範囲指定に変更
      *  01.00.06    @@@@-田中    11/10/17  CSVファイル作成機能追加
      *  01.00.07    @@@@-田中    11/11/21  CSVファイル作成領域を修正
      *  01.00.08    @@@@-田中    12/01/10  CSV合計計算処理を修正
      *  01.00.09    @@@@-田中    12/02/02  公務災害（健保準拠）時の点数単価不具合を修正
      *  01.00.10    @@@@-門間    12/12/06  公費単独の場合、項目「自費負担額」に
      *                                     公費１計算負担金を参照するように修正
      *  02.00.01    @@@@-門間    14/10/31  ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.02    門間         16/02/01  一時ディレクトリ対応(エラーファイル)
      ******************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
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
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      **    エラーファイル 名称領域
           COPY    "CPRECEERR.INC".
      *
           COPY    "A00000D600V02.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR               PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-GET                   PIC 9(01).
           03  FLG-END                   PIC 9(01).
           03  FLG-SYSKANRI              PIC 9(01).
           03  FLG-SYSKANRI2             PIC 9(01).
           03  FLG-INPUT                 PIC 9(01).
           03  FLG-HKNCOMBI              PIC 9(01).
           03  FLG-PTINF                 PIC 9(01).
           03  FLG-PTHKNINF              PIC 9(01).
           03  FLG-PTRSIINF              PIC 9(01).
           03  FLG-SYUMEI                PIC 9(01).
           03  FLG-SYUNOU                PIC 9(01).
           03  FLG-HKNNUM                PIC 9(01).
           03  FLG-CSV-HEAD              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                  PIC 9(02).
           03  CNT-PAGE                  PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                PIC 9(02).
               05  SYS-MM                PIC 9(02).
               05  SYS-DD                PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                       PIC 9(04).
           03  IDY                       PIC 9(04).
           03  IDZ                       PIC 9(04).
           03  IDX-S                     PIC 9(05).
           03  IDX-N                     PIC 9(05).
           03  IDX-MAX                   PIC 9(05).
           03  IDX-SUM                   PIC 9(04).
      *
           03  IDX1                      PIC 9(05).
           03  IDX2                      PIC 9(04).
           03  IDX3                      PIC 9(02).
           03  IDX4                      PIC 9(02).
           03  JIHIIDX                   PIC 9(04).
      *
           03  IDX-NG                    PIC 9(01).
           03  IDX-DAY                   PIC 9(02).
           03  IDX-STDAY                 PIC 9(02).
           03  IDX-EDDAY                 PIC 9(02).
           03  IDXP                      PIC 9(05).
           03  IDX-ST                    PIC 9(03).
           03  IDX-ED                    PIC 9(03).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID            PIC 9(07).
           03  WRK-PARA-SHELLID          PIC X(08).
           03  WRK-PARA-HOSPNUM          PIC 9(02).
           03  WRK-PARA-DENPSTYMD        PIC X(08).
           03  WRK-PARA-DENPEDYMD        PIC X(08).
           03  WRK-PARA-SORTKBN          PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PATH                  PIC X(64).
      *
           03  WRK-RECEERR               PIC X(200).
      *
           03  WRK-PARA-DENPSTYMDWH      PIC X(22).
           03  WRK-PARA-DENPEDYMDWH      PIC X(22).
           03  WRK-SYSYMDWH              PIC X(22).
           03  WRK-RENNUM                PIC 9(03).
           03  WRK-PTNUM                 PIC X(20).
      *
           03  WRK-SYSYMD.
               05  WRK-SYSYY             PIC 9(04).
               05  WRK-SYSMM             PIC 9(02).
               05  WRK-SYSDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY               PIC 9(04).
               05  WRK-SMM               PIC 9(02).
               05  WRK-SDD               PIC 9(02).
           03  WRK-HENYMDG               PIC X(22).
           03  WRK-BIRTHDAY.
            05 WRK-BIRTHY                PIC 9(04).
            05 WRK-BIRTHM                PIC 9(02).
            05 WRK-BIRTHD                PIC 9(02).
           03  WRK-NENREI                PIC 9(03).
           03  WRK-NYUGAIKBN             PIC X(01).
           03  WRK-SRYKA                 PIC X(02).
           03  WRK-FTNRATE               PIC 9(01)V9(02).
           03  WRK-KYFRATE               PIC 9(01)V9(02).
      *
           03  WRK-SYUHKN-KBN            PIC 9(01).
           03  WRK-MONEY                 PIC 9(08).
           03  WRK-JIHI                  PIC 9(08).
           03  WRK-SYUEKI                PIC 9(08).
           03  WRK-JIHI2                 PIC 9(08).
           03  WRK-JIHI3                 PIC 9(08).
           03  WRK-JIHI4                 PIC 9(08).
           03  WRK-4001-TENTANKA         PIC 9(02)V9(01).
           03  WRK-4001-KSNMONEY         PIC 9(08).
           03  WRK-4001-KOMUFLG          PIC X(01).
           03  WRK-4001-KOMUTENTANKA     PIC 9(02).
           03  WRK-TENTNK                PIC 9(02)V9(01).
           03  WRK-RSI-MONEY1            PIC S9(09)V9(01).
           03  WRK-RSI-MONEY2            PIC S9(09)V9(01).
      *
           03  WRK-RENNUM-X              PIC ZZ9.
           03  WRK-PAGE                  PIC ZZ9.
           03  WRK-Z9                    PIC ZZZZZZ9.
           03  WRK-ZZ                    PIC ZZZZZZZ.
           03  WRK-ZZ9                   PIC ZZZZZZZ9.
           03  WRK-ZZZ                   PIC ZZZZZZZZ.
           03  WRK-S9                    PIC -------9.
           03  WRK-SZ                    PIC -------Z.
           03  WRK-SZ9                   PIC --------9.
           03  WRK-SZZ                   PIC --------Z.
           03  WRK-Z9-3                  PIC ZZ9.
           03  WRK-Z9-7                  PIC ZZZZZZ9.
           03  WRK-ZZ-7                  PIC ZZZZZZZ.
           03  WRK-Z9-8                  PIC ZZZZZZZ9.
           03  WRK-S9-8                  PIC -------9.
           03  WRK-SZ-8                  PIC -------Z.
      *
       01  SUM-AREA                      VALUE ZERO.
           03  SUM-HKNTEN                PIC 9(08).
           03  SUM-HKNKEN                PIC 9(08).
           03  SUM-OCC                   OCCURS 14.
               05  SUM-MONEY             PIC 9(08).
               05  SUM-KEN               PIC 9(08).
           03  SUM-SEIKYU                PIC S9(08).
           03  SUM-RYOSYU                PIC S9(08).
           03  SUM-SEIKYUKEN             PIC 9(08).
           03  SUM-RYOSYUKEN             PIC 9(08).
      *
       01  SUM-G-AREA                    VALUE ZERO.
           03  SUM-G-HKNTEN              PIC 9(09).
           03  SUM-G-OCC                 OCCURS 16.
             05  SUM-G-TEN               PIC 9(09).
           03  SUM-G-HKNFTN              PIC S9(09).
           03  SUM-G-JIHIFTN             PIC S9(09).
           03  SUM-G-SEIKYU              PIC S9(09).
           03  SUM-G-RYOSYU              PIC S9(09).
      *
       01  WRK-SMEI-REC.
           03  WRK-SMEI-TBL              OCCURS 10000.
             05  WRK-SMEI-HOSPNUM        PIC 9(02).
             05  WRK-SMEI-PTID           PIC 9(10).
             05  WRK-SMEI-SORT-KEY.
               07  WRK-SMEI-NYUGAIKBN    PIC X(01).
               07  WRK-SMEI-SRYKA        PIC X(02).
               07  WRK-SMEI-DENPNUM      PIC 9(07).
               07  WRK-SMEI-DENPEDANUM   PIC 9(02).
             05  WRK-SMEI-SORT-KEY2.
               07  WRK-SMEI-NYUGAIKBN2   PIC X(01).
               07  WRK-SMEI-SRYKA2       PIC X(02).
               07  WRK-SMEI-PTNUM        PIC X(20).
             05  WRK-SMEI-SKYMONEY       PIC S9(07).
             05  WRK-SMEI-NYUHEN-MONEY   PIC S9(07).
             05  WRK-SMEI-NYUHEN-YMD     PIC X(08).
           03  WRK-SV-SMEI-TBL           PIC X(69).
      *
       01  WRK-CONS-AREA.
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE "973".
      *    公害保険番号
           03  WRK-CONS-KOGAI-HKNNUM   PIC X(03)   VALUE "975".
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-CSV-AREA.
           03  WRK-ITEM-SIZE           PIC 9(03)    VALUE 100.
           03  WRK-ITEM                PIC X(100)   VALUE SPACE.
           03  WRK-CSVDATA             PIC X(20000) VALUE SPACE.
      *
       01  CSV-G-AREA                  VALUE ZERO.
           03  CSV-G-HKNTEN            PIC 9(09).
           03  CSV-G-OCC               OCCURS 16.
             05  CSV-G-TEN             PIC 9(09).
           03  CSV-G-HKNFTN            PIC S9(09).
           03  CSV-G-JIHIFTN           PIC S9(09).
           03  CSV-G-SEIKYU            PIC S9(09).
           03  CSV-G-RYOSYU            PIC S9(09).
           03  CSV-MONEY               PIC S9(08).
      *
       01  CSV-HEAD-AREA.
           03  CSV-HEAD-G.
             05  CSV-HEAD-VAL.
               07  CSV-HEAD-DENPNUM    PIC X(50)  VALUE "伝票番号".
               07  CSV-HEAD-PTNUM      PIC X(50)  VALUE "患者番号".
               07  CSV-HEAD-PTNAME     PIC X(50)  VALUE "患者氏名".
               07  CSV-HEAD-SEX        PIC X(50)  VALUE "性別".
               07  CSV-HEAD-AGE        PIC X(50)  VALUE "年齢".
               07  CSV-HEAD-HONKZK     PIC X(50)  VALUE "本/家".
               07  CSV-HEAD-HKNNAME    PIC X(50)  VALUE "保険種類".
               07  CSV-HEAD-HKNTEN     PIC X(50)  VALUE "保険点数".
               07  CSV-HEAD-SSU        PIC X(50)  VALUE "初・再診料".
               07  CSV-HEAD-SDO        PIC X(50)  VALUE "医学管理等".
               07  CSV-HEAD-ZTK        PIC X(50)  VALUE "在宅医療".
               07  CSV-HEAD-TYK        PIC X(50)  VALUE "投薬".
               07  CSV-HEAD-CSY        PIC X(50)  VALUE "注射".
               07  CSV-HEAD-SYC        PIC X(50)  VALUE "処置".
               07  CSV-HEAD-SJT        PIC X(50)  VALUE "手術".
               07  CSV-HEAD-MSI        PIC X(50)  VALUE "麻酔".
               07  CSV-HEAD-KNS        PIC X(50)  VALUE "検査".
               07  CSV-HEAD-GZU        PIC X(50)  VALUE "画像診断".
               07  CSV-HEAD-ETC        PIC X(50)
                                       VALUE "リハビリテーション".
               07  CSV-HEAD-SSN        PIC X(50)
                                       VALUE "精神科専門療法".
               07  CSV-HEAD-HOU        PIC X(50)  VALUE "放射線治療".
               07  CSV-HEAD-BYR        PIC X(50)  VALUE "病理診断".
               07  CSV-HEAD-NYN        PIC X(50)  VALUE "入院料".
               07  CSV-HEAD-RYO        PIC X(50)  VALUE "療養担当手当".
               07  CSV-HEAD-KYUFU      PIC X(50)  VALUE "保険給付額".
               07  CSV-HEAD-JIHIFTN    PIC X(50)  VALUE "自費負担額".
               07  CSV-HEAD-SKYMONEY   PIC X(50)  VALUE "窓口請求額".
               07  CSV-HEAD-RSYMONEY   PIC X(50)  VALUE "窓口領収額".
             05  CSV-HEAD-R            REDEFINES CSV-HEAD-VAL.
               07  CSV-HEAD-ITEM       PIC X(50)  OCCURS 28.
             05  CSV-HEAD-MAX          PIC 9(05)  VALUE 28.
      *
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
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    自費管理情報
           COPY    "CPSK1013.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *
      *    患者保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者労災マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細マスタ
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
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
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC  UNTIL  FLG-END  =  1
      *
           PERFORM  300-END-SEC
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
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-SMEI-REC
           INITIALIZE                  SUM-G-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  CSV-G-AREA
           INITIALIZE                  RECEERR
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
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-DENPSTYMD
                                               WRK-PARA-DENPEDYMD
                                               WRK-PARA-SORTKBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "A00000D600"    TO  JOB-PGID
           MOVE    "日計表明細（診療請求明細）"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           INITIALIZE                     SGETTEMP-AREA
           MOVE    RECEERR                TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"          USING  SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)  TO  RECEERR
      *
      *    入力パラチェック
           IF  WRK-PARA-DENPEDYMD  =  SPACE
             MOVE    WRK-PARA-DENPSTYMD  TO  WRK-PARA-DENPEDYMD
           END-IF
           IF   WRK-PARA-DENPSTYMD  >  WRK-PARA-DENPEDYMD
             MOVE    "集計範囲が不正です"    TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
           IF   WRK-PARA-DENPSTYMD(1:6)  NOT =   WRK-PARA-DENPEDYMD(1:6)
             MOVE    "集計範囲は同月で指定してください"  TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
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
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    対象期間セット
           MOVE    WRK-PARA-DENPSTYMD(7:2)   TO  IDX-STDAY
           MOVE    WRK-PARA-DENPEDYMD(7:2)   TO  IDX-EDDAY
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD   TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  WRK-SYSYMDWH
      *
      *    対象期間開始年月日編集
           MOVE    WRK-PARA-DENPSTYMD    TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  WRK-PARA-DENPSTYMDWH
      *
      *    対象期間終了年月日編集
           MOVE    WRK-PARA-DENPEDYMD    TO  WRK-SYMD
           PERFORM  31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG           TO  WRK-PARA-DENPEDYMDWH
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE                 TO  SYS-1001-REC
           INITIALIZE                    SYS-1001-REC
           MOVE    "1001"                TO  SYS-1001-KANRICD
           MOVE    "*"                   TO  SYS-1001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-1001-HOSPNUM
           MOVE    WRK-PARA-DENPSTYMD    TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-DENPEDYMD    TO  SYS-1001-STYUKYMD
           MOVE    SYS-1001-REC          TO  MCPDATA-REC
           PERFORM  800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI  =  ZERO
             MOVE    MCPDATA-REC         TO  SYS-1001-REC
             MOVE    SYS-1001-HOSPNUM    TO  WRK-PARA-HOSPNUM
           ELSE
             MOVE    "医療機関ＩＤが取得できませんでした。"
                                         TO  WRK-RECEERR
             PERFORM  500-ERR-HENSYU-SEC
           END-IF
      *
      *    労災医療機関情報
           MOVE    SPACE                 TO  SYS-4001-REC
           INITIALIZE                    SYS-4001-REC
           MOVE    "4001"                TO  SYS-4001-KANRICD
           MOVE    "*"                   TO  SYS-4001-KBNCD
           MOVE    SPA-HOSPNUM           TO  SYS-4001-HOSPNUM
           MOVE    WRK-PARA-DENPSTYMD    TO  SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-DENPEDYMD    TO  SYS-4001-STYUKYMD
           MOVE    SYS-4001-REC          TO  MCPDATA-REC
           PERFORM  800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE  MCPDATA-REC           TO  SYS-4001-REC
             EVALUATE  SYS-4001-TENKBN
               WHEN  "1"
                 MOVE    11.5            TO  WRK-4001-TENTANKA
               WHEN  "2"
                 MOVE    12              TO  WRK-4001-TENTANKA
               WHEN  OTHER
                 MOVE    10              TO  WRK-4001-TENTANKA
             END-EVALUATE
      *
             IF  SYS-4001-KOMUKBN  =  "2"
      *    公務災害(健保基準)時の点数単価設定
               MOVE    SYS-4001-KOMUTENTANKA-X TO  WRK-4001-KOMUTENTANKA
      *    公務災害区分が健保基準の場合、１を格納
               MOVE    "1"        TO  WRK-4001-KOMUFLG
             END-IF
           ELSE
             MOVE    10         TO  WRK-4001-TENTANKA
           END-IF
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
           PERFORM  310-DATA-SELECT-SEC
      *
           PERFORM  UNTIL  FLG-END  =  1
      *    帳票編集<明細>処理
             PERFORM  320-BODY-HEN-SEC
      *    帳票印刷処理
             IF  FLG-GET = 1
               PERFORM  390-PRINT-OUT-SEC
      *
               IF  WRK-SMEI-NYUGAIKBN(IDX-S)  NOT =  WRK-NYUGAIKBN
                   OR  WRK-SMEI-SRYKA(IDX-S)  NOT =  WRK-SRYKA
      *    CSV合計編集処理
                 PERFORM  3001-CSV-GOUKEI-SET-SEC
                 MOVE    ZERO     TO  FLG-CSV-HEAD
               END-IF
      *    CSV出力処理
               PERFORM  230-TOUKEICSV-SEC
             END-IF
           END-PERFORM
      *
      *    帳票印刷処理
           IF  FLG-GET  =  ZERO
             PERFORM  310-HEAD-HEN-SEC
             MOVE    "＊＊＊  対象データが１件も存在しません。  ＊＊＊"
                           TO   D600-JOBMSG
             PERFORM  390-PRINT-OUT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象データ選定処理
      *****************************************************************
       310-DATA-SELECT-SEC          SECTION.
      *
           MOVE     ZERO        TO    IDX-S
      *
      *    外来・入院患者を取得
           PERFORM  VARYING  IDX-NG  FROM  1  BY 1
                    UNTIL  IDX-NG  >  2
      *
             PERFORM  VARYING  IDX-DAY  FROM  IDX-STDAY  BY 1
                      UNTIL  IDX-DAY  >  IDX-EDDAY
               PERFORM  910-DBSELECT-SEC
               PERFORM  UNTIL  FLG-SYUMEI  =  1
                 PERFORM  900-SYUNOU-READ-SEC
      *
                 IF      FLG-SYUNOU            =  ZERO
                   AND   SYU-DENPJTIKBN    NOT =  "3"
                   AND ((SMEI-JOUTAIKBN    NOT =  "8"
                   AND   SMEI-JOUTAIKBN    NOT =  "3")
                    OR   SMEI-SKYMONEY     NOT = 0
                    OR   SMEI-NYUHEN-MONEY NOT = 0)
      *
                   PERFORM  900-PTNUM-READ-SEC
      *
                   ADD   1                TO  IDX-S
                   MOVE  SMEI-HOSPNUM     TO  WRK-SMEI-HOSPNUM(IDX-S)
                   MOVE  SMEI-NYUGAIKBN   TO  WRK-SMEI-NYUGAIKBN(IDX-S)
                   MOVE  SMEI-SRYKA       TO  WRK-SMEI-SRYKA(IDX-S)
                   MOVE  SMEI-NYUGAIKBN   TO  WRK-SMEI-NYUGAIKBN2(IDX-S)
                   MOVE  SMEI-SRYKA       TO  WRK-SMEI-SRYKA2(IDX-S)
                   MOVE  SMEI-PTID        TO  WRK-SMEI-PTID(IDX-S)
                   MOVE  SMEI-DENPNUM     TO  WRK-SMEI-DENPNUM(IDX-S)
                   MOVE  SMEI-DENPEDANUM  TO  WRK-SMEI-DENPEDANUM(IDX-S)
                   MOVE  SMEI-SKYMONEY    TO  WRK-SMEI-SKYMONEY(IDX-S)
                   MOVE  SMEI-NYUHEN-MONEY
                                        TO  WRK-SMEI-NYUHEN-MONEY(IDX-S)
                   MOVE  SMEI-NYUHEN-YMD  TO  WRK-SMEI-NYUHEN-YMD(IDX-S)
                   MOVE  PTNUM-PTNUM      TO  WRK-SMEI-PTNUM(IDX-S)
                 END-IF
                 PERFORM   910-DBFETCH-SEC
               END-PERFORM
               PERFORM   910-DBCLOSE-SEC
             END-PERFORM
           END-PERFORM
      *
      *    ソート処理
           MOVE IDX-S TO IDX-MAX
      *
           IF   IDX-MAX     >    1
             MOVE  1  TO  FLG-GET
             MOVE  ZERO    TO   FLG-END
             MOVE 1  TO  IDX-S
             MOVE 2  TO  IDX-N
             PERFORM UNTIL IDX-S NOT  <  IDX-MAX
              PERFORM  UNTIL IDX-N  >  IDX-MAX
                IF  WRK-PARA-SORTKBN   =   "1"
                  IF  WRK-SMEI-SORT-KEY2(IDX-S)  >
                      WRK-SMEI-SORT-KEY2(IDX-N)
                    MOVE  WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                    MOVE  WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                    MOVE  WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                  END-IF
                ELSE
                  IF  WRK-SMEI-SORT-KEY(IDX-S)
                               >   WRK-SMEI-SORT-KEY(IDX-N)
                    MOVE  WRK-SMEI-TBL(IDX-N)  TO   WRK-SV-SMEI-TBL
                    MOVE  WRK-SMEI-TBL(IDX-S)  TO   WRK-SMEI-TBL(IDX-N)
                    MOVE  WRK-SV-SMEI-TBL      TO   WRK-SMEI-TBL(IDX-S)
                  END-IF
                END-IF
                ADD 1  TO  IDX-N
              END-PERFORM
              ADD 1  TO  IDX-S
              COMPUTE IDX-N = IDX-S + 1
             END-PERFORM
           ELSE
             IF   IDX-MAX     =     ZERO
               MOVE  ZERO    TO   FLG-GET
               MOVE  1            TO   FLG-END
             ELSE
               MOVE  1            TO  FLG-GET
               MOVE  ZERO    TO   FLG-END
             END-IF
           END-IF
      *
           MOVE 1     TO   IDX-S
      *
           .
       310-DATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
      *
           INITIALIZE                  D600
      *
      *    システム日付
           MOVE    WRK-SYSYMDWH        TO  D600-SYSYMD
      *
      *    対象日
           IF  WRK-PARA-DENPSTYMD  =  WRK-PARA-DENPEDYMD
             MOVE     WRK-PARA-DENPSTYMDWH    TO  D600-NYUHEN-YMD
           ELSE
             STRING    WRK-PARA-DENPSTYMDWH   DELIMITED  BY  SIZE
                       "　〜　"               DELIMITED  BY  SIZE
                       WRK-PARA-DENPEDYMDWH   DELIMITED  BY  SIZE
                                              INTO  D600-NYUHEN-YMD
             END-STRING
           END-IF
      *
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  D600-PAGE
      *
           EVALUATE  WRK-NYUGAIKBN
              WHEN "1"
                   MOVE "入院"         TO  D600-NYUGAIKBN
      *
              WHEN "2"
                   MOVE "外来"         TO  D600-NYUGAIKBN
      *
           END-EVALUATE
           MOVE    "入院料"            TO   D600-NYUINRYO
      *
      *    診療科名称取得
           INITIALIZE                       SYS-1005-REC
           MOVE    "1005"               TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO  SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
           MOVE    WRK-PARA-DENPSTYMD   TO  SYS-1005-EDYUKYMD
           MOVE    WRK-PARA-DENPEDYMD   TO  SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           PERFORM  800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI2        =   ZERO
             MOVE  MCPDATA-REC        TO  SYS-1005-REC
             MOVE  SYS-1005-SRYKANAME TO  D600-SRYKA
           ELSE
             MOVE  SPACE              TO  D600-SRYKA
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           MOVE    ZERO               TO  CNT-LINE
      *
           INITIALIZE                 SUM-G-AREA
      *
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  WRK-NYUGAIKBN
           MOVE    WRK-SMEI-SRYKA(IDX-S)       TO  WRK-SRYKA
      *
      *    帳票編集<見出し>処理
           PERFORM  310-HEAD-HEN-SEC
      *
      *    CSVヘッダ編集処理
           IF  FLG-CSV-HEAD  =  ZERO
             PERFORM  800-CSV-HEAD-SEC
             MOVE    1         TO  FLG-CSV-HEAD
           END-IF
      *
           PERFORM   UNTIL   FLG-END     =   1
                     OR      CNT-LINE    =   21
      *    収納テーブル再読み込み
             PERFORM 901-SYUNOU-READ-SEC
             IF    FLG-SYUNOU = ZERO
      *
      *    入外区分ブレイクチェック処理
               IF  WRK-SMEI-NYUGAIKBN(IDX-S)  NOT =  WRK-NYUGAIKBN
      *
                 PERFORM  3001-SYOUKEI-SET-SEC
      *
      *    帳票編集<見出し>処理
                 PERFORM  390-PRINT-OUT-SEC
      *
      *    CSV合計編集処理
                 PERFORM  3001-CSV-GOUKEI-SET-SEC
      *    CSV出力処理
                 PERFORM  230-TOUKEICSV-SEC
      *
                 INITIALIZE                          SUM-G-AREA
      *
                 MOVE    ZERO                      TO  CNT-LINE
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *    帳票編集<見出し>処理
                 PERFORM  310-HEAD-HEN-SEC
      *    CSVヘッダ編集処理
                 PERFORM  800-CSV-HEAD-SEC
               END-IF
      *
      *    診療科ブレイクチェック処理
               IF   WRK-SMEI-SRYKA(IDX-S)  NOT =  WRK-SRYKA
      *
                 PERFORM  3001-SYOUKEI-SET-SEC
      *
      *    帳票編集<見出し>処理
                 PERFORM  390-PRINT-OUT-SEC
      *
      *    CSV合計編集処理
                 PERFORM  3001-CSV-GOUKEI-SET-SEC
      *    CSV出力処理
                 PERFORM  230-TOUKEICSV-SEC
      *
                 INITIALIZE                        SUM-G-AREA
      *
                 MOVE    ZERO                      TO  CNT-LINE
                 MOVE    WRK-SMEI-NYUGAIKBN(IDX-S) TO  WRK-NYUGAIKBN
                 MOVE    WRK-SMEI-SRYKA(IDX-S)     TO  WRK-SRYKA
      *    帳票編集<見出し>処理
                 PERFORM  310-HEAD-HEN-SEC
      *    CSVヘッダ編集処理
                 PERFORM  800-CSV-HEAD-SEC
               END-IF
      *
      *    患者情報読み込み
               INITIALIZE                        PTINF-REC
               MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTINF-HOSPNUM
               MOVE    WRK-SMEI-PTID(IDX-S)      TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *
               ADD     1                   TO  CNT-LINE
      *    伝票番号
               MOVE    WRK-SMEI-DENPNUM(IDX-S)
                             TO  D600-RENNUM(CNT-LINE)
      *    患者番号
               INITIALIZE   ORCSPTIDAREA
               MOVE    WRK-PARA-HOSPNUM        TO  SPTID-HOSPNUM
               MOVE    WRK-SMEI-PTID(IDX-S)    TO  SPTID-PTID
               CALL "ORCSPTID"  USING ORCSPTIDAREA
                                      SPA-AREA
               IF  SPTID-RC = ZERO
                 IF  SPTID-PTNUM(11:)  =  SPACE
                   MOVE    SPTID-PTNUM    TO  D600-PTNUM(CNT-LINE)
                 ELSE
                   MOVE    SPTID-PTNUM    TO  D600-PTNUM2(CNT-LINE)
                 END-IF
               ELSE
                 MOVE    SPACE            TO  D600-PTNUM(CNT-LINE)
               END-IF
      *    氏名
               MOVE    PTINF-NAME       TO  D600-NAME(CNT-LINE)
      *    性別
               EVALUATE  PTINF-SEX
                    WHEN  "1"
                          MOVE  "男"     TO  D600-SEX (CNT-LINE)
                    WHEN  "2"
                          MOVE  "女"     TO  D600-SEX (CNT-LINE)
               END-EVALUATE
      *    年齢
               MOVE  SYU-SRYYMD          TO  WRK-SYSYMD
               MOVE  PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
               COMPUTE WRK-NENREI        =   WRK-SYSYY
                                         -   WRK-BIRTHY
               IF    WRK-SYSMM           <   WRK-BIRTHM
                     COMPUTE WRK-NENREI  =   WRK-NENREI   -   1
               ELSE
                 IF   WRK-SYSMM          =   WRK-BIRTHM
                  AND WRK-SYSDD          <   WRK-BIRTHD
                      COMPUTE WRK-NENREI =   WRK-NENREI   -   1
                 END-IF
               END-IF
               MOVE  WRK-NENREI          TO  WRK-Z9-3
               MOVE  WRK-Z9-3            TO  D600-AGE (CNT-LINE)
      *    本／家  保険組合情報
               INITIALIZE                           HKNCOMBI-REC
               MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  COMB-HOSPNUM
               MOVE    WRK-SMEI-PTID(IDX-S)     TO  COMB-PTID
               MOVE    SYU-HKNCOMBINUM          TO  COMB-HKNCOMBINUM
               PERFORM 900-HKNCOMBI-READ-SEC
      *    患者保険情報検索
               INITIALIZE                           PTHKNINF-REC
               MOVE    WRK-SMEI-HOSPNUM(IDX-S)   TO  PTHKN-HOSPNUM
               MOVE    WRK-SMEI-PTID(IDX-S)     TO  PTHKN-PTID
               MOVE    COMB-HKNID               TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
               EVALUATE  PTHKN-HONKZKKBN
                  WHEN  "1"
                        MOVE  "本"    TO  D600-HKNKBN (CNT-LINE)
                  WHEN  "2"
                        MOVE  "家"    TO  D600-HKNKBN (CNT-LINE)
               END-EVALUATE
      *    保険種別
               EVALUATE    SYU-SYUHKNNUM
                  WHEN    SPACE
                           MOVE  "公費単独" TO  D600-HKNS (CNT-LINE)
                           MOVE  3          TO  WRK-SYUHKN-KBN
                  WHEN    "980"  THRU  "989"
                           MOVE  "自費"     TO  D600-HKNS (CNT-LINE)
                           MOVE  4          TO  WRK-SYUHKN-KBN
                  WHEN    "900"  THRU  "919"
                           MOVE  "治験"     TO  D600-HKNS(CNT-LINE)
                           MOVE  4          TO  WRK-SYUHKN-KBN
                  WHEN    WRK-CONS-ROUSAI-HKNNUM
                           MOVE  "労災" TO  D600-HKNS (CNT-LINE)
                           MOVE  5          TO  WRK-SYUHKN-KBN
                  WHEN    WRK-CONS-JIBAI-HKNNUM
                           MOVE  "自賠責"   TO  D600-HKNS (CNT-LINE)
                           MOVE  6          TO  WRK-SYUHKN-KBN
                  WHEN    "060"
                  WHEN    "067"
                  WHEN    "068"
                  WHEN    "069"
                           MOVE  "国保"     TO  D600-HKNS (CNT-LINE)
                           MOVE  1          TO  WRK-SYUHKN-KBN
                  WHEN    WRK-CONS-KOGAI-HKNNUM
                           MOVE  "公害"   TO  D600-HKNS (CNT-LINE)
                           MOVE  7          TO  WRK-SYUHKN-KBN
                  WHEN    "039"
                  WHEN    "040"
                           MOVE  "後期高齢" TO  D600-HKNS (CNT-LINE)
                           MOVE  8          TO  WRK-SYUHKN-KBN
                  WHEN    OTHER
                           MOVE  "社保"     TO  D600-HKNS (CNT-LINE)
                           MOVE  2          TO  WRK-SYUHKN-KBN
               END-EVALUATE
      *
      *    伝票番号枝番が１の場合は保険点数、保険別請求額の明細を印字(開始)
               IF     WRK-SMEI-DENPEDANUM(IDX-S)   =     1
                     IF  FLG-SYUNOU  = ZERO
                         PERFORM 320-BODY-HEN-SUB-SEC
                     END-IF
               END-IF
      *
      *    請求金額
               INITIALIZE  WRK-SZ
               MOVE    WRK-SMEI-SKYMONEY(IDX-S)   TO   WRK-SZ-8
               MOVE    WRK-SZ-8    TO   D600-SEIKYU(CNT-LINE)
               IF   WRK-SMEI-SKYMONEY(IDX-S)  NOT =  0
                     ADD   WRK-SMEI-SKYMONEY(IDX-S) TO   SUM-SEIKYU
                     ADD   1   TO   SUM-SEIKYUKEN
      *
                     ADD WRK-SMEI-SKYMONEY(IDX-S)  TO   SUM-G-SEIKYU
      *
               END-IF
      *
      *    領収金額
               INITIALIZE  WRK-SZ
               MOVE WRK-SMEI-NYUHEN-MONEY(IDX-S) TO  WRK-SZ-8
               MOVE WRK-SZ-8 TO   D600-RYOSYU  (CNT-LINE)
               IF   WRK-SMEI-NYUHEN-MONEY(IDX-S)  NOT =  0
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                           TO   SUM-RYOSYU
                   ADD 1   TO   SUM-RYOSYUKEN
      *
                   ADD WRK-SMEI-NYUHEN-MONEY(IDX-S)
                          TO  SUM-G-RYOSYU
      *
               END-IF
      *    CSV編集処理
               PERFORM  400-CSV-DATA-SEC
             END-IF
      *    配列インデックスのカウントアップ
             ADD  1       TO  IDX-S
      *    配列のデータを全件出力した場合は合計表の印刷を行う為にループより脱出
             IF IDX-S    >   IDX-MAX
                MOVE    1   TO   FLG-END
             END-IF
           END-PERFORM
      *
           PERFORM  3001-SYOUKEI-SET-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>サブ処理
      *****************************************************************
       320-BODY-HEN-SUB-SEC      SECTION.
      *
      *    点数単価の算出
      *    保険番号情報読み込み
           PERFORM 900-HKNNUM-READ-SEC
           IF   FLG-HKNNUM  =  1
      *    該当レコードが存在しない場合は単価を10円に設定する。
             MOVE    10     TO  HKN-TENTNK
           END-IF
      *
      *    保険点数
           MOVE    ZERO             TO  SYU-TOTAL-HKNTEN
           PERFORM  VARYING  IDZ  FROM  1  BY  1
                    UNTIL  IDZ  >  16
             MOVE    SYU-HKNTEN(IDZ)   TO  WRK-ZZ-7
             MOVE    WRK-ZZ-7          TO  D600-TEN(IDZ CNT-LINE)
             ADD  SYU-HKNTEN(IDZ)      TO  SYU-TOTAL-HKNTEN
      *
             ADD  SYU-HKNTEN(IDZ)      TO  SUM-G-TEN(IDZ)
      *
           END-PERFORM
           MOVE    SYU-TOTAL-HKNTEN    TO  WRK-ZZ-7
           MOVE    WRK-ZZ-7            TO  D600-HKNTEN (CNT-LINE)
           ADD  SYU-TOTAL-HKNTEN       TO  SUM-HKNTEN
           ADD  1                      TO  SUM-HKNKEN
      *
           ADD  SYU-TOTAL-HKNTEN       TO  SUM-G-HKNTEN
      *
      *    主保険患者負担割合、主保険給付割合の算出
      *
           IF  WRK-SYUHKN-KBN  =  1
               OR  WRK-SYUHKN-KBN  =  2
               OR  WRK-SYUHKN-KBN  =  8
      *    年齢の算出
             MOVE    SYU-SRYYMD         TO  WRK-SYSYMD
             MOVE    PTINF-BIRTHDAY     TO  WRK-BIRTHDAY
             COMPUTE  WRK-NENREI  =  WRK-SYSYY  -  WRK-BIRTHY
             IF  WRK-SYSMM  <  WRK-BIRTHM
               COMPUTE  WRK-NENREI =   WRK-NENREI  -  1
             ELSE
               IF   WRK-SYSMM  =  WRK-BIRTHM
                   AND  WRK-SYSDD  <  WRK-BIRTHD
                 COMPUTE  WRK-NENREI  =  WRK-NENREI  -  1
               END-IF
             END-IF
      *
             IF  SYU-SYUHKNNUM  =  "060"
               EVALUATE   PTHKN-HOJOKBN
                 WHEN  ZERO
                   MOVE    ZERO     TO  WRK-FTNRATE
                 WHEN  1
                   MOVE    0.1      TO  WRK-FTNRATE
                 WHEN  2
                   MOVE    0.2      TO  WRK-FTNRATE
                 WHEN  3
                   MOVE    0.3      TO  WRK-FTNRATE
                 WHEN  OTHER
                   MOVE    0.3      TO  WRK-FTNRATE
               END-EVALUATE
             ELSE
               EVALUATE   PTHKN-HOJOKBN
                 WHEN  SPACE
                   MOVE    0.3      TO  WRK-FTNRATE
                 WHEN  8
                   MOVE    0.2      TO  WRK-FTNRATE
                 WHEN  9
                   MOVE    0.1      TO  WRK-FTNRATE
                 WHEN  OTHER
                   MOVE    ZERO     TO  WRK-FTNRATE
               END-EVALUATE
             END-IF
      *
      *    三歳未満、負担割合３割以上のチェック
             IF  WRK-NENREI  <=  2
                 AND  WRK-FTNRATE  >  0.2
               MOVE    0.2          TO  WRK-FTNRATE
             END-IF
             IF  SYU-SYUHKNNUM  =  "069"  OR  "068"
               MOVE    1            TO  WRK-FTNRATE
             END-IF
             COMPUTE  WRK-KYFRATE  =  1  -  WRK-FTNRATE
           END-IF
      *
      *    保険負担額
           IF   WRK-SYUHKN-KBN  =  1
               OR  WRK-SYUHKN-KBN  =  2
               OR  WRK-SYUHKN-KBN  =  5
               OR  WRK-SYUHKN-KBN  =  6
               OR  WRK-SYUHKN-KBN  =  7
               OR  WRK-SYUHKN-KBN  =  8
             EVALUATE  WRK-SYUHKN-KBN
               WHEN  1  THRU  2
               WHEN  8
                 IF  SYU-KOH-HKNNUM(1)  =  "027"
                    COMPUTE  WRK-MONEY  =  (SYU-TOTAL-HKNTEN  *  10)
                                        -  SYU-KOH-COMPFTN(1)
                 ELSE
                   COMPUTE  WRK-MONEY  =  (SYU-TOTAL-HKNTEN * 10)
                                       -  SYU-SYUCOMPFTN
                 END-IF
      *
                 COMPUTE  WRK-MONEY  =  WRK-MONEY
                                     +  SYU-RYOYOHI-SKJ
                                     +  SYU-RYOYOHI-LIFE
                                     -  SYU-SKYMONEY-SKJ-KEI
                                     -  SYU-SKYMONEY-LIFE-KEI
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                 IF  HKN-RECEFTNDSPKBN  =  "1"
                   IF  WRK-MONEY(8:1)  >  4
                     MOVE    ZERO      TO  WRK-MONEY(8:1)
                     ADD  10           TO  WRK-MONEY
                   ELSE
                     MOVE    ZERO      TO  WRK-MONEY(8:1)
                   END-IF
                 END-IF
               WHEN  5
      *    労災の場合
      *    患者労災保険情報取得
                 PERFORM  900-PTRSIINF-READ-SEC
      *    点数単価を設定
                 IF  PTRSI-HKNKBN  =  "5"
                     AND  WRK-4001-KOMUFLG  =  "1"
      *    公務災害（健保基準）の場合
                   MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
                 ELSE
                   MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
                 END-IF
      *
                 IF  SYU-PTFTNRATE NOT =  ZERO
                   MOVE     ZERO       TO  WRK-MONEY
                 ELSE
                   COMPUTE  WRK-MONEY  =
                            (SYU-TOTAL-HKNTEN  *  WRK-TENTNK)
                   COMPUTE  WRK-MONEY  =  WRK-MONEY
                                       +  SYU-RSISHOSHIN-MONEY
                                       +  SYU-RSISAISHIN-MONEY
                                       +  SYU-RSISDO-MONEY
                                       +  SYU-RSIETC-MONEY
                                       +  SYU-RYOYOHI-SKJ
                                       +  SYU-RYOYOHI-LIFE
                 END-IF
               WHEN  6
      *    自賠の場合
                 IF  SYU-PTFTNRATE  NOT =  ZERO
                   MOVE    ZERO      TO   WRK-MONEY
                 ELSE
                   COMPUTE  WRK-MONEY  =  SYU-RSIJIBAI-SKYMONEY
                                       -  SYU-JIHI-TOTAL
                                       -  SYU-JIHI-TOTAL-TAX
                                       -  SYU-TOTAL-TGMONEY
                                       -  SYU-TOTAL-TGMONEY-TAX
                                       -  SYU-CHOSEI
                                       -  SYU-YKZ-FTN
                                       -  SYU-SKYMONEY-SKJ-JIHI-KEI
                                       -  SYU-SKYMONEY-LIFE-JIHI-KEI
                                       -  SYU-RMSAGAKU
                 END-IF
      *    公害の場合
               WHEN   7
                 COMPUTE WRK-JIHI4  =  SYU-TOTAL-TGMONEY
                                    +  SYU-TOTAL-TGMONEY-TAX
                                    +  SYU-JIHI-TOTAL
                                    +  SYU-JIHI-TOTAL-TAX
                                    +  SYU-RMSAGAKU
                                    +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                    +  SYU-SKYMONEY-LIFE-JIHI-KEI
                 IF  SYU-SRYYMD  <  "20070401"
                   COMPUTE  WRK-JIHI4  =  WRK-JIHI4
                                       +  SYU-JIHI-TAX
                 END-IF
                 COMPUTE  WRK-MONEY  =  SYU-RSIJIBAI-SKYMONEY
                                     -  WRK-JIHI4
             END-EVALUATE
             MOVE    WRK-MONEY      TO  WRK-SZ-8
             MOVE    WRK-SZ-8       TO  D600-HKNFTN(CNT-LINE)
      *
             IF   WRK-MONEY  NOT =  0
               ADD  WRK-MONEY       TO  SUM-G-HKNFTN
             END-IF
      *
           END-IF
      *
      *    自費負担額
           IF   WRK-SYUHKN-KBN  =  1
               OR  WRK-SYUHKN-KBN  =  2
               OR  WRK-SYUHKN-KBN  =  3
               OR  WRK-SYUHKN-KBN  =  4
               OR  WRK-SYUHKN-KBN  =  5
               OR  WRK-SYUHKN-KBN  =  6
               OR  WRK-SYUHKN-KBN  =  8
             EVALUATE  WRK-SYUHKN-KBN
               WHEN  1  THRU  2
               WHEN  8
      *    国保、社保の場合
                 IF     SYU-KOH-HKNNUM(1)  =  "027"
                   MOVE    SYU-KOH-COMPFTN(1)   TO  WRK-MONEY
                 ELSE
                   MOVE    SYU-SYUCOMPFTN       TO  WRK-MONEY
                 END-IF
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                 IF  WRK-MONEY(8:1) >   4
                     MOVE   ZERO    TO  WRK-MONEY(8:1)
                     ADD    10      TO  WRK-MONEY
                 ELSE
                     MOVE   ZERO    TO  WRK-MONEY(8:1)
                 END-IF
               WHEN 3
      *    公費単独の場合
                 MOVE    SYU-KOH-COMPFTN(1)     TO  WRK-MONEY
      *    レセプト１円単位表示区分が"1"の場合は１０円未満を四捨五入する。
                 IF  WRK-MONEY(8:1)  >  4
                   MOVE   ZERO      TO  WRK-MONEY(8:1)
                   ADD  10          TO  WRK-MONEY
                 ELSE
                   MOVE   ZERO      TO  WRK-MONEY(8:1)
                 END-IF
               WHEN  4
                 MOVE    SYU-SKYMONEY      TO  WRK-MONEY
               WHEN  5
                 PERFORM  900-PTRSIINF-READ-SEC
      *    点数単価を設定
                 IF  PTRSI-HKNKBN  =  "5"
                     AND  WRK-4001-KOMUFLG  =  "1"
      *    公務災害（健保基準）の場合
                   MOVE    WRK-4001-KOMUTENTANKA   TO  WRK-TENTNK
                 ELSE
                   MOVE    WRK-4001-TENTANKA       TO  WRK-TENTNK
                 END-IF
      *
                 IF  SYU-PTFTNRATE  =  ZERO
                   COMPUTE  WRK-MONEY  =  SYU-TOTAL-TGMONEY
                                       +  SYU-TOTAL-TGMONEY-TAX
                                       +  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
                                       +  SYU-RMSAGAKU
      *
                 ELSE
                   COMPUTE  WRK-MONEY  = (SYU-TOTAL-HKNTEN
                                       *  WRK-TENTNK)
                   COMPUTE  WRK-MONEY  =  WRK-MONEY
                                       +  SYU-RSISHOSHIN-MONEY
                                       +  SYU-RSISAISHIN-MONEY
                                       +  SYU-RSISDO-MONEY
                                       +  SYU-RSIETC-MONEY
                                       +  SYU-RYOYOHI-SKJ
                                       +  SYU-RYOYOHI-LIFE
                                       +  SYU-TOTAL-TGMONEY
                                       +  SYU-TOTAL-TGMONEY-TAX
                                       +  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
                                       +  SYU-RMSAGAKU
                 END-IF
               WHEN  6
                 IF  SYU-PTFTNRATE  =  ZERO
                   COMPUTE  WRK-MONEY  =  SYU-TOTAL-TGMONEY
                                       +  SYU-TOTAL-TGMONEY-TAX
                                       +  SYU-JIHI-TOTAL
                                       +  SYU-JIHI-TOTAL-TAX
                                       +  SYU-RMSAGAKU
      *
                 ELSE
                   MOVE    SYU-RSIJIBAI-SKYMONEY    TO  WRK-MONEY
                 END-IF
             END-EVALUATE
      *
             IF  WRK-SYUHKN-KBN  NOT =  4  AND  5  AND  6
      *    自費,労災、自賠責以外の場合
               IF  SYU-SRYYMD  <=  "20070331"
                 COMPUTE  WRK-JIHI  =  SYU-TOTAL-TGMONEY
                                    +  SYU-TOTAL-TGMONEY-TAX
                                    +  SYU-JIHI-TOTAL
                                    +  SYU-JIHI-TOTAL-TAX
                                    +  SYU-JIHI-TAX
               ELSE
                 COMPUTE  WRK-JIHI  =  SYU-TOTAL-TGMONEY
                                    +  SYU-TOTAL-TGMONEY-TAX
                                    +  SYU-JIHI-TOTAL
                                    +  SYU-JIHI-TOTAL-TAX
               END-IF
               COMPUTE  WRK-MONEY  =  WRK-MONEY
                                   +  WRK-JIHI
                                   +  SYU-SKYMONEY-SKJ-KEI
                                   +  SYU-SKYMONEY-SKJ-JIHI-KEI
                                   +  SYU-SKYMONEY-LIFE-KEI
                                   +  SYU-SKYMONEY-LIFE-JIHI-KEI
                                   +  SYU-RMSAGAKU
             END-IF
             MOVE    WRK-MONEY    TO  WRK-SZ-8
             MOVE    WRK-SZ-8     TO  D600-JIHIFTN(CNT-LINE)
      *
             IF   WRK-MONEY  NOT =  0
               ADD  WRK-MONEY     TO  SUM-G-JIHIFTN
             END-IF
      *
           END-IF
      *
           .
       320-BODY-HEN-SUB-EXT.
           EXIT.
      *
      *****************************************************************
      *    小計印字処理
      *****************************************************************
       3001-SYOUKEI-SET-SEC          SECTION.
      *
      *    保険点数
           IF  SUM-G-HKNTEN  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-ZZZ
               MOVE     SUM-G-HKNTEN     TO    WRK-ZZZ
               MOVE     WRK-ZZZ         TO    D600-GG-HKNTEN
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-ZZ
               MOVE     SUM-G-HKNTEN     TO    WRK-ZZ
               MOVE     WRK-ZZ         TO    D600-G-HKNTEN
           END-IF
      *    診療種別点数（診察料〜入院料）
           PERFORM  VARYING  IDX-SUM   FROM 1  BY  1
                    UNTIL    IDX-SUM   >    16
             IF  SUM-G-TEN (IDX-SUM)   >   0
               AND IDX-SUM               =  16
                 MOVE     "療養担当"        TO   D600-TRYO
             END-IF
             IF  SUM-G-TEN (IDX-SUM)  >  999999
      *    7桁の場合
                 INITIALIZE  WRK-ZZZ
                 MOVE     SUM-G-TEN (IDX-SUM)   TO   WRK-ZZZ
                 MOVE     WRK-ZZZ   TO   D600-GG-TEN (IDX-SUM)
             ELSE
      *    6桁以下の場合
                 INITIALIZE  WRK-ZZ
                 MOVE     SUM-G-TEN (IDX-SUM)   TO   WRK-ZZ
                 MOVE     WRK-ZZ    TO   D600-G-TEN (IDX-SUM)
             END-IF
           END-PERFORM
      *    保険給付額
           IF  SUM-G-HKNFTN  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-HKNFTN     TO    WRK-SZZ
               MOVE     WRK-SZZ          TO    D600-GG-HKNFTN
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-HKNFTN     TO    WRK-SZ
               MOVE     WRK-SZ           TO    D600-G-HKNFTN
           END-IF
      *    自費負担額
           IF  SUM-G-JIHIFTN  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-JIHIFTN    TO    WRK-SZZ
               MOVE     WRK-SZZ          TO    D600-GG-JIHIFTN
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-JIHIFTN    TO    WRK-SZ
               MOVE     WRK-SZ         TO    D600-G-JIHIFTN
           END-IF
      *    請求金額
           IF  SUM-G-SEIKYU  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-SEIKYU     TO    WRK-SZZ
               MOVE     WRK-SZZ         TO    D600-GG-SEIKYU
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-SEIKYU     TO    WRK-SZ
               MOVE     WRK-SZ         TO    D600-G-SEIKYU
           END-IF
      *    領収金額
           IF  SUM-G-RYOSYU  >  9999999
      *    8桁の場合
               INITIALIZE  WRK-SZZ
               MOVE     SUM-G-RYOSYU     TO    WRK-SZZ
               MOVE     WRK-SZZ         TO    D600-GG-RYOSYU
           ELSE
      *    7桁以下の場合
               INITIALIZE  WRK-SZ
               MOVE     SUM-G-RYOSYU     TO    WRK-SZ
               MOVE     WRK-SZ         TO    D600-G-RYOSYU
           END-IF
           .
       3001-SYOUKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       3201-PTHKNINF-HENSYU-SEC          SECTION.
      *
      *   保険組合せ読み出し
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SYU-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    SYU-PTID          TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *    患者保険情報読み込み
               INITIALIZE                  PTHKNINF-REC
               MOVE    SYU-HOSPNUM        TO  PTHKN-HOSPNUM
               MOVE    SYU-PTID          TO  PTHKN-PTID
               MOVE    COMB-HKNID          TO  PTHKN-HKNID
               PERFORM 900-PTHKNINF-READ-SEC
           END-IF
           .
       3201-PTHKNINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"                      TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM        TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP     TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM         TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD        TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM  TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY      TO  SPRT-PRIORITY
           IF   WRK-PARA-DENPSTYMD   <   "20080401"
             MOVE    "A00000D600O.red"        TO  SPRT-PRTID
           ELSE
             MOVE    "A00000D600V02.red"        TO  SPRT-PRTID
           END-IF
           MOVE    "日計表明細（診療費請求明細）"  TO  SPRT-TITLE
           MOVE    D600                       TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID        TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID          TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM         TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    CSVヘッダ編集処理
      *****************************************************************
       800-CSV-HEAD-SEC            SECTION.
      *
           MOVE    1                 TO  IDXP
      *
      *    集計範囲セット
           STRING    WRK-PARA-DENPSTYMDWH   DELIMITED  BY  SIZE
                     "　〜　"               DELIMITED  BY  SIZE
                     WRK-PARA-DENPEDYMDWH   DELIMITED  BY  SIZE
                                            INTO  WRK-ITEM
           END-STRING
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    入外区分セット
           IF  WRK-NYUGAIKBN  =  1
             MOVE    "入外区分：入院"       TO  WRK-ITEM
           ELSE
             MOVE    "入外区分：外来"       TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療科セット
      *    診療科名称取得
           INITIALIZE                   SYS-1005-REC
           MOVE    "1005"               TO  SYS-1005-KANRICD
           MOVE    WRK-SRYKA            TO  SYS-1005-KBNCD
           MOVE    SPA-HOSPNUM          TO  SYS-1005-HOSPNUM
           MOVE    WRK-PARA-DENPSTYMD   TO  SYS-1005-EDYUKYMD
           MOVE    WRK-PARA-DENPEDYMD   TO  SYS-1005-STYUKYMD
           MOVE    SYS-1005-REC         TO  MCPDATA-REC
           PERFORM  800-SYSKANRI-READ-SEC
           IF   FLG-SYSKANRI2  =  ZERO
             MOVE    MCPDATA-REC        TO  SYS-1005-REC
             INITIALIZE                 WRK-ITEM
             STRING    "診療科："           DELIMITED  BY  SIZE
                       SYS-1005-SRYKANAME   DELIMITED  BY  SIZE
                                            INTO  WRK-ITEM
             END-STRING
           ELSE
             MOVE    SPACE              TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    システム日付セット
           MOVE    WRK-SYSYMDWH     TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
      *    各項目名称セット
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL  IDX4  >  CSV-HEAD-MAX
             MOVE    CSV-HEAD-ITEM(IDX4)     TO  WRK-ITEM
             IF   IDX4  >=  CSV-HEAD-MAX
               PERFORM  810-CSVDATA-EDIT-SEC
             ELSE
               PERFORM  800-CSVDATA-EDIT-SEC
             END-IF
           END-PERFORM
           .
      *
       800-CSV-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    CSV編集処理
      *****************************************************************
       400-CSV-DATA-SEC            SECTION.
      *
      *    伝票番号
           MOVE    D600-RENNUM(CNT-LINE)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           IF  D600-PTNUM(CNT-LINE)  =  SPACE
             MOVE    D600-PTNUM2(CNT-LINE)   TO  WRK-ITEM
           ELSE
             MOVE    D600-PTNUM(CNT-LINE)    TO  WRK-ITEM
           END-IF
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者氏名
           MOVE    D600-NAME(CNT-LINE)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    性別
           MOVE    D600-SEX(CNT-LINE)        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    年齢
           MOVE    D600-AGE(CNT-LINE)        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    本人家族区分
           MOVE    D600-HKNKBN(CNT-LINE)     TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険種類
           MOVE    D600-HKNS(CNT-LINE)       TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険点数
           MOVE    D600-HKNTEN(CNT-LINE)     TO  WRK-ITEM
           MOVE    D600-HKNTEN(CNT-LINE)     TO  CSV-MONEY
           ADD  CSV-MONEY                TO  CSV-G-HKNTEN
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療内容明細
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL  IDX3  >  16
             MOVE    D600-TEN(IDX3 CNT-LINE)    TO  WRK-ITEM
             MOVE    D600-TEN(IDX3 CNT-LINE)    TO  CSV-MONEY
             ADD  CSV-MONEY            TO  CSV-G-TEN(IDX3)
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    保険負担額
           MOVE    D600-HKNFTN(CNT-LINE)     TO  WRK-ITEM
           MOVE    D600-HKNFTN(CNT-LINE)     TO  CSV-MONEY
           ADD  CSV-MONEY                TO  CSV-G-HKNFTN
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    自費負担額
           MOVE    D600-JIHIFTN(CNT-LINE)    TO  WRK-ITEM
           MOVE    D600-JIHIFTN(CNT-LINE)    TO  CSV-MONEY
           ADD  CSV-MONEY                TO  CSV-G-JIHIFTN
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    窓口請求金額
           MOVE    D600-SEIKYU(CNT-LINE)     TO  WRK-ITEM
           MOVE    D600-SEIKYU(CNT-LINE)     TO  CSV-MONEY
           ADD   CSV-MONEY               TO  CSV-G-SEIKYU
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    窓口領収金額
           MOVE    D600-RYOSYU(CNT-LINE)     TO  WRK-ITEM
           MOVE    D600-RYOSYU(CNT-LINE)     TO  CSV-MONEY
           ADD  CSV-MONEY                TO  CSV-G-RYOSYU
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           .
       400-CSV-DATA-EXT.
           EXIT.
      *****************************************************************
      *    CSV合計印字処理
      *****************************************************************
       3001-CSV-GOUKEI-SET-SEC          SECTION.
      *
      *    伝票番号
           MOVE    SPACE        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者番号
           MOVE    SPACE        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    患者氏名
           MOVE    SPACE        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    性別
           MOVE    SPACE        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    年齢
           MOVE    SPACE        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    本人家族区分
           MOVE    SPACE        TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険種類
           MOVE    "計"         TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    保険点数
           INITIALIZE                  WRK-ZZZ
           MOVE    CSV-G-HKNTEN        TO  WRK-ZZZ
           MOVE    WRK-ZZZ             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    診療種別点数（診察料〜入院料）
           PERFORM  VARYING  IDX-SUM  FROM  1  BY  1
                    UNTIL  IDX-SUM  >  16
             INITIALIZE              WRK-ZZZ
             MOVE    CSV-G-TEN(IDX-SUM)   TO  WRK-ZZZ
             MOVE    WRK-ZZZ              TO  WRK-ITEM
             PERFORM  800-CSVDATA-EDIT-SEC
           END-PERFORM
      *
      *    保険給付額
           INITIALIZE                  WRK-SZZ
           MOVE    CSV-G-HKNFTN        TO  WRK-SZZ
           MOVE    WRK-SZZ             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    自費負担額
           INITIALIZE                  WRK-SZZ
           MOVE    CSV-G-JIHIFTN       TO  WRK-SZZ
           MOVE    WRK-SZZ             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    請求金額
           INITIALIZE                  WRK-SZZ
           MOVE    CSV-G-SEIKYU        TO  WRK-SZZ
           MOVE    WRK-SZZ             TO  WRK-ITEM
           PERFORM  800-CSVDATA-EDIT-SEC
      *
      *    領収金額
           INITIALIZE                  WRK-SZZ
           MOVE    CSV-G-RYOSYU        TO  WRK-SZZ
           MOVE    WRK-SZZ             TO  WRK-ITEM
           PERFORM  810-CSVDATA-EDIT-SEC
      *
           INITIALIZE                     CSV-G-AREA
      *
           .
       3001-CSV-GOUKEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       800-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-DELIMITER-SEC
           .
      *
       801-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       810-CSVDATA-EDIT-SEC            SECTION.
      *
           PERFORM  820-CSVDATA-EDIT-SEC
           PERFORM  800-PUT-CR-SEC
           .
      *
       810-CSVDATA-EDIT-EXE.
           EXIT.
      *****************************************************************
      *    CSVデータ編集処理
      *****************************************************************
       820-CSVDATA-EDIT-SEC            SECTION.
      *
           IF   WRK-ITEM  NOT =  SPACE
             PERFORM  800-GET-ITEM-SIZE-SEC
             STRING  WRK-ITEM(IDX-ST:IDX-ED)  DELIMITED   BY  SIZE
                                              INTO  WRK-CSVDATA
                                              WITH  POINTER  IDXP
             END-STRING
           END-IF
           .
      *
       820-CSVDATA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    区切り文字編集処理
      *****************************************************************
       800-PUT-DELIMITER-SEC           SECTION.
      *
           STRING    ","        DELIMITED   BY  SIZE
                                INTO  WRK-CSVDATA
                                WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-DELIMITER-EXT.
           EXIT.
      *****************************************************************
      *    改行文字編集処理
      *****************************************************************
       800-PUT-CR-SEC                  SECTION.
      *
           STRING    X"0D"      DELIMITED BY  SIZE
                                INTO  WRK-CSVDATA
                                WITH  POINTER  IDXP
           END-STRING
      *
           .
       800-PUT-CR-EXT.
           EXIT.
      *****************************************************************
      *    項目サイズ取得処理
      *****************************************************************
       800-GET-ITEM-SIZE-SEC           SECTION.
      *
           MOVE    ZERO            TO  IDX-ST
                                       IDX-ED
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL  IDX1  >  WRK-ITEM-SIZE
                    OR  WRK-ITEM(IDX1:)    =  SPACE
      *
             IF  IDX-ST  =  ZERO
               IF  WRK-ITEM(IDX1:1)  NOT =  SPACE
                 MOVE    IDX1      TO  IDX-ST
                 MOVE    1         TO  IDX-ED
               END-IF
             ELSE
               COMPUTE  IDX-ED  =  IDX-ED  +  1
             END-IF
      *
           END-PERFORM
      *
           .
       800-GET-ITEM-SIZE-EXT.
           EXIT.
      *****************************************************************
      *    ＣＳＶ出力処理
      *****************************************************************
       230-TOUKEICSV-SEC               SECTION.
      *
           IF   WRK-PARA-DENPSTYMD  >=  "20080401"
             INITIALIZE                     ORCSTOUKEICSVAREA
             MOVE    "INS"                  TO  STOUKEICSV-MODE
             MOVE    LNK-PRTKANRI-TBL-KEY   TO  STOUKEICSV-TBL-KEY
             MOVE    LNK-PRTKANRI-RENNUM    TO  STOUKEICSV-RENNUM
             MOVE    LNK-PRTKANRI-TBL-GROUP TO  STOUKEICSV-TBL-GROUP
             MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                            TO  STOUKEICSV-SHORI-RENNUM
             MOVE    LNK-PRTKANRI-SHELLID   TO  STOUKEICSV-SHELLID
             MOVE    LNK-PRTKANRI-SRYYM     TO  STOUKEICSV-SRYYM
             MOVE    ZERO                   TO  STOUKEICSV-PTID
             MOVE    "/var/tmp/"            TO  STOUKEICSV-TO-FOLDER
             STRING  SPA-HOSPNUM            DELIMITED  BY  SIZE
                     "nikkeimeisai_shinryouhi_"  DELIMITED  BY  SIZE
                     WRK-PARA-DENPSTYMD     DELIMITED  BY  SIZE
                     "_"                    DELIMITED  BY  SIZE
                     WRK-PARA-DENPEDYMD     DELIMITED  BY  SIZE
                     ".csv"                 DELIMITED  BY  SIZE
                                            INTO  STOUKEICSV-TO-DATA
             END-STRING
             MOVE    "2"                    TO  STOUKEICSV-CODE-TYPE
             MOVE    WRK-CSVDATA            TO  STOUKEICSV-CSVDATA
             MOVE    "日計表明細（診療費請求明細）" TO  STOUKEICSV-TITLE
      *
             CALL    "ORCSTOUKEICSV"        USING
                                            ORCSTOUKEICSVAREA
                                            SPA-AREA
             IF  ( STOUKEICSV-RETURN  =  ZERO )
               CONTINUE
             ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                            TO  WRK-RECEERR
               PERFORM  500-ERR-HENSYU-SEC
             END-IF
      *
             INITIALIZE                     WRK-CSVDATA
           END-IF
      *
           MOVE    1                     TO  IDXP
      *
           .
      *
       230-TOUKEICSV-EXT.
           EXIT.
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
      *    ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           PERFORM  900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細情報　入返金発行日で検索
      *****************************************************************
       910-DBSELECT-SEC                 SECTION.
      *
           INITIALIZE                   SYUMEI-REC
      *
           MOVE    WRK-PARA-HOSPNUM     TO  SMEI-HOSPNUM
           MOVE    IDX-NG               TO  SMEI-NYUGAIKBN
           MOVE    WRK-PARA-DENPSTYMD(1:6)  TO  SMEI-NYUHEN-YMD(1:6)
           MOVE    IDX-DAY              TO  SMEI-NYUHEN-YMD(7:2)
           MOVE    SYUMEI-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_syumei"         TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  910-DBFETCH-SEC
           ELSE
            MOVE    1               TO  FLG-SYUMEI
           END-IF
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     収納明細情報を検索処理
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"         TO  MCP-FUNC
           MOVE    "tbl_syumei"      TO  MCP-TABLE
           MOVE    "key4"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    ZERO            TO  FLG-SYUMEI
             MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
             MOVE    1               TO  FLG-SYUMEI
           END-IF
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"   TO  MCP-FUNC
           MOVE    "tbl_syumei"      TO  MCP-TABLE
           MOVE    "key4"            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"       USING
                                     MCPAREA
                                     MCPDATA-REC
                                     SPA-AREA
           .
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       900-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                 SYUNOU-REC
           MOVE    SMEI-HOSPNUM    TO  SYU-HOSPNUM
           MOVE    SMEI-NYUGAIKBN TO  SYU-NYUGAIKBN
           MOVE    SMEI-PTID      TO  SYU-PTID
           MOVE    SMEI-DENPNUM   TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syunou"     TO    MCP-TABLE
               MOVE    "key"            TO    MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC         =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO       TO  FLG-SYUNOU
               ELSE
                   MOVE    1      TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1      TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報検索
      *****************************************************************
       901-SYUNOU-READ-SEC                 SECTION.
      *
           INITIALIZE                   SYUNOU-REC
      *
           MOVE    WRK-SMEI-HOSPNUM(IDX-S)  TO  SYU-HOSPNUM
           MOVE    WRK-SMEI-NYUGAIKBN(IDX-S)   TO  SYU-NYUGAIKBN
           MOVE    WRK-SMEI-PTID(IDX-S)  TO  SYU-PTID
           MOVE    WRK-SMEI-DENPNUM(IDX-S)  TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC     TO  MCPDATA-REC
           MOVE    "DBSELECT"     TO  MCP-FUNC
           MOVE    "tbl_syunou"           TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syunou"           TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC   TO  SYUNOU-REC
                   MOVE    ZERO          TO  FLG-SYUNOU
               ELSE
                   MOVE    1        TO  FLG-SYUNOU
               END-IF
           ELSE
               MOVE    1       TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       901-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報検索
      *****************************************************************
       900-HKNNUM-READ-SEC                 SECTION.
      *
           INITIALIZE                      HKNNUM-REC
           MOVE    SYU-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SYU-SYUHKNNUM       TO  HKN-HKNNUM
           MOVE    WRK-PARA-DENPEDYMD  TO  HKN-TEKSTYMD
           MOVE    WRK-PARA-DENPSTYMD  TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC             =   ZERO
               MOVE    "DBFETCH"      TO  MCP-FUNC
               MOVE    "tbl_hknnum"       TO  MCP-TABLE
               MOVE    "key5"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC          =  ZERO
                   MOVE    MCPDATA-REC TO  HKNNUM-REC
                   MOVE    ZERO        TO  FLG-HKNNUM
               ELSE
                   MOVE    1           TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hknnum"            TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
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
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTNUM-READ-SEC         SECTION.
      *
           INITIALIZE                    PTNUM-REC
           MOVE    SMEI-HOSPNUM          TO   PTNUM-HOSPNUM
           MOVE    SMEI-PTID             TO   PTNUM-PTID
           MOVE    PTNUM-REC             TO   MCPDATA-REC
           MOVE    "DBSELECT"            TO   MCP-FUNC
           MOVE    "tbl_ptnum"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF   MCP-RC   =   ZERO
             MOVE    "DBFETCH"           TO   MCP-FUNC
             MOVE    "tbl_ptnum"         TO   MCP-TABLE
             MOVE    "key"               TO   MCP-PATHNAME
             CALL    "ORCDBMAIN"         USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF   MCP-RC   =   ZERO
               MOVE    MCPDATA-REC       TO   PTNUM-REC
             ELSE
               INITIALIZE                PTNUM-REC
             END-IF
           ELSE
             INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"       TO   MCP-FUNC
           MOVE    "tbl_ptnum"           TO   MCP-TABLE
           MOVE    "key"                 TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込処理
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_pthkninf"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_pthkninf"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   MOVE    ZERO                TO  FLG-PTHKNINF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
                   INITIALIZE                  PTHKNINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_pthkninf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災マスター読込処理
      *****************************************************************
       900-PTRSIINF-READ-SEC         SECTION.
      *
           INITIALIZE                  PTRSIINF-REC
           MOVE    SYU-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SYU-PTID            TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF   MCP-RC  =  ZERO
             MOVE    "DBFETCH"         TO  MCP-FUNC
             MOVE    "tbl_ptrsiinf"    TO  MCP-TABLE
             MOVE    "key"             TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"       USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
             ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE              PTRSIINF-REC
             END-IF
           ELSE
             MOVE    1                 TO  FLG-PTRSIINF
             INITIALIZE                PTRSIINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込処理
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"           TO  MCP-TABLE
           MOVE    "key"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"       TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC      TO  HKNCOMBI-REC
                   MOVE    ZERO             TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                TO  FLG-HKNCOMBI
                   INITIALIZE               HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                    TO  FLG-HKNCOMBI
               INITIALIZE                   HKNCOMBI-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hkncombi"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"             TO     MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBSTART"            TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"           TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
      *
           MOVE    "DBDISCONNECT"       TO  MCP-FUNC.
           MOVE    LOW-VALUE            TO     MCP-TABLE
                                               MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
                                        .
           .
       900-DBCLOSE-EXT.
           EXIT.
