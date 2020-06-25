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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGR02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト作成−印刷指示（Ｒ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.07.13  県内・県外別に印刷するように修正 
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     01/12/05  レセプト種別が不明のレセプトも
      *                                     印刷できるようにする      
      *  01.02.01    MCC-藤原     02.01.23  帳票出力用パラメタファイル
      *                                     を未使用とする
      *  01.02.02    MCC-藤原     02.01.31  ジョブ管理ＤＢの追加
      *                                     Ｆ８の追加
      *  01.02.03    MCC-藤原     02.02.12  件数の追加
      *  01.02.04    MCC-藤原     02.02.25  不明分の出力方法の変更
      *  01.02.05    MCC-藤原     02/02/26  レセプト管理ファイルを
      *                                     索引ファイルからＤＢへ変更
      *  01.02.06    MCC-藤原     02/02/26  レセ電処理の追加
      *  01.02.07    MCC-藤原     02.03.18  端末ＩＤ
      *                                     X(16)からX(32)に変更
      *  01.02.08    MCC-藤原     02/04/30  印刷順の追加
      *  01.02.09    MCC-藤原     02/06/06  処理結果画面追加
      *  01.02.10    NACL-藤原    02/07/30  情報削除ボタン追加
      *  01.02.11    NACL-藤原    02/08/23  自費を不明分と分けて表示する
      *  01.02.12    NACL-藤原    02/08/29  レセプトが選択されていないとき
      *                                     エラーとする
      *
      *  01.03.01    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.03.02    NACL-藤原    02/09/24  ２００２．１０改正対応
      *  01.03.03    NACL-藤原    02/10/28  印刷順が０３、０４のときの印刷修正
      *                                     修正
      *　入院機能追加　
      *  01.04.01    NACL-藤原    02/12/11  入外区分追加 
      *  01.04.02    NACL-藤原    03/01/08  通し番号のためのパラメタ追加 
      *  01.04.03    NACL-藤原    03/01/15  確定のとき自動的に情報確認画面を表示
      *  01.04.04    NACL-藤原    03/02/25  プリンタ名をオンラインより渡す
      *  01.04.05    NACL-藤原    03/03/03  再印刷機能追加
      *  01.04.06    NACL-藤原    03/05/23  レセ電のボタンを削除
      *  01.04.07    NACL-藤原    03/08/05  ヴで始まるカナ氏名のレセプト
      *                                     を対象とするように修正
      *  01.04.08    NACL-藤原    03/08/05  レセ電のボタンを削除
      *  01.04.09    NACL-藤原    03/08/19  件数印刷の処理を追加
      *  01.04.10    NACL-藤原    03/08/29  出力帳票の指定を追加
      *  01.04.11    NACL-小豆沢  04/04/15  入院レセ出力先プリンタの指定追加
      *　プレビュー機能追加
      *  01.05.01    NACL-藤原    04/04/07  プレビュー機能追加
      *  01.05.02    NACL-藤原    04/06/30  印刷順の追加（個別作成時のみ）
      *  01.05.03    NACL-藤原    04/07/12  指定印刷時の選択項目の色指定
      *  中止処理追加
      *  01.06.01    NACL-藤原    04/09/28  中止ボタンを追加
      *                                     印刷済のチェック表示対応 
      *  01.06.02    NACL-藤原    05/01/13  受診日指定
      *  01.06.03    NACL-藤原    05/01/30  訂正日指定
      *  01.06.03    NACL-藤原    05/01/30  「印刷する」場合だけ印刷済の
      *                                     チェック表示を行う
      *
      *  02.06.01    NACL-藤原    05/07/13  国保の県内県外の印刷順の指定
      *                                     「４」の追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.03    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *  02.08.02    NACL-藤原    06/03/10  患者番号順の範囲指定を変更
      *  02.08.03    NACL-藤原    06/03/14  編綴順情報の表示の修正
      *  02.08.04    NACL-藤原    06/04/07  編綴順指定のときの自費・不明分
      *                                     レセが印刷されない現象の修正
      *
      *  03.00.01    NACL-藤原    06/05/29  印刷時のプリンター指定
      *
      *  03.02.01    NACL-藤原    06/08/01  国保主科対応
      *
      *  03.03.01    NACL-藤原    06/09/27  ２００６年１０月改正対応
      *                                     前期高齢者の表示
      *
      *  03.04.01    NACL-藤原    06/11/01  照会機能連携対応
      *  03.04.02    NACL-藤原    06/12/05  未請求レセ対応
      *  03.04.03    NACL-藤原    07/01/16  プリンタ一括設定対応
      *  03.04.04    NACL-藤原    07/01/19  全体での出力順の指定
      *                                     「０７」「０８」の追加
      *  03.04.05    NACL-藤原    07/01/24  種別・カナ氏名順、種別・患者
      *                                     番号順の指定
      *                                     「０９」「１０」の追加
      *  03.04.06    NACL-藤原    07/03/01  国保レセプト編綴順詳細設定対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/30  印刷後のチェック解除機能対応
      *
      *  04.02.01    NACL-藤原    08/02/25  一括入力対応
      *  04.02.02    NACL-藤原    08/03/25  平成２０年４月改正対応
      *  04.02.03    NACL-藤原    08/05/07  公費３種、４種の併用の
      *                                     レセプト種別の追加
      *  04.02.04    NACL-藤原    08/06/18  メッセージの追加
      *
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *  04.03.02    NACL-藤原    08/07/09  入力順指定時の出力先設定の不可
      *  04.03.03    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.04    NACL-藤原    08/11/05  医師別出力時の「主科未設定分」
      *                                     を表示しないようにする
      *
      *  04.04.01    NACL-藤原    09/03/05  プリンタの初期設定
      *
      *  04.05.01    NACL-藤原    09/06/11  病棟別出力対応
      *  04.05.02    NACL-藤原    09/06/18  社保レセプト遍綴順対応
      *  04.05.03    NACL-藤原    09/08/19  全件印刷時の自費レセプトの
      *                                     印刷指定対応
      *  04.05.04    NACL-藤原    12/04/10  平成２４年４月改正対応
      *                                     平成２４年４月以降の社保分に
      *                                     ついて在総診の設定は行わない
      *
      *  04.06.01    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *
      *  04.07.01    NACL-藤原    11/09/02  起動方法の変更
      *  04.07.02    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.03    NACL-藤原    12/12/03  ターミナルデジットによる出力対応
      *  04.07.04    NACL-藤原    13/01/07  自費レセプトを印刷しない設定
      *                                     のとき、入力順、ターミナルデジット
      *                                     出力の対象としない
      *  04.07.05    NACL-藤原    13/03/01  保険者番号の条件設定変更
      *  04.07.06    NACL-藤原    13/07/24  入力順指定時の画面表示の修正
      *  04.07.07    NACL-藤原    14/06/10  ターミナルデジットのときは
      *                                     特別療養費等の処理はしない
      *  04.07.08    NACL-藤原    14/11/19  診療科の初期表示
      *  04.07.09    NACL-藤原    15/06/19  診療科表示件数の変更
      *  04.07.10    NACL-藤原    15/07/15  社保レセプト遍綴順のときの
      *                                     平成２４年４月以降の表示の修正
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    16/03/07  プリンタ設定の修正
      *  04.08.04    NACL-藤原    16/08/24  持参薬対応
      *  04.08.05    NACL-藤原    17/02/14  編綴順指定時の特別療養費（国保）
      *                                     のパラメタ設定の修正
      *  04.08.06    NACL-藤原    17/08/29  一時保存テーブル削除対応
      *
      *  04.09.01    NACL-藤原    16/12/12  ＨＡＯＲＩ対応
      *  05.00.00    NACL-森脇    17/12/05  クライアント印刷対応
      *  05.00.01    NACL-藤原    18/10/15  ＭＯＮＢＬＯＢエラー対応
      *  05.00.02    NACL-藤原    18/11/21  社保編綴順のときの表示の修正
      *  05.00.03    NACL-藤原    18/11/28  カナ氏名順のときの印刷対応
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
           COPY    "CPTEMPFL.INC"  REPLACING   //TEMPFLPARA//
                                   BY          //RECEERR//.
      *
      *    エラーファイル名称領域 
           COPY    "CPERRFL.INC".
      *
      *    帳票出力用パラメタ  
       01  WRK-RECE09-REC. 
           03  WRK-RECE09-KEY.
               05  WRK-RECE09-PRTID        PIC X(04).
               05  WRK-RECE09-SRYYM        PIC X(06).
               05  WRK-RECE09-CREYMD       PIC X(08).
               05  WRK-RECE09-CREHMS       PIC X(06).
               05  WRK-RECE09-PREFKBN      PIC X(01).
               05  WRK-RECE09-TEISYUTUSAKI PIC X(01).
               05  WRK-RECE09-RECESYUBETU  PIC X(04).
               05  WRK-RECE09-NYUGAIKBN    PIC X(01).
           03  WRK-RECE09-PRTJYUN          PIC X(02).
           03  WRK-RECE09-DATA.
               05  WRK-RECE09-HKNJANUM     PIC X(08).
               05  WRK-RECE09-CHOUKI       PIC X(01).
               05  WRK-RECE09-ZAITAKU      PIC X(01).
               05  WRK-RECE09-KOUGAKU      PIC X(01).
               05  WRK-RECE09-SRT-LAST     PIC X(01).
           03  WRK-RECE09-DATA-R           REDEFINES    WRK-RECE09-DATA.
               05  WRK-RECE09-DRCD         PIC X(05).
               05  WRK-RECE09-DRCD-PRTJYUNKBN
                                           PIC X(01).
               05  WRK-RECE09-DRCD-FIL     PIC X(06).
           03  WRK-RECE09-DATA-R1          REDEFINES    WRK-RECE09-DATA.
               05  WRK-RECE09-BTUNUM       PIC X(02).
               05  WRK-RECE09-BTUNUM-PRTJYUNKBN
                                           PIC X(01).
               05  WRK-RECE09-BTUNUM-FIL   PIC X(09).
       01  WRK-RECE09-STDATA               PIC X(100).
       01  WRK-RECE09-EDDATA               PIC X(100).
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
      *    レセプト作成−印刷指示画面ＳＰＡ
           COPY    "R02SCR-SPA".
      *
       01  SPA-SCR-REC.
           COPY    "CPR02SPASCR.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-QRECEPRT        PIC 9(01).
      *
           03  FLG-PRTNM-SET       PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
           03  STS-RECEERR         PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDV                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
      *
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDWW                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(03).
               05  WRK-WIDGET2     PIC 9(02).
               05  WRK-WIDGET-FIL  PIC X(01).
               05  WRK-WIDGET3     PIC 9(02).
               05  WRK-WIDGET4     PIC 9(02).
      *
           03  WRK-RIDMSG          PIC X(70).
           03  WRK-Z9              PIC ZZZ9.
           03  WRK-YM              PIC X(07). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-PRTKBN          PIC X(42).
           03  WRK-RECEKBN         PIC X(10).
           03  WRK-NYUGAIKBN       PIC X(06).
           03  WRK-PREFKBN         PIC X(01).
           03  WRK-TEISYUTUSAKI    PIC X(01).
           03  WRK-CHOUKI          PIC X(01).
           03  WRK-ZAITAKU         PIC X(01).
           03  WRK-KOUGAKU         PIC X(01).
           03  WRK-SRT-LAST        PIC X(01).
           03  WRK-SYUKA           PIC X(02).
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-PAGE            PIC ZZZZ9.
           03  WRK-STYMD           PIC X(22).
           03  WRK-EDYMD           PIC X(22).
           03  WRK-PRTJYUN         PIC X(02).
           03  WRK-PAGE-MAX        PIC 9(02).
      *  
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(03).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *     
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM   PIC  X(06).
             05  WRK-SHELL-TERMID  PIC  X(16).
             05  WRK-SHELL-SYSYMD  PIC  X(08).  
      *
           03  WRK-RECE08-PRTJYUN  PIC 9(04).
      *
           03  WRK-KBNCD.
               05  WRK-KBNCD-9     PIC 9(08).
      *
           03  WRK-PRTNM           PIC 9(04).
           03  WRK-PRTNM-R         REDEFINES   WRK-PRTNM.
               05  WRK-PRTNM-FIL   PIC X(02).
               05  WRK-PRTNM-X     PIC X(02). 
      *
           03  WRK-GMN-PRTNM-G.
               05  WRK-GMN-PRTNM   PIC X(02).
               05  WRK-GMN-PRTNM-FIL
                                   PIC X(01).
               05  WRK-GMN-PRTNMMEI
                                   PIC X(16).
           03  WRK-PRTNMMEI        PIC X(16).
      * 
           03  WRK-SYUKA-BACK      PIC X(02).
      *
           03  WRK-GMN-PAGE        PIC 9(02).
      *
           03  WRK-GMN-SYUKA-G.
               05  WRK-GMN-SYUKA   PIC X(02).
               05  WRK-GMN-F1      PIC X(01).
               05  WRK-GMN-SYUKAMEI
                                   PIC X(18).
      *      
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG1        PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
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
       01  WRK-CONS-AREA.
      *    １頁の最大行数
           03  WRK-CONS-LINE-MAX   PIC 9(02)   VALUE   36.   
      *    最大頁数
           03  WRK-CONS-PAGE-MAX   PIC 9(02)   VALUE   19.   
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "RECEPT2".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
      *
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPT2".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR042ERR".
      *
           03  WRK-CONS-SRYYM-200804
                                   PIC X(06)   VALUE   "200804".
           03  WRK-CONS-SRYYM-201204
                                   PIC X(06)   VALUE   "201204".
      *
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
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
           COPY    "CPSK2001.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1010.INC".
      *
           COPY    "CPSK2002.INC".
      *
      *    レセプト作成指示（印刷順）情報
           COPY    "CPSK2003.INC".
      *    レセプト作成指示（レセプト種別）情報
           COPY    "CPSK2004.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK2008.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *    病棟情報
           COPY    "CPSK5001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    レセプト     
           COPY    "CPRCF008.INC".
      *
       01  QUERY-RECE08-REC. 
           COPY    "CPQRECEPRT.INC".
      *
      *    保険者番号
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    プリンタ名取得サブ
           COPY  "CPORCSPRTNMGET.INC".
      *
           COPY    "MONBLOB".
      *
      *----(05.00.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(05.00.00)--ADD-END-----
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
           COPY    "ORCA42SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-R02
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
      *    画面ＳＰＡ読込み
           PERFORM 000-R02SPA-SET-SEC
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO    "CLICKED"
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
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 000-R02SPA-OUT-SEC
           END-IF
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-R02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    画面ＳＰＡ読込
      *****************************************************************
       000-R02SPA-SET-SEC             SECTION.
      *
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  FLG-SPASCR
      *??  MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
      *
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SPATMP-READ-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO   SPA-SCR-REC
               END-IF
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-DBCLOSE-SEC
           .
       000-R02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ書込
      *****************************************************************
       000-R02SPA-OUT-SEC             SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
           MOVE    1                   TO  SPATMP-EDANUM
           MOVE    SPA-SCR-REC         TO  SPATMP-DATA-REC
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "R02 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       000-R02SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "RERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "R02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *        画面ＳＰＡ読込
               PERFORM 000-R02SPA-SET-SEC
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XB01"  OR  "XC01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    SPA-WORK        TO  SPA-R01KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-R02
                   GO  TO  300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "RID1"
                   PERFORM 330-RID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "RID3"
                   PERFORM 330-RID3-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R99"
                   PERFORM 340-R99-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R06"
                   PERFORM 340-R06-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "R05"
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
      *****MOVE    1               TO  SPA-GMN-PAGE
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
           INITIALIZE              SPA-R02-AREA
                                   SPA-R02-AREA-1
                                   SPA-GMN-PRTNM-AREA
      *
      *    処理区分
           MOVE    "T"             TO  SPA-GMN-SYOKBN
      *
      *    点検用レセプト作成時
           EVALUATE    SPA-R01-PRTKBN
               WHEN    "2"
                   MOVE    "【点検用】"    TO  WRK-PRTKBN
               WHEN    "3"
                   MOVE    "【点検用（０点公費）】"
                                           TO  WRK-PRTKBN
               WHEN    "4"
                   MOVE    "【点検用（包括診療分・持参薬）】"
                                           TO  WRK-PRTKBN
               WHEN    "5"
                   MOVE    "【点検用（０点公費・包括診療分・持参薬）】"
                                           TO  WRK-PRTKBN
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-PRTKBN
           END-EVALUATE
      *
      *    ファイル作成区分
           EVALUATE    SPA-R01-FILEKBN
               WHEN    "1"
                   MOVE    SPA-R01-SRYYM       TO  WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2) 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   EVALUATE    SPA-R01-RECE07-RECEKBN
                       WHEN    "0"
                           MOVE    SPACE           TO  WRK-RECEKBN
                       WHEN    "1"
                           MOVE    "・社保"        TO  WRK-RECEKBN
                       WHEN    "2"
                           MOVE    "・国保"        TO  WRK-RECEKBN
                       WHEN    "7"
                           MOVE    "・広域"        TO  WRK-RECEKBN
                   END-EVALUATE  
                   EVALUATE    SPA-R01-RECE07-NYUGAIKBN
                       WHEN    "1"
                           MOVE    "入院"        TO  WRK-NYUGAIKBN
                       WHEN    "2"
                           MOVE    "入院外"      TO  WRK-NYUGAIKBN
                   END-EVALUATE  
                   STRING  WRK-PRTKBN         DELIMITED  BY  SPACE 
                           WRK-HENYMDG3(1:16) DELIMITED  BY  SIZE
                           WRK-NYUGAIKBN      DELIMITED  BY  SPACE 
                           WRK-RECEKBN        DELIMITED  BY  SPACE 
                           "一括作成分"       DELIMITED  BY  SIZE
                           INTO    SPA-GMN-FILEKBN
                   END-STRING
               WHEN    "2"
                   EVALUATE    SPA-R01-RECE07-NYUGAIKBN
                       WHEN    "1"
                           MOVE    "入院"        TO  WRK-NYUGAIKBN
                       WHEN    "2"
                           MOVE    "入院外"      TO  WRK-NYUGAIKBN
                   END-EVALUATE  
                   EVALUATE    SPA-R01-RECE07-SYORISBT
                       WHEN    ZERO
                       WHEN    SPACE
                           STRING  WRK-PRTKBN     DELIMITED  BY  SPACE 
                                   WRK-NYUGAIKBN  DELIMITED  BY  SPACE 
                                   "個別作成分"   DELIMITED  BY  SIZE
                                   INTO    SPA-GMN-FILEKBN
                           END-STRING
                       WHEN    "1"
                           MOVE    SPA-R01-RECE07-STYMD
                                                      TO  WRK-SYMD
                           MOVE    "01"               TO  WRK-SYMD (7:)
                           PERFORM 5002-HIZUKE-HEN-SEC
                           STRING  WRK-PRTKBN     DELIMITED  BY  SPACE 
                                   "訂正日指定　" DELIMITED  BY  SIZE
                                   WRK-HENYMDG3 (1:16)
                                                  DELIMITED  BY  SIZE
                                   "分"           DELIMITED  BY  SIZE
                                   INTO    SPA-GMN-FILEKBN
                           END-STRING
                       WHEN    "2"
                           MOVE    SPA-R01-RECE07-STYMD
                                                      TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    WRK-HENYMDG3       TO  WRK-STYMD
                           MOVE    SPA-R01-RECE07-EDYMD
                                                      TO  WRK-SYMD
                           PERFORM 5002-HIZUKE-HEN-SEC
                           MOVE    WRK-HENYMDG3       TO  WRK-EDYMD
                           STRING  WRK-PRTKBN     DELIMITED  BY  SPACE 
                                   "受診日指定　" DELIMITED  BY  SIZE
                                   WRK-STYMD      DELIMITED  BY  SIZE
                                   "〜"           DELIMITED  BY  SIZE
                                   WRK-EDYMD (17:6)
                                                  DELIMITED  BY  SIZE
                                   INTO    SPA-GMN-FILEKBN
                           END-STRING
                       WHEN    "3"
                           MOVE    SPA-R01-RECE07-STYMD
                                                      TO  WRK-SYMD
                           MOVE    "01"               TO  WRK-SYMD (7:)
                           PERFORM 5002-HIZUKE-HEN-SEC
                           STRING  WRK-PRTKBN     DELIMITED  BY  SPACE 
                                   WRK-NYUGAIKBN  DELIMITED  BY  SPACE 
                                   "　照会連携　" DELIMITED  BY  SIZE
                                   WRK-HENYMDG3 (1:16)
                                                  DELIMITED  BY  SIZE
                                   "分"           DELIMITED  BY  SIZE
                                   INTO    SPA-GMN-FILEKBN
                           END-STRING
                       WHEN    "4"
                           STRING  WRK-PRTKBN     DELIMITED  BY  SPACE 
                                   WRK-NYUGAIKBN  DELIMITED  BY  SPACE 
                                   "未請求解除分" DELIMITED  BY  SIZE
                                   INTO    SPA-GMN-FILEKBN
                           END-STRING
                   END-EVALUATE
           END-EVALUATE
      *
      *    ファイル作成日
           IF      SPA-R01-RECE07-CREYMD   NOT =   SPACE
               MOVE    SPA-R01-RECE07-CREYMD
                                           TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG1        TO  SPA-GMN-CREYMD
               MOVE    SPA-R01-RECE07-CREHMS(1:2)
                                           TO  SPA-GMN-CREYMD (11:2)
               MOVE    ":"                 TO  SPA-GMN-CREYMD (13:1)
               MOVE    SPA-R01-RECE07-CREHMS(3:2)
                                           TO  SPA-GMN-CREYMD (14:2)
               MOVE    ":"                 TO  SPA-GMN-CREYMD (16:1)
               MOVE    SPA-R01-RECE07-CREHMS(5:2)
                                           TO  SPA-GMN-CREYMD (17:2)
           END-IF
      *         
      *    レセプト編綴順情報
           MOVE    ZERO                TO  SPA-NAI-2008-HYOUJI
           IF    ( SPA-R01-FILEKBN     =   "1"             )
           AND   ( SPA-R01-RECEKBN     =   ZERO    OR  "2" )
           AND   ( SPA-R01-RECE07-HSEQNUM
                                       >   ZERO            )
           AND   ( SPA-R01-RECE07-HKNJANUM
                                   NOT =   "99999999"      )
               IF      SPA-R01-RECE07-HSEQNUM
                                           =   "99999999"
      *            レセプト編綴順情報（詳細設定）
                   MOVE    9               TO  SPA-NAI-2008-HYOUJI
                   PERFORM 3103-2003-HENSYU-SEC
               ELSE  
      *            レセプト編綴順情報（詳細設定以外）
                   MOVE    SPACE               TO  SYS-2008-REC
                   INITIALIZE                      SYS-2008-REC
                   MOVE    "2008"              TO  SYS-2008-KANRICD
                   MOVE    SPA-R01-RECE07-HSEQNUM
                                               TO  WRK-KBNCD-9
                   MOVE    WRK-KBNCD           TO  SYS-2008-KBNCD
                   MOVE    SPA-SYSYMD          TO  SYS-2008-STYUKYMD
                                                   SYS-2008-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM         TO  SYS-2008-HOSPNUM
                   MOVE    SYS-2008-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
                   MOVE    "key10"             TO  WRK-MCP-PATHNAME
                   PERFORM 900-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-2008-REC
                       MOVE    SYS-2008-ZAITAKU
                                               TO  SPA-NAI-2008-ZAITAKU
                       MOVE    SYS-2008-CHOUKI TO  SPA-NAI-2008-CHOUKI
                       MOVE    SYS-2008-PREFKBN-PRTJYUN
                                               TO
                                       SPA-NAI-2008-PREFKBN-PRTJYUN
                       MOVE    SYS-2008-HKNJANUM-PRTJYUN
                                               TO
                                       SPA-NAI-2008-HKNJANUM-PRTJYUN
                       MOVE    SYS-2008-RECESYUBETU-PRTJYUN
                                               TO
                                       SPA-NAI-2008-SYUBETU-PRTJYUN
                       MOVE    SYS-2008-HKNJANUM-PRTKBN
                                               TO
                                       SPA-NAI-2008-HKNJANUM-PRTKBN
                       MOVE    SYS-2008-HKNJANUM
                                               TO
                                       SPA-NAI-2008-HKNJANUM
                       MOVE    SPACE           TO  HKNJAINF-REC
                       INITIALIZE                  HKNJAINF-REC
grpsys                 MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
                       MOVE    SPA-NAI-2008-HKNJANUM
                                               TO  HKNJA-HKNJANUM
                       MOVE    HKNJAINF-REC    TO  MCPDATA-REC
                       PERFORM 900-HKNJAINF-READ-SEC
                       MOVE    HKNJA-HKNJANAME TO
                                               SPA-NAI-2008-HKNJANAME
                       MOVE    1               TO  SPA-NAI-2008-HYOUJI
                   END-IF
               END-IF
           END-IF
      *
      *    印刷順（社保）
           MOVE    "01"                  TO  SPA-GMN-PRTJYUNL    (1)
           MOVE    "種別・保険者番号・カナ氏名順"
                                         TO  SPA-GMN-PRTJYUNMEIL (1)
           MOVE    "02"                  TO  SPA-GMN-PRTJYUNL    (2)
           MOVE    "種別・保険者番号・患者番号順"
                                         TO  SPA-GMN-PRTJYUNMEIL (2)
           MOVE    "03"                  TO  SPA-GMN-PRTJYUNL    (3)
           MOVE    "カナ氏名順"          TO  SPA-GMN-PRTJYUNMEIL (3)
           MOVE    "04"                  TO  SPA-GMN-PRTJYUNL    (4)
           MOVE    "患者番号順"          TO  SPA-GMN-PRTJYUNMEIL (4)
           MOVE    4                     TO  SPA-GMN-PRTJYUN-MAX
           IF    ( SPA-R01-FILEKBN       =   "2"             )
           AND   ( SPA-R01-RECE07-SYORISBT
                                         =   ZERO    OR  "3"
                                                     OR  "4" )
               MOVE    "05"                  TO  SPA-GMN-PRTJYUNL    (5)
               MOVE    "入力順"              TO  SPA-GMN-PRTJYUNMEIL (5)
               ADD     1                     TO  SPA-GMN-PRTJYUN-MAX
           END-IF
           IF      SPA-R01-FILEKBN       =   "1"
               IF    ( SPA-R01-RECEKBN   =   ZERO    OR  "1" )
               AND   ( SPA-R01-RECE08-RECESYUBETU-SRT
                                     NOT =   SPACE           )
                   ADD     1         TO  SPA-GMN-PRTJYUN-MAX
                   MOVE    "06"      TO  SPA-GMN-PRTJYUNL
                                             (SPA-GMN-PRTJYUN-MAX)
                   MOVE    "編綴順"  TO  SPA-GMN-PRTJYUNMEIL
                                             (SPA-GMN-PRTJYUN-MAX)
               END-IF
           END-IF
           ADD     1                     TO  SPA-GMN-PRTJYUN-MAX
           MOVE    "07"                  TO  SPA-GMN-PRTJYUNL
                                                 (SPA-GMN-PRTJYUN-MAX)
           MOVE    "カナ氏名順（全体）"
                                         TO  SPA-GMN-PRTJYUNMEIL
                                                 (SPA-GMN-PRTJYUN-MAX)
           ADD     1                     TO  SPA-GMN-PRTJYUN-MAX
           MOVE    "08"                  TO  SPA-GMN-PRTJYUNL
                                                 (SPA-GMN-PRTJYUN-MAX)
           MOVE    "患者番号順（全体）"  TO  SPA-GMN-PRTJYUNMEIL
                                                 (SPA-GMN-PRTJYUN-MAX)
           ADD     1                     TO  SPA-GMN-PRTJYUN-MAX
           MOVE    "09"                  TO  SPA-GMN-PRTJYUNL
                                                 (SPA-GMN-PRTJYUN-MAX)
           MOVE    "種別・カナ氏名順"    TO  SPA-GMN-PRTJYUNMEIL
                                                 (SPA-GMN-PRTJYUN-MAX)
           ADD     1                     TO  SPA-GMN-PRTJYUN-MAX
           MOVE    "10"                  TO  SPA-GMN-PRTJYUNL
                                                 (SPA-GMN-PRTJYUN-MAX)
           MOVE    "種別・患者番号順"    TO  SPA-GMN-PRTJYUNMEIL
                                                 (SPA-GMN-PRTJYUN-MAX)
      *
           MOVE    SPA-GMN-PRTJYUNLST(01)
                                         TO  SPA-GMN-PRTJYUN-G
      *
      *    印刷順（国保）一括作成時のみ
           IF      SPA-R01-FILEKBN       =   "1"
               MOVE    "01"              TO  SPA-GMN-PRTJYUN1L    (1)
               MOVE    "種別・保険者番号・カナ氏名順"
                                         TO  SPA-GMN-PRTJYUN1MEIL (1)
               MOVE    "02"              TO  SPA-GMN-PRTJYUN1L    (2)
               MOVE    "種別・保険者番号・患者番号順"
                                         TO  SPA-GMN-PRTJYUN1MEIL (2)
               MOVE    "03"              TO  SPA-GMN-PRTJYUN1L    (3)
               MOVE    "カナ氏名順"      TO  SPA-GMN-PRTJYUN1MEIL (3)
               MOVE    "04"              TO  SPA-GMN-PRTJYUN1L    (4)
               MOVE    "患者番号順"      TO  SPA-GMN-PRTJYUN1MEIL (4)
               MOVE    4                 TO  SPA-GMN-PRTJYUN1-MAX
               IF    ( SPA-R01-RECEKBN   =   ZERO    OR  "2" )
               AND   ( SPA-R01-RECE07-HSEQNUM
                                         >   ZERO            )
               AND   ( SPA-R01-RECE07-HKNJANUM
                                     NOT =   "99999999"      )
                   IF      SPA-NAI-2008-HYOUJI   =   1   OR  9
                       ADD     1         TO  SPA-GMN-PRTJYUN1-MAX
                       MOVE    "06"      TO  SPA-GMN-PRTJYUN1L
                                                 (SPA-GMN-PRTJYUN1-MAX)
                       MOVE    "編綴順"  TO  SPA-GMN-PRTJYUN1MEIL
                                                 (SPA-GMN-PRTJYUN1-MAX)
                   END-IF
               END-IF
               ADD     1                     TO  SPA-GMN-PRTJYUN1-MAX
               MOVE    "09"                  TO  SPA-GMN-PRTJYUN1L
                                                 (SPA-GMN-PRTJYUN1-MAX)
               MOVE    "種別・カナ氏名順"    TO  SPA-GMN-PRTJYUN1MEIL
                                                 (SPA-GMN-PRTJYUN1-MAX)
               ADD     1                     TO  SPA-GMN-PRTJYUN1-MAX
               MOVE    "10"                  TO  SPA-GMN-PRTJYUN1L
                                                 (SPA-GMN-PRTJYUN1-MAX)
               MOVE    "種別・患者番号順"    TO  SPA-GMN-PRTJYUN1MEIL
                                                 (SPA-GMN-PRTJYUN1-MAX)
      *
               MOVE    SPA-GMN-PRTJYUN1LST(01)
                                         TO  SPA-GMN-PRTJYUN1-G
           ELSE
               MOVE    SPA-GMN-PRTJYUNLST(01)
                                         TO  SPA-GMN-PRTJYUN1-G
           END-IF
      *
      *    印刷順（広域連合）一括作成時のみ
           IF      SPA-R01-FILEKBN       =   "1"
               MOVE    "01"              TO  SPA-GMN-PRTJYUN2L    (1)
               MOVE    "種別・保険者番号・カナ氏名順"
                                         TO  SPA-GMN-PRTJYUN2MEIL (1)
               MOVE    "02"              TO  SPA-GMN-PRTJYUN2L    (2)
               MOVE    "種別・保険者番号・患者番号順"
                                         TO  SPA-GMN-PRTJYUN2MEIL (2)
               MOVE    "03"              TO  SPA-GMN-PRTJYUN2L    (3)
               MOVE    "カナ氏名順"      TO  SPA-GMN-PRTJYUN2MEIL (3)
               MOVE    "04"              TO  SPA-GMN-PRTJYUN2L    (4)
               MOVE    "患者番号順"      TO  SPA-GMN-PRTJYUN2MEIL (4)
               MOVE    4                 TO  SPA-GMN-PRTJYUN2-MAX
      *********IF    ( SPA-R01-RECEKBN   =   ZERO    OR  "2" )
      *********AND   ( SPA-R01-RECE07-HSEQNUM
      *********                          >   ZERO            )
      *********AND   ( SPA-R01-RECE07-HKNJANUM
      *********                      NOT =   "99999999"      )
      *********    IF      SPA-NAI-2008-HYOUJI   =   1   OR  9
      *********        ADD     1         TO  SPA-GMN-PRTJYUN2-MAX
      *********        MOVE    "06"      TO  SPA-GMN-PRTJYUN2L
      *********                                  (SPA-GMN-PRTJYUN2-MAX)
      *********        MOVE    "編綴順"  TO  SPA-GMN-PRTJYUN2MEIL
      *********                                  (SPA-GMN-PRTJYUN2-MAX)
      *********    END-IF
      *********END-IF
               ADD     1                     TO  SPA-GMN-PRTJYUN2-MAX
               MOVE    "09"                  TO  SPA-GMN-PRTJYUN2L
                                                 (SPA-GMN-PRTJYUN2-MAX)
               MOVE    "種別・カナ氏名順"    TO  SPA-GMN-PRTJYUN2MEIL
                                                 (SPA-GMN-PRTJYUN2-MAX)
               ADD     1                     TO  SPA-GMN-PRTJYUN2-MAX
               MOVE    "10"                  TO  SPA-GMN-PRTJYUN2L
                                                 (SPA-GMN-PRTJYUN2-MAX)
               MOVE    "種別・患者番号順"    TO  SPA-GMN-PRTJYUN2MEIL
                                                 (SPA-GMN-PRTJYUN2-MAX)
      *
               MOVE    SPA-GMN-PRTJYUN2LST (1)
                                         TO  SPA-GMN-PRTJYUN2-G
           ELSE
               MOVE    SPA-GMN-PRTJYUNLST (1)
                                         TO  SPA-GMN-PRTJYUN2-G
           END-IF
      *
      *    県内・県外
           MOVE    "1"                   TO  SPA-GMN-PREFKBNL    (1)
           MOVE    "全部（県内分先頭）"  TO  SPA-GMN-PREFKBNMEIL (1)
           MOVE    "2"                   TO  SPA-GMN-PREFKBNL    (2)
           MOVE    "県内"                TO  SPA-GMN-PREFKBNMEIL (2)
           MOVE    "3"                   TO  SPA-GMN-PREFKBNL    (3)
           MOVE    "県外"                TO  SPA-GMN-PREFKBNMEIL (3)
           MOVE    "4"                   TO  SPA-GMN-PREFKBNL    (4)
           MOVE    "全部（順不同）"      TO  SPA-GMN-PREFKBNMEIL (4)
      *
           MOVE    SPA-GMN-PREFKBNLST(01)
                                         TO  SPA-GMN-PREFKBN-G
      *    出力内容
           MOVE    "1"                   TO  SPA-GMN-PRTKBNL    (1)
           MOVE    "標準"                TO  SPA-GMN-PRTKBNMEIL (1)
           MOVE    "2"                   TO  SPA-GMN-PRTKBNL    (2)
           MOVE    "一号用紙のみ"        TO  SPA-GMN-PRTKBNMEIL (2)
           MOVE    "3"                   TO  SPA-GMN-PRTKBNL    (3)
           MOVE    "続紙のみ"            TO  SPA-GMN-PRTKBNMEIL (3)
      *
           MOVE    SPA-GMN-PRTKBNLST(01) TO  SPA-GMN-PRTKBN-G
      *     
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYS-2005-REC
           INITIALIZE                   SYS-2005-REC
           MOVE    "2005"           TO  SYS-2005-KANRICD
           MOVE    "*"              TO  SYS-2005-KBNCD
           MOVE    SPA-SYSYMD       TO  SYS-2005-STYUKYMD
                                        SYS-2005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM      TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC     TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
               INITIALIZE                   SYS-2005-REC
           END-IF    
      *
      *    区分コード０１のデータ読み込み
           INITIALIZE                  SYS-200501-REC
           MOVE    "2005"          TO  SYS-200501-KANRICD
           MOVE    "01"            TO  SYS-200501-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-200501-STYUKYMD
                                       SYS-200501-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC  TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE   MCPDATA-REC  TO  SYS-200501-REC
           ELSE
               INITIALIZE              SYS-200501-REC
               MOVE    SYS-2005-TERMID (1:4)
                                   TO  SYS-200501-TBL (1:4)
           END-IF
      *
           MOVE    SYS-2005-SYAPRTNM    TO  SPA-NAI-2005-SYAPRTNM
           MOVE    SYS-2005-KKHPRTNM    TO  SPA-NAI-2005-KKHPRTNM
           MOVE    SYS-200501-KOUIKIPRTNM
                                        TO  SPA-NAI-2005-KOUIKIPRTNM
           MOVE    SYS-2005-SYAPRTNM-NEXT
                                        TO  SPA-NAI-2005-SYAPRTNM-NEXT
           MOVE    SYS-2005-KKHPRTNM-NEXT
                                        TO  SPA-NAI-2005-KKHPRTNM-NEXT
           MOVE    SYS-200501-KOUIKIPRTNM-NEXT
                                        TO
                                        SPA-NAI-2005-KOUIKIPRTNM-NEXT
           IF      SYS-2005-SYAPRTNM-N  =   SPACE
                   MOVE   SYS-2005-SYAPRTNM
                                        TO  SYS-2005-SYAPRTNM-N
           END-IF
           IF      SYS-2005-KKHPRTNM-N  =   SPACE
                   MOVE   SYS-2005-KKHPRTNM
                                        TO  SYS-2005-KKHPRTNM-N
           END-IF
           IF      SYS-200501-KOUIKIPRTNM-N  =   SPACE
                   MOVE   SYS-200501-KOUIKIPRTNM
                                        TO  SYS-200501-KOUIKIPRTNM-N
           END-IF
           IF      SYS-2005-SYAPRTNM-NEXT-N  =   SPACE
                   MOVE   SYS-2005-SYAPRTNM-NEXT
                                        TO  SYS-2005-SYAPRTNM-NEXT-N
           END-IF
           IF      SYS-2005-KKHPRTNM-NEXT-N  =   SPACE
                   MOVE   SYS-2005-KKHPRTNM-NEXT
                                        TO  SYS-2005-KKHPRTNM-NEXT-N
           END-IF
           IF      SYS-200501-KOUIKIPRTNM-NEXT-N  =   SPACE
                   MOVE   SYS-200501-KOUIKIPRTNM-NEXT
                                        TO
                                        SYS-200501-KOUIKIPRTNM-NEXT-N
           END-IF
           IF      SYS-200501-DRCD-PRTKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-200501-DRCD-PRTKBN
           END-IF
           IF    ( SYS-200501-DRCD-PRTKBN
                                       =   "1"   )
           AND   ( SYS-200501-DRCD-PRTJYUNKBN
                                       =   SPACE )
               MOVE    "1"             TO  SYS-200501-DRCD-PRTJYUNKBN
           END-IF
           MOVE    SYS-200501-DRCD-PRTJYUNKBN
                                       TO  SPA-NAI-2005-DRCD-PRTJYUNKBN
           IF      SYS-200501-BTUNUM-PRTKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-200501-BTUNUM-PRTKBN
           END-IF
           IF    ( SYS-200501-BTUNUM-PRTKBN
                                       =   "1"   )
           AND   ( SYS-200501-BTUNUM-PRTJYUNKBN
                                       =   SPACE )
               MOVE    "1"             TO  SYS-200501-BTUNUM-PRTJYUNKBN
           END-IF
           MOVE    SYS-200501-BTUNUM-PRTJYUNKBN
                                       TO
                                       SPA-NAI-2005-BTUNUM-PRTJYUNKBN
           IF      SYS-200501-JIHI-PRTKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-200501-JIHI-PRTKBN
           END-IF
           MOVE    SYS-200501-JIHI-PRTKBN
                                       TO  SPA-NAI-2005-JIHI-PRTKBN
      *
      *    診療科の表示（一括作成時のみ）
           IF    ( SPA-SYUKA-KBN       =   "1"             )
           AND   ( SPA-R01-FILEKBN     =   "1"             )
             IF    ( SPA-R01-RECE07-SRYKA
                                     NOT =   "00" )
             OR    ( SPA-R01-RECE07-SRYKA-KOKUHO
                                     NOT =   "00" )
             OR    ( SPA-R01-RECE07-SRYKA-KOUIKI
                                     NOT =   "00" )
               MOVE    "00"                TO  SPA-GMN-SYUKAL   (1)
               MOVE    "全科"              TO  SPA-GMN-SYUKAMEIL(1)
               MOVE    1                   TO  IDX
      *
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
                                               SYS-1005-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC 
               ELSE
                   INITIALIZE                  SYS-1005-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF 
      *
               PERFORM         UNTIL       (IDX            >=  99   )
                               OR          (FLG-SYSKANRI   =   1    )
                   ADD     1                     TO  IDX
                   MOVE    MCPDATA-REC           TO  SYS-1005-REC
                   MOVE    SYS-1005-KBNCD (1:2)  TO
                                                 SPA-GMN-SYUKAL   (IDX)
                   MOVE    SYS-1005-SRYKANAME1   TO
                                                 SPA-GMN-SYUKAMEIL(IDX)
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               ADD     1                   TO  IDX
               MOVE    "XX"                TO  SPA-GMN-SYUKAL   (IDX)
               MOVE    "主科未設定"        TO  SPA-GMN-SYUKAMEIL(IDX)
      *
               MOVE    IDX                   TO  SPA-GMN-SYUKA-MAX
      * 
               MOVE    SPA-GMN-SYUKALST (1)
                                           TO  SPA-GMN-SYUKA-G
             END-IF
           ELSE
      *      医師別出力設定のとき
             IF    ( SYS-200501-DRCD-PRTKBN
                                         =   "1"   )
             AND   ( SPA-R01-RECE08-DRCD-SRYKA
                                     NOT =   SPACE )
               MOVE    "00"                TO  SPA-GMN-SYUKAL   (1)
               MOVE    "全科"              TO  SPA-GMN-SYUKAMEIL(1)
               MOVE    1                   TO  IDX
      *
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
                                               SYS-1005-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC 
               ELSE
                   INITIALIZE                  SYS-1005-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF 
      *
               PERFORM         UNTIL       (IDX            >=  99   )
                               OR          (FLG-SYSKANRI   =   1    )
                   ADD     1                     TO  IDX
                   MOVE    MCPDATA-REC           TO  SYS-1005-REC
                   MOVE    SYS-1005-KBNCD (1:2)  TO
                                                 SPA-GMN-SYUKAL   (IDX)
                   MOVE    SYS-1005-SRYKANAME1   TO
                                                 SPA-GMN-SYUKAMEIL(IDX)
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               MOVE    IDX                   TO  SPA-GMN-SYUKA-MAX
      * 
               MOVE    SPA-GMN-SYUKALST (1)
                                           TO  SPA-GMN-SYUKA-G
             END-IF
           END-IF                                                             
      *
      *    一括作成で医師別出力設定のとき
           IF    ( SYS-200501-DRCD-PRTKBN
                                       =   "1"   )
           AND   ( SPA-R01-RECE08-DRCD-SRYKA
                                   NOT =   SPACE )
               ADD     1                     TO  SPA-GMN-PRTJYUN-MAX
               MOVE    "11"                  TO  SPA-GMN-PRTJYUNL
                                                 (SPA-GMN-PRTJYUN-MAX)
               EVALUATE    SPA-NAI-2005-DRCD-PRTJYUNKBN
                   WHEN    "1"
                       MOVE    "医師別順（カナ氏名順）"
                                               TO  SPA-GMN-PRTJYUNMEIL
                                                   (SPA-GMN-PRTJYUN-MAX)
                   WHEN    "2"
                       MOVE    "医師別順（患者番号順）"
                                               TO  SPA-GMN-PRTJYUNMEIL
                                                   (SPA-GMN-PRTJYUN-MAX)
                   WHEN    "3"
                       MOVE    "医師別順（提出先・カナ氏名順）"
                                               TO  SPA-GMN-PRTJYUNMEIL
                                                   (SPA-GMN-PRTJYUN-MAX)
                   WHEN    "4"
                       MOVE    "医師別順（提出先・患者番号順）"
                                               TO  SPA-GMN-PRTJYUNMEIL
                                                   (SPA-GMN-PRTJYUN-MAX)
               END-EVALUATE
           END-IF
      *
      *    一括作成で病棟別出力設定のとき
           IF    ( SYS-200501-BTUNUM-PRTKBN
                                       =   "1"   )
           AND   ( SPA-R01-RECE08-SORT-1
                                   NOT =   SPACE )
               ADD     1                     TO  SPA-GMN-PRTJYUN-MAX
               MOVE    "12"                  TO  SPA-GMN-PRTJYUNL
                                                 (SPA-GMN-PRTJYUN-MAX)
               EVALUATE    SPA-NAI-2005-BTUNUM-PRTJYUNKBN
                   WHEN    "1"
                       MOVE    "病棟別順（カナ氏名順）"
                                               TO  SPA-GMN-PRTJYUNMEIL
                                                   (SPA-GMN-PRTJYUN-MAX)
                   WHEN    "2"
                       MOVE    "病棟別順（患者番号順）"
                                               TO  SPA-GMN-PRTJYUNMEIL
                                                   (SPA-GMN-PRTJYUN-MAX)
               END-EVALUATE
           END-IF
      *
      *
      *    ターミナルデジット出力設定のとき
           IF    ( SYS-200501-TERMINAL-DIGIT-PRTKBN
                                       =   "1"   )
           AND   ( SPA-R01-RECE08-SORT-2 (1:2)
                                   NOT =   SPACE )
               ADD     1                     TO  SPA-GMN-PRTJYUN-MAX
               MOVE    "13"                  TO  SPA-GMN-PRTJYUNL
                                                 (SPA-GMN-PRTJYUN-MAX)
               MOVE    "ターミナルデジット"  TO  SPA-GMN-PRTJYUNMEIL
                                                 (SPA-GMN-PRTJYUN-MAX)
           END-IF
      *
      *    社保遍綴順用
           IF      SYS-2005-PRTJYUNKBN1 =   SPACE
               MOVE   "01"             TO  SPA-NAI-2005-PRTJYUNKBN1
           ELSE
               MOVE     SYS-2005-PRTJYUNKBN1
                                       TO  SPA-NAI-2005-PRTJYUNKBN1
           END-IF
           IF      SYS-2005-PRTJYUNKBN3 =   SPACE
               IF      SYS-2005-PRTJYUNKBN1 =   "06"
                   MOVE   "01"         TO  SPA-NAI-2005-PRTJYUNKBN3
               ELSE
                   MOVE     SYS-2005-PRTJYUNKBN1
                                       TO  SPA-NAI-2005-PRTJYUNKBN3
               END-IF
           ELSE
               MOVE     SYS-2005-PRTJYUNKBN3
                                       TO  SPA-NAI-2005-PRTJYUNKBN3
           END-IF
           IF      SYS-200501-PRTJYUNKBN5   =   SPACE
               IF      SYS-2005-PRTJYUNKBN1 =   "06"
                   MOVE   "01"         TO  SPA-NAI-200501-PRTJYUNKBN5
               ELSE
                   MOVE     SYS-2005-PRTJYUNKBN1
                                       TO  SPA-NAI-200501-PRTJYUNKBN5
               END-IF
           ELSE
               MOVE     SYS-200501-PRTJYUNKBN5
                                       TO  SPA-NAI-200501-PRTJYUNKBN5
           END-IF
      *
           IF      SYS-2005-PRTJYUNKBN4 =   SPACE
               IF      SYS-2005-PRTJYUNKBN1 =   SPACE
                       MOVE   "01"          TO  SYS-2005-PRTJYUNKBN1
               END-IF
               IF      SYS-2005-PRTJYUNKBN3 =   SPACE
                       MOVE    SYS-2005-PRTJYUNKBN1
                                            TO  SYS-2005-PRTJYUNKBN3
               END-IF
               IF      SYS-200501-PRTJYUNKBN5
                                            =   SPACE 
                       MOVE    SYS-2005-PRTJYUNKBN1
                                            TO  SYS-200501-PRTJYUNKBN5
               END-IF
           ELSE
               MOVE   SYS-2005-PRTJYUNKBN4  TO  SYS-2005-PRTJYUNKBN1
                                                SYS-2005-PRTJYUNKBN3
                                                SYS-200501-PRTJYUNKBN5
           END-IF
     
           IF    ( SPA-R01-NYUGAIKBN    =   "1"   )
           AND   ( SYS-200501-PRTJYUNKBN6
                                    NOT =   SPACE )
               MOVE   SYS-200501-PRTJYUNKBN6
                                            TO  SYS-2005-PRTJYUNKBN1
                                                SYS-2005-PRTJYUNKBN3
                                                SYS-200501-PRTJYUNKBN5
           END-IF           
           IF      SYS-2005-PRTJYUNKBN2 =   SPACE
                   MOVE   "01"          TO  SYS-2005-PRTJYUNKBN2
           END-IF
      *
           MOVE    SYS-2005-SYAPRTNM-N  TO  SPA-NAI-2005-SYAPRTNM-N
           MOVE    SYS-2005-KKHPRTNM-N  TO  SPA-NAI-2005-KKHPRTNM-N
           MOVE    SYS-200501-KOUIKIPRTNM-N
                                        TO  SPA-NAI-2005-KOUIKIPRTNM-N
           MOVE    SYS-2005-SYAPRTNM-NEXT-N
                                       TO  SPA-NAI-2005-SYAPRTNM-NEXT-N
           MOVE    SYS-2005-KKHPRTNM-NEXT-N
                                       TO  SPA-NAI-2005-KKHPRTNM-NEXT-N
           MOVE    SYS-200501-KOUIKIPRTNM-NEXT-N
                                       TO
                                       SPA-NAI-2005-KOUIKIPRTNM-NEXT-N
      *
           IF      SYS-200501-PRT-CHECKKBN =   SPACE
               MOVE   "0"                TO  SYS-200501-PRT-CHECKKBN
           END-IF
           MOVE    SYS-200501-PRT-CHECKKBN
                                       TO  SPA-NAI-2005-PRT-CHECKKBN
      *    
      *    出力順（一括作成）
           IF      SPA-R01-FILEKBN       =   "1" 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-PRTJYUN-MAX
                       OR      SPA-GMN-PRTJYUNL (IDX)
                                   =   SPACE
                   IF      SYS-2005-PRTJYUNKBN1
                                           =   SPA-GMN-PRTJYUNL (IDX)
                       MOVE    SPA-GMN-PRTJYUNLST(IDX)
                                               TO  SPA-GMN-PRTJYUN-G
                       MOVE    SPA-GMN-PRTJYUN-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
               IF      SYS-2005-PRTJYUNKBN1
                                           =   "07"    OR  "08"    OR  
                                               "11"    OR  "12"    OR
                                               "13"
                   MOVE    SPA-GMN-PRTJYUN-G   TO  SPA-GMN-PRTJYUN1-G
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-PRTJYUN1-MAX
                           OR      SPA-GMN-PRTJYUN1L (IDX)
                                       =   SPACE
                       IF      SYS-2005-PRTJYUNKBN3
                                           =   SPA-GMN-PRTJYUN1L (IDX)
                           MOVE    SPA-GMN-PRTJYUN1LST(IDX)
                                               TO  SPA-GMN-PRTJYUN1-G
                           MOVE    SPA-GMN-PRTJYUN1-MAX
                                               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
               IF      SYS-200501-PRTJYUNKBN5
                                           =   "07"    OR  "08"    OR  
                                               "11"    OR  "12"    OR
                                               "13"
                   MOVE    SPA-GMN-PRTJYUN-G   TO  SPA-GMN-PRTJYUN2-G
               ELSE
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-PRTJYUN2-MAX
                           OR      SPA-GMN-PRTJYUN2L (IDX)
                                       =   SPACE
                       IF      SYS-200501-PRTJYUNKBN5
                                           =   SPA-GMN-PRTJYUN2L (IDX)
                           MOVE    SPA-GMN-PRTJYUN2LST(IDX)
                                               TO  SPA-GMN-PRTJYUN2-G
                           MOVE    SPA-GMN-PRTJYUN2-MAX
                                               TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
               IF    ( SPA-R01-RECE07-SYORISBT =   "1" OR  "2" )
               AND   ( SYS-2005-PRTJYUNKBN2    =   "05"        )
                   MOVE    SYS-2005-PRTJYUNKBN1
                                           TO  SYS-2005-PRTJYUNKBN2
               END-IF
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-PRTJYUN-MAX
                       OR      SPA-GMN-PRTJYUNL (IDX)
                                   =   SPACE
                   IF      SYS-2005-PRTJYUNKBN2
                                           =   SPA-GMN-PRTJYUNL (IDX)
                       MOVE    SPA-GMN-PRTJYUNLST(IDX)
                                               TO  SPA-GMN-PRTJYUN-G
                                                   SPA-GMN-PRTJYUN1-G
                                                   SPA-GMN-PRTJYUN2-G
                       MOVE    SPA-GMN-PRTJYUN-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           MOVE    SPA-GMN-PRTJYUN     TO  SPA-GMN-PRTJYUN-OLD
      *
           IF      SPA-GMN-PRTJYUN     =   "07"    OR  "08"    OR  "13"
               MOVE    SPACE               TO  SPA-GMN-SYUKA-G
           ELSE
               IF      SPA-GMN-PRTJYUN     =  "11"
                   MOVE    SPA-GMN-SYUKALST (1) TO  SPA-GMN-SYUKA-G
               ELSE
                   IF    ( SPA-R01-RECE07-SRYKA
                                           =   "00" )
                   AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                           =   "00" )
                   AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                           =   "00" )
                       MOVE    SPACE           TO  SPA-GMN-SYUKA-G
                   ELSE
                       IF    ( SPA-GMN-SYUKA-MAX   >   ZERO )
                       AND   ( SPA-GMN-SYUKA   NOT =   "00" )
                           MOVE    SPA-GMN-SYUKALST (1)
                                                   TO  SPA-GMN-SYUKA-G
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    出力先プリンタ名割り当て情報編集
           MOVE    SPACE               TO  SYS-1031-REC
           INITIALIZE                  SYS-1031-REC
           MOVE    "1031"              TO  SYS-1031-KANRICD
           MOVE    "*"                 TO  SYS-1031-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1031-STYUKYMD
                                           SYS-1031-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1031-HOSPNUM
           MOVE    SYS-1031-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1031-REC
           ELSE    
               INITIALIZE                  SYS-1031-REC
           END-IF                                     
      *    社保印刷先
           IF      SYS-2005-SYAPRTNM   =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-SYAPRTNM
           END-IF                                              
           IF      SYS-2005-SYAPRTNM-N =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-SYAPRTNM-N
           END-IF                                              
      *    国保印刷先
           IF      SYS-2005-KKHPRTNM   =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-KKHPRTNM
           END-IF                                              
           IF      SYS-2005-KKHPRTNM-N =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-KKHPRTNM-N
           END-IF                                              
      *    広域連合印刷先
           IF      SYS-200501-KOUIKIPRTNM   =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO  SPA-NAI-2005-KOUIKIPRTNM
           END-IF                                              
           IF      SYS-200501-KOUIKIPRTNM-N =   SPACE
               MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-KOUIKIPRTNM-N
           END-IF                                              
      *    社保印刷先（続紙）
           IF      SYS-2005-SYAPRTNM-NEXT  =   SPACE
               IF      SYS-2005-SYAPRTNM       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-SYAPRTNM-NEXT
               ELSE                                              
                   MOVE    SYS-2005-SYAPRTNM   TO
                                           SPA-NAI-2005-SYAPRTNM-NEXT
               END-IF
           END-IF                                              
           IF      SYS-2005-SYAPRTNM-NEXT-N  =   SPACE
               IF      SYS-2005-SYAPRTNM-N       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-SYAPRTNM-NEXT-N
               ELSE                                              
                   MOVE    SYS-2005-SYAPRTNM-N   TO
                                           SPA-NAI-2005-SYAPRTNM-NEXT-N
               END-IF
           END-IF                                              
      *    国保印刷先（続紙）
           IF      SYS-2005-KKHPRTNM-NEXT  =   SPACE
               IF      SYS-2005-KKHPRTNM       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-KKHPRTNM-NEXT
               ELSE                                              
                   MOVE    SYS-2005-KKHPRTNM   TO
                                           SPA-NAI-2005-KKHPRTNM-NEXT
               END-IF
           END-IF                                              
           IF      SYS-2005-KKHPRTNM-NEXT-N  =   SPACE
               IF      SYS-2005-KKHPRTNM-N       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-KKHPRTNM-NEXT-N
               ELSE                                              
                   MOVE    SYS-2005-KKHPRTNM-N  TO
                                           SPA-NAI-2005-KKHPRTNM-NEXT-N
               END-IF
           END-IF
      *    広域連合印刷先（続紙）
           IF      SYS-200501-KOUIKIPRTNM-NEXT  =   SPACE
               IF      SYS-200501-KOUIKIPRTNM       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                           SPA-NAI-2005-KOUIKIPRTNM-NEXT
               ELSE                                              
                   MOVE    SYS-200501-KOUIKIPRTNM   TO
                                           SPA-NAI-2005-KOUIKIPRTNM-NEXT
               END-IF
           END-IF                                              
           IF      SYS-200501-KOUIKIPRTNM-NEXT-N  =   SPACE
               IF      SYS-200501-KOUIKIPRTNM-N       =   SPACE
                   MOVE    SYS-1031-PRTNM (4)  TO
                                       SPA-NAI-2005-KOUIKIPRTNM-NEXT-N
               ELSE                                              
                   MOVE    SYS-200501-KOUIKIPRTNM-N  TO
                                       SPA-NAI-2005-KOUIKIPRTNM-NEXT-N
               END-IF
           END-IF
      *
      *    プリンター名取得
           INITIALIZE                      ORCSPRTNMGETAREA
           MOVE    SPA-SYSYMD          TO  PRTNMGET-YMD
           CALL    "ORCSPRTNMGET"      USING   ORCSPRTNMGETAREA
                                               SPA-AREA
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       PRTNMGET-PRTNM-MAX
               COMPUTE IDY =   IDX     -   1
               MOVE    IDY                 TO  WRK-PRTNM
               MOVE    WRK-PRTNM-X         TO  SPA-GMN-PRTNML (IDX)
               MOVE    PRTNMGET-PRTNM (IDX)    TO
                                           SPA-GMN-PRTNMMEIL (IDX)
               MOVE    IDX                 TO  SPA-GMN-PRTNM-MAX
           END-PERFORM
      *
      *    初期値のプリンタ名設定
           EVALUATE    SPA-R01-NYUGAIKBN
               WHEN    "1"
                   MOVE    SPA-NAI-2005-SYAPRTNM-N
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (1)
                   MOVE    SPA-NAI-2005-SYAPRTNM-NEXT-N
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (2)
                   MOVE    SPA-NAI-2005-KKHPRTNM-N
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (3)
                   MOVE    SPA-NAI-2005-KKHPRTNM-NEXT-N
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (4)
                   MOVE    SPA-NAI-2005-KOUIKIPRTNM-N
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (5)
                   MOVE    SPA-NAI-2005-KOUIKIPRTNM-NEXT-N
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (6)
               WHEN    "2"
                   MOVE    SPA-NAI-2005-SYAPRTNM
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (1)
                   MOVE    SPA-NAI-2005-SYAPRTNM-NEXT
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (2)
                   MOVE    SPA-NAI-2005-KKHPRTNM
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (3)
                   MOVE    SPA-NAI-2005-KKHPRTNM-NEXT
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (4)
                   MOVE    SPA-NAI-2005-KOUIKIPRTNM
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (5)
                   MOVE    SPA-NAI-2005-KOUIKIPRTNM-NEXT
                                           TO  WRK-PRTNMMEI
                   PERFORM 5001-PRTNM-HENSYU-SEC
                   MOVE    WRK-GMN-PRTNM-G TO  SPA-NAI-PRTNM-G (6)
           END-EVALUATE
      *
      *    広域連合の印刷順の設定
           IF      SPA-R01-RECE07-SRYYM    <   WRK-CONS-SRYYM-200804
               MOVE    SPACE               TO  SPA-GMN-PRTJYUN2-G
               MOVE    ZERO                TO  SPA-GMN-PRTJYUN2-MAX
           END-IF
      *
      *    医師別出力の編集処理
           IF    ( SYS-200501-DRCD-PRTKBN
                                       =   "1"   )
           AND   ( SPA-R01-RECE08-DRCD-SRYKA
                                   NOT =   SPACE )
               PERFORM 3102-DRCD-HENSYU-SEC
           END-IF
      *
      *    病棟別出力の編集処理
           IF    ( SYS-200501-BTUNUM-PRTKBN
                                       =   "1"   )
           AND   ( SPA-R01-RECE08-SORT-1
                                   NOT =   SPACE )
               PERFORM 3104-BTUNUM-HENSYU-SEC
           END-IF
      *
      *    頁・件数の編集処理
           MOVE    "T"                 TO  SPA-GMN-SYOKBN
           MOVE    ZERO                TO  FLG-PRTNM-SET
           PERFORM 3101-HENSYU-SEC
      *
      *    不明分、主科未設定分ありのメッセージ
           IF    ( SPA-R01-RECE07-SRYKA    =   "XX" )
           OR    ( SPA-R01-RECE07-SRYKA-KOKUHO
                                           =   "XX" )
           OR    ( SPA-R01-RECE07-SRYKA-KOUIKI
                                           =   "XX" )
               MOVE    "主科未設定分があります"    TO  SPA-GMN-MSG
           END-IF
           IF      SPA-GMN-TPAGE (1 36)    >   ZERO
               IF      SPA-GMN-MSG             =   SPACE
                   MOVE    "種別不明分があります"      TO  SPA-GMN-MSG
               ELSE
                   MOVE    "種別不明分、主科未設定分があります"
                                                       TO  SPA-GMN-MSG
               END-IF
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別取得処理
      *            レセプト編綴順情報（詳細設定）
      *****************************************************************
       3103-2003-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX
           INITIALIZE                      SPA-NAI-2003-TABLE 
      * 
      *    レセプト作成指示（印刷順）読み込み
           MOVE    SPACE               TO  SYS-2003-KEY
           MOVE    "2003"              TO  SYS-2003-KANRICD
           MOVE    SPA-R01-SRYYM       TO  SYS-2003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2003-STYUKYMD (7:2) 
           MOVE    SYS-2003-STYUKYMD   TO  SYS-2003-EDYUKYMD
      *
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2003-HOSPNUM
           MOVE    SYS-2003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-2003-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM    UNTIL   FLG-SYSKANRI     =   1
               MOVE    MCPDATA-REC   TO  SYS-2003-REC
               IF    ( SYS-2003-TEISYUTUSAKI   =   "2"          )
               AND   ( SYS-2003-NYUGAIKBN      =
                                       SPA-R01-RECE07-NYUGAIKBN )
                   MOVE    SYS-2003-DSPSEQ         TO  IDX
                   MOVE    SYS-2003-SYUBETU (1)    TO
                                       SPA-NAI-2003-TSYUBETU     (IDX 1)
                   MOVE    SYS-2003-SYUBETU (2)    TO
                                       SPA-NAI-2003-TSYUBETU     (IDX 2)
                   MOVE    SYS-2003-SYUBETU (3)    TO
                                       SPA-NAI-2003-TSYUBETU     (IDX 3)
                   MOVE    SYS-2003-SYURUINAME     TO
                                       SPA-NAI-2003-TSYUBETUNAME (IDX)
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           . 
       3103-2003-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理
      *****************************************************************
       3101-HENSYU-SEC                 SECTION.
       
           EVALUATE    SPA-GMN-PRTJYUN
               WHEN    "07"
               WHEN    "08"
               WHEN    "11"
               WHEN    "12"
               WHEN    "13"
                   MOVE    SPA-GMN-PREFKBNLST (4)
                                               TO  SPA-GMN-PREFKBN-G
               WHEN    OTHER
                   MOVE    SPA-GMN-PREFKBNLST (1)
                                               TO  SPA-GMN-PREFKBN-G
           END-EVALUATE
           MOVE    SPA-GMN-PRTKBNLST  (1)
                                       TO  SPA-GMN-PRTKBN-G
      *
           INITIALIZE                  SPA-GMN-TBL
                                       SPA-GMN-TBL1
                                       SPA-NAI-TBL   
           MOVE    ZERO                TO  SPA-GMN-PAGEH
                                           SPA-GMN-KENSUH     
                                           SPA-GMN-PAGE-MAX
                                           SPA-GMN-PAGE-MAX1
           IF      FLG-PRTNM-SET       =   ZERO
               INITIALIZE                  SPA-GMN-TBL2
           END-IF                          
      *
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       WRK-CONS-PAGE-MAX
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-LINE-MAX
                   MOVE    "F"         TO  SPA-GMN-TCHK-VALUE (IDW IDX)
               END-PERFORM
           END-PERFORM
      *
      *    社保分
           MOVE    "1"                 TO  WRK-TEISYUTUSAKI
           EVALUATE    SPA-GMN-PRTJYUN
               WHEN    "01"
               WHEN    "02"
               WHEN    "05"
               WHEN    "09"
               WHEN    "10"
                   PERFORM 3101-PAGE-HENSYU-01-SEC     
               WHEN    "03"
               WHEN    "04"
                   PERFORM 3101-PAGE-HENSYU-02-SEC     
               WHEN    "06"
                   PERFORM 3101-PAGE-HENSYU-07-SEC     
               WHEN    "07"
               WHEN    "08"
                   PERFORM 3101-PAGE-HENSYU-04-SEC     
               WHEN    "11"
                   PERFORM 3101-PAGE-HENSYU-05-SEC     
               WHEN    "12"
                   PERFORM 3101-PAGE-HENSYU-06-SEC     
               WHEN    "13"
                   PERFORM 3101-PAGE-HENSYU-08-SEC     
           END-EVALUATE
      *
           EVALUATE    SPA-GMN-PRTJYUN
               WHEN    "01"
               WHEN    "02"
               WHEN    "06"
               WHEN    "09"
               WHEN    "10"
                   CONTINUE
               WHEN    "05"
                   MOVE    1           TO  SPA-GMN-PAGE-MAX
               WHEN    OTHER
                   MOVE    1           TO  SPA-GMN-PAGE-MAX
           END-EVALUATE 
      *    
      *    国保分
           MOVE    "2"                 TO  WRK-TEISYUTUSAKI
           EVALUATE    SPA-GMN-PRTJYUN1
               WHEN    "01"
               WHEN    "02"
               WHEN    "05"
               WHEN    "09"
               WHEN    "10"
                   PERFORM 3101-PAGE-HENSYU-01-SEC     
               WHEN    "06"
                   PERFORM 3101-PAGE-HENSYU-03-SEC     
               WHEN    "03"
               WHEN    "04"
                   PERFORM 3101-PAGE-HENSYU-02-SEC     
           END-EVALUATE
      *
           IF      SPA-R01-RECE07-SRYYM    <   WRK-CONS-SRYYM-200804
               EVALUATE    SPA-GMN-PRTJYUN ALSO    SPA-GMN-PRTJYUN1
                   WHEN    ANY             ALSO    "06"
                   WHEN    "11"            ALSO    ANY
                       CONTINUE
                   WHEN    "01"            ALSO    ANY
                   WHEN    "02"            ALSO    ANY
                   WHEN    "05"            ALSO    ANY
                   WHEN    "09"            ALSO    ANY
                   WHEN    "10"            ALSO    ANY
                       MOVE    2                   TO  SPA-GMN-PAGE-MAX
                   WHEN    "03"            ALSO    "01"
                   WHEN    "03"            ALSO    "02"
                   WHEN    "03"            ALSO    "09"
                   WHEN    "03"            ALSO    "10"
                   WHEN    "04"            ALSO    "01"
                   WHEN    "04"            ALSO    "02"
                   WHEN    "04"            ALSO    "09"
                   WHEN    "04"            ALSO    "10"
                       MOVE    2                   TO  SPA-GMN-PAGE-MAX
                   WHEN    "03"            ALSO    ANY
                   WHEN    "04"            ALSO    ANY
                   WHEN    "07"            ALSO    ANY
                   WHEN    "08"            ALSO    ANY
                       MOVE    1                   TO  SPA-GMN-PAGE-MAX
               END-EVALUATE
           ELSE    
               EVALUATE    SPA-GMN-PRTJYUN ALSO    SPA-GMN-PRTJYUN1
                                             ALSO    SPA-GMN-PRTJYUN2
                   WHEN    "03"            ALSO    "01"    ALSO    ANY
                   WHEN    "03"            ALSO    "02"    ALSO    ANY
                   WHEN    "03"            ALSO    "09"    ALSO    ANY
                   WHEN    "03"            ALSO    "10"    ALSO    ANY
                   WHEN    "04"            ALSO    "01"    ALSO    ANY
                   WHEN    "04"            ALSO    "02"    ALSO    ANY
                   WHEN    "04"            ALSO    "09"    ALSO    ANY
                   WHEN    "04"            ALSO    "10"    ALSO    ANY
      *************    MOVE    3                   TO  SPA-GMN-PAGE-MAX
                       CONTINUE
                   WHEN    "05"            ALSO    ANY    ALSO     ANY
                       MOVE    2                   TO  SPA-GMN-PAGE-MAX
                   WHEN    "03"            ALSO    "03"    ALSO    "03"
                   WHEN    "03"            ALSO    "04"    ALSO    "03"
                   WHEN    "03"            ALSO    "03"    ALSO    "04"
                   WHEN    "03"            ALSO    "04"    ALSO    "04"
                   WHEN    "04"            ALSO    "03"    ALSO    "03"
                   WHEN    "04"            ALSO    "04"    ALSO    "03"
                   WHEN    "04"            ALSO    "03"    ALSO    "04"
                   WHEN    "04"            ALSO    "04"    ALSO    "04"
                   WHEN    "07"            ALSO    ANY     ALSO    ANY
                   WHEN    "08"            ALSO    ANY     ALSO    ANY
                       MOVE    1                   TO  SPA-GMN-PAGE-MAX
               END-EVALUATE
      *
      *        広域連合
               MOVE    "6"                 TO  WRK-TEISYUTUSAKI
               EVALUATE    SPA-GMN-PRTJYUN2
                   WHEN    "01"
                   WHEN    "02"
                   WHEN    "05"
                   WHEN    "09"
                   WHEN    "10"
                       PERFORM 3101-PAGE-HENSYU-01-SEC     
                   WHEN    "03"
                   WHEN    "04"
                       PERFORM 3101-PAGE-HENSYU-02-SEC     
               END-EVALUATE
           END-IF     
      *    
      *    特別療養費・自費・不明分の編集     
           PERFORM 31011-PAGE-HENSYU-SEC
      *
           MOVE    199             TO  SPA-GMN-CUR
      *
           MOVE    1               TO  SPA-GMN-PAGE     
           .
       3101-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（レセプト種別）
      *****************************************************************
       3101-PAGE-HENSYU-01-SEC            SECTION.
      *
      *    ページ数・件数設定
           EVALUATE    WRK-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    1               TO  IDW
               WHEN    "2"
      *************MOVE    2               TO  IDW
                   MOVE    SPA-GMN-PAGE-MAX
                                           TO  IDW
                   ADD     1               TO  IDW 
               WHEN    "6"
                   MOVE    SPA-GMN-PAGE-MAX
                                           TO  IDW
                   ADD     1               TO  IDW 
           END-EVALUATE
      *
      *    レセプト作成指示（印刷順）読み込み
           MOVE    SPACE               TO  SYS-2003-KEY
           MOVE    "2003"              TO  SYS-2003-KANRICD
           MOVE    SPA-R01-RECE07-SRYYM
                                       TO  SYS-2003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2003-STYUKYMD (7:2)
           MOVE    SYS-2003-STYUKYMD   TO  SYS-2003-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2003-HOSPNUM
      * 
           MOVE    SYS-2003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-2003-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *                                           
           PERFORM    UNTIL   FLG-SYSKANRI     =   1
               MOVE    MCPDATA-REC   TO  SYS-2003-REC
               IF    ( SYS-2003-TEISYUTUSAKI   =   WRK-TEISYUTUSAKI ) 
               AND   ( SYS-2003-NYUGAIKBN      =
                                           SPA-R01-RECE07-NYUGAIKBN )
                   MOVE    SYS-2003-DSPSEQ         TO  IDX
                   MOVE    SYS-2003-DSPSEQ (2:2)   TO                         
                                           SPA-NAI-PRTJYUNI    (IDW IDX)
                   MOVE    SYS-2003-TEISYUTUSAKI   TO
                                           SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    SYS-2003-SYUBETU (1)    TO
                                           SPA-NAI-SYUBETU   (IDW IDX 1)
                   MOVE    SYS-2003-SYUBETU (2)    TO
                                           SPA-NAI-SYUBETU   (IDW IDX 2)
                   MOVE    SYS-2003-SYUBETU (3)    TO
                                           SPA-NAI-SYUBETU   (IDW IDX 3)
                   MOVE    SYS-2003-SYUBETU (4)    TO
                                           SPA-NAI-SYUBETU   (IDW IDX 4)
                   MOVE    SYS-2003-SYUBETU (5)    TO
                                           SPA-NAI-SYUBETU   (IDW IDX 5)
                   MOVE    SYS-2003-SYURUINAME     TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
                   MOVE    "T"         TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
               END-IF    
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME     
           PERFORM 900-CLOSE-SEC
      *
      *    社保の直接請求分があればページを分けて表示
           IF      WRK-TEISYUTUSAKI    =   "1"
               PERFORM VARYING IDYY    FROM    1   BY  1
                       UNTIL   IDYY    >
                                       SPA-R01-RECE07-HKNJANUM-MAX
                   IF      IDW             =   1
                       MOVE    SPA-R01-RECE07-HKNJANUM-SYAHO (IDYY)
                                                   TO
                                           SPA-NAI-THKNJANUM-SYAHO (IDW)
                       IF      SPA-R01-RECE07-HKNJANUM-SRT
                                                   =   "8"
                           MOVE    "直接請求する保険者以外"
                                                       TO
                                           SPA-GMN-THKNJANAME-SYAHO(IDW)
                       END-IF
                   ELSE
                       MOVE    SPA-NAI-TBL-OCC (1) TO
                                                   SPA-NAI-TBL-OCC (IDW)
                       MOVE    SPA-GMN-TBL-OCC (1) TO
                                                   SPA-GMN-TBL-OCC (IDW)
                       MOVE    SPA-GMN-TBL-OCC1(1) TO
                                                   SPA-GMN-TBL-OCC1(IDW)
                       MOVE    SPA-R01-RECE07-HKNJANUM-SYAHO (IDYY)
                                                   TO
                                           SPA-NAI-THKNJANUM-SYAHO (IDW)
                       INITIALIZE                      HKNJAINF-REC
                       MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
                       MOVE    SPA-NAI-THKNJANUM-SYAHO (IDW)
                                                   TO  HKNJA-HKNJANUM
                       MOVE    HKNJAINF-REC        TO  MCPDATA-REC
                       PERFORM 900-HKNJAINF-READ-SEC
                       MOVE    HKNJA-HKNJANAME     TO
                                           SPA-GMN-THKNJANAME-SYAHO(IDW)
                   END-IF
      * 
                   PERFORM 31011-PAGE-HENSYU-01-KENSU-SEC
      *
                   ADD     1               TO  IDW
               END-PERFORM
           ELSE
               PERFORM 31011-PAGE-HENSYU-01-KENSU-SEC
           END-IF    
           .
       3101-PAGE-HENSYU-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（社保・国保別）
      *****************************************************************
       31011-PAGE-HENSYU-01-KENSU-SEC  SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-LINE-MAX
                   OR      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
               MOVE    ZERO                TO  SPA-GMN-TPAGE  (IDW IDX)
               MOVE    ZERO                TO  SPA-GMN-TKENSU (IDW IDX)
               INITIALIZE                      SPA-NAI-TPAGE-G(IDW IDX)
      *
               PERFORM VARYING IDY FROM    1   BY  1
      *****            UNTIL   IDY >       5
                       UNTIL   IDY >       1
                       OR      SPA-NAI-SYUBETU (IDW IDX IDY)   =   SPACE 
                   INITIALIZE                  RECE07-REC
                   MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
                   MOVE    SPA-R01-RECE07-NYUGAIKBN
                                               TO  RECE07-NYUGAIKBN
                   MOVE    SPA-NAI-TEISYUTUSAKI (IDW IDX)
                                               TO  RECE07-TEISYUTUSAKI
                   MOVE    SPA-NAI-SYUBETU      (IDW IDX IDY)
                                               TO  RECE07-RECESYUBETU
                   EVALUATE    SPA-NAI-TEISYUTUSAKI (IDW IDX)
                       WHEN    "1"
                           EVALUATE    SPA-GMN-SYUKA
                               WHEN    SPACE
                               WHEN    "00"
                                   MOVE    "%%"        TO  RECE07-SRYKA
                               WHEN    OTHER
                                   IF      SPA-R01-RECE07-SRYKA
                                                       NOT =   "00"
                                       MOVE    SPA-GMN-SYUKA
                                                       TO  RECE07-SRYKA
                                   ELSE
                                       MOVE    "%%"    TO  RECE07-SRYKA
                                   END-IF 
                           END-EVALUATE
                       WHEN    "2" 
                           EVALUATE    SPA-GMN-SYUKA
                               WHEN    SPACE
                               WHEN    "00"
                                   MOVE    "%%"        TO  RECE07-SRYKA
                               WHEN    OTHER
                                   IF      SPA-R01-RECE07-SRYKA-KOKUHO
                                                       NOT =   "00"
                                       MOVE    SPA-GMN-SYUKA
                                                       TO  RECE07-SRYKA
                                   ELSE
                                       MOVE    "%%"    TO  RECE07-SRYKA
                                   END-IF 
                           END-EVALUATE
                       WHEN    "6" 
                           EVALUATE    SPA-GMN-SYUKA
                               WHEN    SPACE
                               WHEN    "00"
                                   MOVE    "%%"        TO  RECE07-SRYKA
                               WHEN    OTHER
                                   IF      SPA-R01-RECE07-SRYKA-KOUIKI
                                                       NOT =   "00"
                                       MOVE    SPA-GMN-SYUKA
                                                       TO  RECE07-SRYKA
                                   ELSE
                                       MOVE    "%%"    TO  RECE07-SRYKA
                                   END-IF 
                           END-EVALUATE
                   END-EVALUATE
                   IF      WRK-TEISYUTUSAKI    =   "1"
                       MOVE    SPA-NAI-THKNJANUM-SYAHO (IDW)
                                                   TO  RECE07-HKNJANUM
                   ELSE
                       MOVE    "%%%%%%%%"          TO  RECE07-HKNJANUM
                   END-IF
                   MOVE    RECE07-REC          TO  MCPDATA-REC                             
                   MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
                   MOVE    "key9"              TO  WRK-MCP-PATHNAME
                   PERFORM 900-RECE07-SELECT-SEC
                   PERFORM             UNTIL   FLG-RECE07  =   1
                       PERFORM 31011-SYUKA-PAGE-HENSYU-SEC
      *
                       MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                       MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                       PERFORM 900-RECE07-NEXT-SEC
                   END-PERFORM
      *
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-PERFORM
      *
      *        プリンタ設定
      *********IF      FLG-PRTNM-SET           =   ZERO
                   EVALUATE    SPA-NAI-TEISYUTUSAKI  (IDW IDX)
                       WHEN    "1"
                           MOVE    SPA-NAI-PRTNM-G (1)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                           MOVE    SPA-NAI-PRTNM-G (2)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                       WHEN    "2"
                           MOVE    SPA-NAI-PRTNM-G (3)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                           MOVE    SPA-NAI-PRTNM-G (4)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                       WHEN    "6"
                           MOVE    SPA-NAI-PRTNM-G (5)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                           MOVE    SPA-NAI-PRTNM-G (6)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                   END-EVALUATE
      *********END-IF
           END-PERFORM
      *
           MOVE    IDW             TO  SPA-GMN-PAGE-MAX
           .
       31011-PAGE-HENSYU-01-KENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（社保・国保別）
      *****************************************************************
       3101-PAGE-HENSYU-02-SEC            SECTION.
      *
      *    表示ページ・行の設定
           EVALUATE    WRK-TEISYUTUSAKI    ALSO    SPA-GMN-PRTJYUN
               WHEN    "1"                 ALSO    ANY
                   MOVE    1               TO  IDW
                   MOVE    1               TO  IDX 
               WHEN    "2"                 ALSO    "01"
               WHEN    "2"                 ALSO    "02"
               WHEN    "2"                 ALSO    "05"
               WHEN    "2"                 ALSO    "09"
               WHEN    "2"                 ALSO    "10"
                   COMPUTE IDW =   SPA-GMN-PAGE-MAX    +   1
                   MOVE    1               TO  IDX 
               WHEN    "2"                 ALSO    "03"
               WHEN    "2"                 ALSO    "04"
                   MOVE    1               TO  IDW
                   MOVE    2               TO  IDX
               WHEN    "2"                 ALSO    "06"
                   IF      SPA-R01-RECE07-HKNJANUM-SRT
                                               =   "9"
                       MOVE    1                   TO  IDW
                       IF      SPA-R01-SRYYM   <   WRK-CONS-SRYYM-201204
                           IF      SPA-R01-NYUGAIKBN   =   "1"
                               MOVE    2                   TO  IDX
                           ELSE 
                               MOVE    3                   TO  IDX
                           END-IF
                       ELSE
                           MOVE    2                   TO  IDX
                       END-IF
                   ELSE
                       COMPUTE IDW =   SPA-GMN-PAGE-MAX    +   1
                       MOVE    1               TO  IDX
                   END-IF
               WHEN    "6"                 ALSO    ANY
                   EVALUATE    SPA-GMN-PRTJYUN1
                       WHEN    "01"
                       WHEN    "02"
                       WHEN    "05"
                       WHEN    "09"
                       WHEN    "10"
      ***                  MOVE    SPA-GMN-PAGE-MAX    TO  IDW
      ***                  MOVE    1               TO  IDX 
                       WHEN    "06"
                           COMPUTE IDW =   SPA-GMN-PAGE-MAX    +   1
                           MOVE    1               TO  IDX 
                       WHEN    "03"
                       WHEN    "04"
                           MOVE    SPA-GMN-PAGE-MAX    TO  IDW
                           EVALUATE    SPA-GMN-PAGE-MAX
                               WHEN    1
                                   IF    ( SPA-GMN-PRTJYUN =   "06"  )
                                   AND   ( SPA-R01-SRYYM   <
                                               WRK-CONS-SRYYM-201204 )
                                       IF      SPA-R01-NYUGAIKBN
                                                               =   "1"
                                           MOVE    3       TO  IDX
                                       ELSE
                                           MOVE    4       TO  IDX
                                       END-IF
                                   ELSE
                                       MOVE    3           TO  IDX
                                   END-IF
                               WHEN    OTHER
                                   MOVE    2               TO  IDX
                           END-EVALUATE
                   END-EVALUATE
           END-EVALUATE
      *
           EVALUATE    WRK-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "01"        TO  SPA-NAI-PRTJYUNI    (IDW IDX)
                   MOVE    "1"         TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    "9999"      TO  SPA-NAI-SYUBETU
                                                           (IDW IDX 1)
                   MOVE    "社保"      TO  SPA-GMN-TSYURUINAME (IDW IDX)
                   MOVE    "T"         TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
               WHEN    "2"
                   MOVE    "02"        TO  SPA-NAI-PRTJYUNI    (IDW IDX)
                   MOVE    "2"         TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    "9999"      TO  SPA-NAI-SYUBETU
                                                           (IDW IDX 1)
                   MOVE    "国保"      TO  SPA-GMN-TSYURUINAME (IDW IDX)
                   MOVE    "T"         TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
               WHEN    "6"
                   MOVE    "03"        TO  SPA-NAI-PRTJYUNI    (IDW IDX)
                   MOVE    "6"         TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    "9999"      TO  SPA-NAI-SYUBETU
                                                           (IDW IDX 1)
                   MOVE    "広域"      TO  SPA-GMN-TSYURUINAME (IDW IDX)
                   MOVE    "T"         TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
           END-EVALUATE
      *
      *    ページ数・件数設定
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    SPA-NAI-TEISYUTUSAKI(IDW IDX)
                                       TO  RECE07-TEISYUTUSAKI
           MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
           EVALUATE    SPA-NAI-TEISYUTUSAKI (IDW IDX)
               WHEN    "1"
                   EVALUATE    SPA-GMN-SYUKA
                       WHEN    SPACE
                       WHEN    "00"
                           MOVE    "%%"        TO  RECE07-SRYKA
                       WHEN    OTHER
                           IF      SPA-R01-RECE07-SRYKA
                                               NOT =   "00"
                               MOVE    SPA-GMN-SYUKA   TO  RECE07-SRYKA
                           ELSE
                               MOVE    "%%"            TO  RECE07-SRYKA
                           END-IF 
                   END-EVALUATE
               WHEN    "2" 
                   EVALUATE    SPA-GMN-SYUKA
                       WHEN    SPACE
                       WHEN    "00"
                           MOVE    "%%"        TO  RECE07-SRYKA
                       WHEN    OTHER
                           IF      SPA-R01-RECE07-SRYKA-KOKUHO
                                               NOT =   "00"
                               MOVE    SPA-GMN-SYUKA   TO  RECE07-SRYKA
                           ELSE
                               MOVE    "%%"            TO  RECE07-SRYKA
                           END-IF 
                   END-EVALUATE
               WHEN    "6" 
                   EVALUATE    SPA-GMN-SYUKA
                       WHEN    SPACE
                       WHEN    "00"
                           MOVE    "%%"        TO  RECE07-SRYKA
                       WHEN    OTHER
                           IF      SPA-R01-RECE07-SRYKA-KOUIKI
                                               NOT =   "00"
                               MOVE    SPA-GMN-SYUKA   TO  RECE07-SRYKA
                           ELSE
                               MOVE    "%%"            TO  RECE07-SRYKA
                           END-IF 
                   END-EVALUATE
           END-EVALUATE
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               IF      RECE07-RECESYUBETU  =   "6666"
                   CONTINUE
               ELSE
                   PERFORM 31011-SYUKA-PAGE-HENSYU-SEC
               END-IF
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    プリンタ設定
      *****IF      FLG-PRTNM-SET           =   ZERO
               EVALUATE    SPA-NAI-TEISYUTUSAKI  (IDW IDX)
                   WHEN    "1"
                       MOVE    SPA-NAI-PRTNM-G (1)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (2)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                   WHEN    "2"
                       MOVE    SPA-NAI-PRTNM-G (3)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (4)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                   WHEN    "6"
                       MOVE    SPA-NAI-PRTNM-G (5)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (6)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
               END-EVALUATE
      *****END-IF
      *
           MOVE    IDW             TO  SPA-GMN-PAGE-MAX
           .
       3101-PAGE-HENSYU-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（国保・編綴順）
      *****************************************************************
       3101-PAGE-HENSYU-03-SEC            SECTION.
      *
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       WRK-CONS-PAGE-MAX
               INITIALIZE              SPA-GMN-TBL-OCC2 (IDX)
           END-PERFORM
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "2"                 TO  RECE07-TEISYUTUSAKI
           MOVE    SPA-R01-RECE07-NYUGAIKBN   
                                       TO  RECE07-NYUGAIKBN
           EVALUATE    SPA-GMN-SYUKA
               WHEN    SPACE
               WHEN    "00"
                   MOVE    "%%"        TO  RECE07-SRYKA
               WHEN    OTHER
                   IF      SPA-R01-RECE07-SRYKA-KOKUHO
                                       NOT =   "00"
                       MOVE    SPA-GMN-SYUKA   TO  RECE07-SRYKA
                   ELSE
                       MOVE    "%%"            TO  RECE07-SRYKA
                   END-IF 
           END-EVALUATE
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    ZERO                TO  IDX
      *****MOVE    1                   TO  IDW
           MOVE    SPA-GMN-PAGE-MAX    TO  IDW
           PERFORM         UNTIL   FLG-RECE07  =   1
                           OR      IDW         >=  WRK-CONS-PAGE-MAX
               ADD     1                   TO  IDW
               MOVE    ZERO                TO  IDX
               PERFORM         UNTIL   FLG-RECE07  =   1
                               OR      IDX     >=  WRK-CONS-LINE-MAX
                   ADD     1               TO  IDX
                   MOVE    RECE07-HKNJANUM TO
                                           SPA-NAI-THKNJANUM(IDW IDX)
                   MOVE    RECE07-PREFKBN-SRT (1:1)
                                           TO
                                           SPA-NAI-TPREFKBN-SRT(IDW IDX)
      *
                   MOVE    RECE07-PREFKBN  TO  SPA-NAI-TPREFKBN(IDW IDX)
                   MOVE    RECE07-SYUBETU-SRT1
                                           TO  SPA-NAI-TSYUBETU-SRT1
                                                               (IDW IDX)
      *
                   IF      SPA-NAI-THKNJANUM(IDW IDX)
                                               =   SPACE
                       MOVE    "保険者設定なし"    TO  
                                               SPA-GMN-TSYURUINAME
                                                        (IDW IDX)
                       MOVE    "1"                 TO   
                                               SPA-NAI-THKNJANUM-ERR
                                                       (IDW IDX)
                   ELSE
                       IF      SPA-NAI-TSYUBETU-SRT1 (IDW IDX)
                                               =   SPACE
                           MOVE    SPACE           TO  HKNJAINF-REC
                           INITIALIZE                  HKNJAINF-REC
grpsys                     MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
                           MOVE    SPA-NAI-THKNJANUM(IDW IDX)
                                                   TO  HKNJA-HKNJANUM
                           MOVE    HKNJAINF-REC    TO  MCPDATA-REC
                           PERFORM 900-HKNJAINF-READ-SEC
                           IF      FLG-HKNJAINF     =   ZERO
                               MOVE    HKNJA-HKNJANAME
                                               TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
                           ELSE
                               MOVE    SPA-NAI-THKNJANUM(IDW IDX)
                                               TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
                               MOVE    "1"     TO   
                                           SPA-NAI-THKNJANUM-ERR
                                                               (IDW IDX)
                           END-IF
                       ELSE
                           PERFORM VARYING IDZ FROM    1   BY  1
                                   UNTIL   IDZ >       50
                                   OR  SPA-NAI-2003-TSYUBETU (IDZ 1)
                                               =       SPACE
                               PERFORM VARYING IDXX    FROM    1   BY  1
                                       UNTIL   IDXX    >       5
                                       OR  SPA-NAI-2003-TSYUBETU 
                                                              (IDZ IDXX)
                                                       =   SPACE
                                   IF      SPA-NAI-TSYUBETU-SRT1
                                                             (IDW IDX)
                                           =   SPA-NAI-2003-TSYUBETU
                                                             (IDZ IDXX)
                                       MOVE    SPA-NAI-2003-TSYUBETUNAME
                                                             (IDZ)
                                                           TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
                                       MOVE    50          TO  IDZ
                                   END-IF
                               END-PERFORM
                           END-PERFORM
                           IF      SPA-GMN-TSYURUINAME (IDW IDX)
                                                       =   SPACE
                               MOVE    SPA-NAI-THKNJANUM(IDW IDX)
                                               TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
                               MOVE    "1"     TO   
                                           SPA-NAI-THKNJANUM-ERR
                                                               (IDW IDX)
                           END-IF
                       END-IF
                   END-IF
      *
                   MOVE    IDX (3:2)       TO  SPA-NAI-PRTJYUNI(IDW IDX)
                   MOVE    "2"             TO
                                       SPA-NAI-TEISYUTUSAKI(IDW IDX)
                   MOVE    "9999"          TO
                                       SPA-NAI-SYUBETU     (IDW IDX 1)
                   MOVE    "T"             TO
                                       SPA-GMN-TCHK-VALUE  (IDW IDX)
      *
      *            プリンタ設定
      *************IF      FLG-PRTNM-SET           =   ZERO
                       MOVE    SPA-NAI-PRTNM-G (3)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (4)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
      *************END-IF
                   PERFORM         UNTIL   FLG-RECE07  =   1
                                   OR      RECE07-HKNJANUM
                                                   NOT =
                                           SPA-NAI-THKNJANUM (IDW IDX)
                       PERFORM 31011-SYUKA-PAGE-HENSYU-SEC
                       ADD     RECE07-PAGE-CHOUKI
                                           TO
                                       SPA-NAI-TPAGE-CHOUKI  (IDW IDX)
                       ADD     RECE07-PAGE-ZAITAKU
                                           TO
                                       SPA-NAI-TPAGE-ZAITAKU (IDW IDX)
                       ADD     RECE07-PAGE-KOUGAKU
                                           TO
                                       SPA-NAI-TPAGE-KOUGAKU (IDW IDX)
      *
                       IF    ( RECE07-PAGE-CHOUKI  >   0 )
                       OR    ( RECE07-PAGE-ZAITAKU >   0 )
                       OR    ( RECE07-PAGE-KOUGAKU >   0 )
                         IF    ( SPA-GMN-SYUKA-MAX   >   ZERO )
                         AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                 NOT =   "00" )
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                  UNTIL   IDXX    >   SPA-GMN-SYUKA-MAX
                               IF      RECE07-SRYKA
                                              =   SPA-GMN-SYUKAL (IDXX)
                                   IF      RECE07-PAGE-CHOUKI  >   0
                                       MOVE    1           TO
                                           SPA-NAI-TPAGE-CHOUKI-SYUKA
                                                          (IDW IDX IDXX)
                                   END-IF
                                   IF      RECE07-PAGE-ZAITAKU >   0
                                       MOVE    1           TO
                                         SPA-NAI-TPAGE-ZAITAKU-SYUKA
                                                          (IDW IDX IDXX)
                                   END-IF 
                                   IF      RECE07-PAGE-KOUGAKU >   0
                                       MOVE    1           TO
                                         SPA-NAI-TPAGE-KOUGAKU-SYUKA
                                                          (IDW IDX IDXX)
                                   END-IF 
                               END-IF
                           END-PERFORM
                         END-IF
                       END-IF 
      *
                       MOVE    "tbl_recekanri"     TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-RECE07-NEXT-SEC
                   END-PERFORM
               END-PERFORM
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    全ての保険者が表示できなかったときエラー表示する
           IF    ( FLG-RECE07          =   ZERO              )
           AND   ( IDW                 >=  WRK-CONS-PAGE-MAX )
               MOVE    "0100"              TO  SPA-ERRCD
           END-IF
      *
           IF      IDW                 =   SPA-GMN-PAGE-MAX
               ADD     1                   TO  IDW
               MOVE    "該当なし"          TO  SPA-GMN-TSYURUINAME
                                                        (IDW 1)
               PERFORM VARYING IDX     FROM    2   BY  1
                       UNTIL   IDX     >       WRK-CONS-PAGE-MAX
                   INITIALIZE                  SPA-GMN-TBL-OCC2 (IDX)
               END-PERFORM
               MOVE    IDW                 TO  SPA-GMN-PAGE-MAX
                                               SPA-GMN-PAGE-MAX1
                                               SPA-GMN-PAGE
           ELSE
               MOVE    IDW                 TO  SPA-GMN-PAGE-MAX
                                               SPA-GMN-PAGE-MAX1
           END-IF
           .
       3101-PAGE-HENSYU-03-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（全体）
      *****************************************************************
       3101-PAGE-HENSYU-04-SEC            SECTION.
      *
      *    表示ページ・行の設定
           MOVE    1               TO  IDW
           MOVE    1               TO  IDX 
      *
           MOVE    "01"            TO  SPA-NAI-PRTJYUNI    (IDW IDX)
           MOVE    "1"             TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
           MOVE    "9999"          TO  SPA-NAI-SYUBETU     (IDW IDX 1)
           IF      SPA-R01-RECE07-SRYYM    <   WRK-CONS-SRYYM-200804
               MOVE    "社保・国保"
                                   TO  SPA-GMN-TSYURUINAME (IDW IDX)
           ELSE
               MOVE    "社保・国保・広域"
                                   TO  SPA-GMN-TSYURUINAME (IDW IDX)
           END-IF
           MOVE    "T"             TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
      *
      *    ページ数・件数設定
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "%"                 TO  RECE07-TEISYUTUSAKI
           MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
           EVALUATE    SPA-GMN-SYUKA
               WHEN    SPACE
               WHEN    "00"
                   MOVE    "%%"            TO  RECE07-SRYKA
               WHEN    OTHER
                   MOVE    SPA-GMN-SYUKA   TO  RECE07-SRYKA
           END-EVALUATE
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               IF      RECE07-RECESYUBETU  =   "6666"
                   CONTINUE
               ELSE
                   PERFORM 31011-SYUKA-PAGE-HENSYU-SEC
               END-IF
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    プリンタ設定
           IF      FLG-PRTNM-SET           =   ZERO
               EVALUATE    SPA-NAI-TEISYUTUSAKI  (IDW IDX)
                   WHEN    "1"
                       MOVE    SPA-NAI-PRTNM-G (1)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (2)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                   WHEN    "2"
                       MOVE    SPA-NAI-PRTNM-G (3)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (4)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                   WHEN    "6"
                       MOVE    SPA-NAI-PRTNM-G (5)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (6)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
               END-EVALUATE
           END-IF
           .
       3101-PAGE-HENSYU-04-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（医師別）
      *****************************************************************
       3101-PAGE-HENSYU-05-SEC            SECTION.
      *
           IF      SPA-GMN-SYUKA       =   SPACE   OR  "00"
               MOVE    101                  TO  IDXX
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-SYUKA-MAX
                   IF      SPA-GMN-SYUKA       =   SPA-GMN-SYUKAL (IDX)
                       MOVE    IDX             TO  IDXX
                       MOVE    SPA-GMN-SYUKA-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
      *
           MOVE    ZERO                TO  IDX
                                           IDW
           MOVE    1                   TO  IDXY
           PERFORM UNTIL   IDXY    >   100
                   OR      IDW     >=  WRK-CONS-PAGE-MAX
                   OR      SPA-NAI-DRCD-TDRCD (IDXY)
                                       =   SPACE
               IF      SPA-NAI-DRCD-TPAGE  (IDXY IDXX)
                                       >   ZERO
                   ADD     1                   TO  IDW
                   MOVE    ZERO                TO  IDX
                   PERFORM UNTIL   IDXY    >   100
                           OR      IDX     >=  WRK-CONS-LINE-MAX
                           OR    ( IDW     =   1       AND
                                   IDX     >=  32          ) 
                           OR      SPA-NAI-DRCD-TDRCD (IDXY)
                                           =   SPACE
                       IF      SPA-NAI-DRCD-TPAGE  (IDXY IDXX)
                                               >   ZERO
                           ADD     1           TO  IDX
                           MOVE    SPA-NAI-DRCD-TDRCD (IDXY)
                                               TO  SPA-NAI-TDRCD
                                                           (IDW IDX)
      *
                           IF      SPA-NAI-TDRCD(IDW IDX)
                                               =   "XXXXX"
                               MOVE    "医師設定なし"  TO  
                                               SPA-GMN-TSYURUINAME
                                                           (IDW IDX)
                               MOVE    "1"          TO
                                               SPA-NAI-THKNJANUM-ERR
                                                           (IDW IDX)
                           ELSE
                               MOVE    SPA-NAI-TDRCD (IDW IDX)
                                                   TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
      *
                               INITIALIZE              SYS-1010-REC
                               MOVE    SPA-HOSPNUM TO  SYS-1010-HOSPNUM
                               MOVE    "1010"      TO  SYS-1010-KANRICD
                               MOVE    SPA-NAI-TDRCD (IDW IDX)
                                                   TO  SYS-1010-KBNCD
                               MOVE    SPA-R01-SRYYM
                                                   TO  SYS-1010-STYUKYMD
                                                                   (1:6)
                               MOVE    "01"        TO  SYS-1010-STYUKYMD
                                                                   (7:2)
                               MOVE    SYS-1010-STYUKYMD
                                                   TO  SYS-1010-EDYUKYMD
                               MOVE    SYS-1010-REC
                                                   TO  MCPDATA-REC 
                               MOVE    "tbl_syskanri"
                                                   TO  WRK-MCP-TABLE
                               MOVE    "key10"     TO  WRK-MCP-PATHNAME
                               PERFORM 900-SYSKANRI-INV-SEC
                               IF      FLG-SYSKANRI
                                                   =   ZERO
                                   MOVE  MCPDATA-REC   TO  SYS-1010-REC
                                   MOVE  SYS-1010-NAME (1:34)
                                                       TO
                                           SPA-GMN-TSYURUINAME
                                                       (IDW IDX) (7:34)
                               ELSE
                                   MOVE    "1"         TO
                                           SPA-NAI-THKNJANUM-ERR
                                                               (IDW IDX)
                               END-IF
                           END-IF
      *
                           MOVE    IDX (3:2)       TO
                                           SPA-NAI-PRTJYUNI    (IDW IDX)
                           MOVE    "%"             TO
                                           SPA-NAI-TEISYUTUSAKI(IDW IDX)
                           MOVE    "9999"          TO
                                           SPA-NAI-SYUBETU   (IDW IDX 1)
                           MOVE    "T"             TO
                                           SPA-GMN-TCHK-VALUE  (IDW IDX)
      *
      *                    プリンタ設定
                           MOVE    SPA-NAI-PRTNM-G (1)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                           MOVE    SPA-NAI-PRTNM-G (2)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
      *                    頁編集処理
                           ADD     SPA-NAI-DRCD-TPAGE  (IDXY IDXX)
                                               TO  SPA-GMN-TPAGE
                                                          (IDW IDX)
                                                   SPA-NAI-TPAGE-SYUKA
                                                          (IDW IDX IDXX)
                                                   SPA-GMN-PAGEH
                           ADD     SPA-NAI-DRCD-TKENSU (IDXY IDXX)
                                               TO  SPA-GMN-TKENSU
                                                          (IDW IDX)
                                                   SPA-GMN-KENSUH
                       END-IF    
      *
                       ADD     1           TO  IDXY
                   END-PERFORM
      *
               ELSE
                   ADD     1           TO  IDXY
               END-IF
           END-PERFORM    
      *
      *    全ての医師が表示できなかったときエラー表示する
           IF    ( IDXY                <   100               )
           AND   ( IDW                 >=  WRK-CONS-PAGE-MAX )
               MOVE    "0100"              TO  SPA-ERRCD
           END-IF
      *
           IF      IDW                 =   ZERO
               MOVE    "該当なし"          TO  SPA-GMN-TSYURUINAME
                                                        (1 1)
               INITIALIZE                  SPA-GMN-TBL2
               MOVE    1                   TO  SPA-GMN-PAGE-MAX
                                               SPA-GMN-PAGE-MAX1
                                               SPA-GMN-PAGE
           ELSE
               MOVE    IDW                 TO  SPA-GMN-PAGE-MAX
                                               SPA-GMN-PAGE-MAX1
           END-IF
           .
       3101-PAGE-HENSYU-05-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（病棟別）
      *****************************************************************
       3101-PAGE-HENSYU-06-SEC            SECTION.
      *
           IF      SPA-GMN-SYUKA       =   SPACE   OR  "00"
               MOVE    101                 TO  IDXX
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-SYUKA-MAX
                   IF      SPA-GMN-SYUKA       =   SPA-GMN-SYUKAL (IDX)
                       MOVE    IDX             TO  IDXX
                       MOVE    SPA-GMN-SYUKA-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
           END-IF    
      *
      *
           MOVE    ZERO                TO  IDX
                                           IDW
           MOVE    1                   TO  IDXY
           PERFORM UNTIL   IDXY    >   100
                   OR      IDW     >=  WRK-CONS-PAGE-MAX
                   OR      SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                                       =   SPACE
               IF      SPA-NAI-BTUNUM-TPAGE  (IDXY IDXX)
                                       >   ZERO
                   ADD     1                   TO  IDW
                   MOVE    ZERO                TO  IDX
                   PERFORM UNTIL   IDXY    >   100
                           OR      IDX     >=  WRK-CONS-LINE-MAX
                           OR    ( IDW     =   1       AND
                                   IDX     >=  32          ) 
                           OR      SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                                           =   SPACE
                       IF      SPA-NAI-BTUNUM-TPAGE  (IDXY IDXX)
                                               >   ZERO
                           ADD     1           TO  IDX
                           MOVE    SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                                               TO  SPA-NAI-TBTUNUM
                                                           (IDW IDX)
      *
                           IF      SPA-NAI-TBTUNUM(IDW IDX)
                                               =   "XX"
                               MOVE    "病棟設定なし"  TO  
                                               SPA-GMN-TSYURUINAME
                                                           (IDW IDX)
                               MOVE    "1"          TO
                                               SPA-NAI-THKNJANUM-ERR
                                                           (IDW IDX)
                           ELSE
                               MOVE    SPA-NAI-TBTUNUM (IDW IDX)
                                                   TO
                                           SPA-GMN-TSYURUINAME (IDW IDX)
      *
                               INITIALIZE              SYS-5001-REC
                               MOVE    SPA-HOSPNUM TO  SYS-5001-HOSPNUM
                               MOVE    "5001"      TO  SYS-5001-KANRICD
                               MOVE    SPA-NAI-TBTUNUM (IDW IDX)
                                                   TO  SYS-5001-KBNCD
                               MOVE    SPA-R01-SRYYM
                                                   TO  SYS-5001-STYUKYMD
                                                                   (1:6)
                               MOVE    "01"        TO  SYS-5001-STYUKYMD
                                                                   (7:2)
                               MOVE    SYS-5001-STYUKYMD
                                                   TO  SYS-5001-EDYUKYMD
                               MOVE    SYS-5001-REC
                                                   TO  MCPDATA-REC 
                               MOVE    "tbl_syskanri"
                                                   TO  WRK-MCP-TABLE
                               MOVE    "key10"     TO  WRK-MCP-PATHNAME
                               PERFORM 900-SYSKANRI-INV-SEC
                               IF      FLG-SYSKANRI
                                                   =   ZERO
                                   MOVE  MCPDATA-REC   TO  SYS-5001-REC
                                   MOVE  SYS-5001-BTU-NAME
                                                       TO
                                           SPA-GMN-TSYURUINAME
                                                       (IDW IDX) (4:20)
                               ELSE
                                   MOVE    "1"         TO
                                           SPA-NAI-THKNJANUM-ERR
                                                               (IDW IDX)
                               END-IF
                           END-IF
      *
                           MOVE    IDX (3:2)       TO
                                           SPA-NAI-PRTJYUNI    (IDW IDX)
                           MOVE    "%"             TO
                                           SPA-NAI-TEISYUTUSAKI(IDW IDX)
                           MOVE    "9999"          TO
                                           SPA-NAI-SYUBETU   (IDW IDX 1)
                           MOVE    "T"             TO
                                           SPA-GMN-TCHK-VALUE  (IDW IDX)
      *
      *                    プリンタ設定
                           MOVE    SPA-NAI-PRTNM-G (1)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                           MOVE    SPA-NAI-PRTNM-G (2)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
      *                    頁編集処理
                           ADD     SPA-NAI-BTUNUM-TPAGE  (IDXY IDXX)
                                               TO  SPA-GMN-TPAGE
                                                          (IDW IDX)
                                                   SPA-GMN-PAGEH
                           IF      IDXX        =   101
                               PERFORM VARYING IDZZ    FROM    1   BY  1
                                       UNTIL   IDZZ    >
                                                       SPA-GMN-SYUKA-MAX
                                   ADD     SPA-NAI-BTUNUM-TPAGE
                                                           (IDXY IDZZ)
                                               TO  SPA-NAI-TPAGE-SYUKA
                                                          (IDW IDX IDZZ)
                               END-PERFORM
                           ELSE
                               ADD     SPA-NAI-BTUNUM-TPAGE (IDXY IDXX)
                                               TO  SPA-NAI-TPAGE-SYUKA
                                                          (IDW IDX IDXX)
                           END-IF
                           ADD     SPA-NAI-BTUNUM-TKENSU (IDXY IDXX)
                                               TO  SPA-GMN-TKENSU
                                                          (IDW IDX)
                                                   SPA-GMN-KENSUH
                       END-IF    
      *
                       ADD     1           TO  IDXY
                   END-PERFORM
               ELSE
                   ADD     1           TO  IDXY
               END-IF
           END-PERFORM    
      *
      *    全ての病棟が表示できなかったときエラー表示する
           IF    ( IDXY                <   100               )
           AND   ( IDW                 >=  WRK-CONS-PAGE-MAX )
               MOVE    "0100"              TO  SPA-ERRCD
           END-IF
      *
           IF      IDW                 =   ZERO
               MOVE    "該当なし"          TO  SPA-GMN-TSYURUINAME
                                                        (1 1)
               INITIALIZE                  SPA-GMN-TBL2
               MOVE    1                   TO  SPA-GMN-PAGE-MAX
                                               SPA-GMN-PAGE-MAX1
                                               SPA-GMN-PAGE
           ELSE
               MOVE    IDW                 TO  SPA-GMN-PAGE-MAX
                                               SPA-GMN-PAGE-MAX1
           END-IF
           .
       3101-PAGE-HENSYU-06-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（社保・編綴順）
      *****************************************************************
       3101-PAGE-HENSYU-07-SEC             SECTION.
      *
      *    表示ページの設定
           MOVE    1               TO  IDW
           MOVE    1               TO  IDX 
      *
           MOVE    "01"            TO  SPA-NAI-PRTJYUNI    (IDW IDX)
           MOVE    "1"             TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
           MOVE    "9999"          TO  SPA-NAI-SYUBETU     (IDW IDX 1)
           MOVE    "編綴順"        TO  SPA-GMN-TSYURUINAME (IDW IDX)
           MOVE    "T"             TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
      *
           IF    ( SPA-R01-NYUGAIKBN   =   "2"                   )
           AND   ( SPA-R01-SRYYM       <   WRK-CONS-SRYYM-201204 )
               ADD     1               TO  IDX 
               MOVE    "02"            TO  SPA-NAI-PRTJYUNI    (IDW IDX)
               MOVE    "1"             TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
               MOVE    "9999"          TO  SPA-NAI-SYUBETU   (IDW IDX 1)
               MOVE    "編綴順（在医総管または在医総）"
                                       TO  SPA-GMN-TSYURUINAME (IDW IDX)
               MOVE    "T"             TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
           END-IF
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY    >   SPA-R01-RECE07-HKNJANUM-MAX
               IF      IDW             =   1
                   MOVE    SPA-R01-RECE07-HKNJANUM-SYAHO (IDYY)
                                               TO
                                           SPA-NAI-THKNJANUM-SYAHO (IDW)
                   IF      SPA-R01-RECE07-HKNJANUM-SRT
                                               =   "8"
                       MOVE    "直接請求する保険者以外"
                                                   TO
                                           SPA-GMN-THKNJANAME-SYAHO(IDW)
                   END-IF
               ELSE
                   MOVE    SPA-NAI-TBL-OCC (1) TO  SPA-NAI-TBL-OCC (IDW)
                   MOVE    SPA-GMN-TBL-OCC (1) TO  SPA-GMN-TBL-OCC (IDW)
                   MOVE    SPA-GMN-TBL-OCC1(1) TO  SPA-GMN-TBL-OCC1(IDW)
                   MOVE    SPA-R01-RECE07-HKNJANUM-SYAHO (IDYY)
                                               TO
                                           SPA-NAI-THKNJANUM-SYAHO (IDW)
                   INITIALIZE                      HKNJAINF-REC
                   MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
                   MOVE    SPA-NAI-THKNJANUM-SYAHO (IDW)
                                               TO  HKNJA-HKNJANUM
                   MOVE    HKNJAINF-REC        TO  MCPDATA-REC
                   PERFORM 900-HKNJAINF-READ-SEC
                   MOVE    HKNJA-HKNJANAME     TO
                                           SPA-GMN-THKNJANAME-SYAHO(IDW)
               END-IF
      *
               MOVE    1               TO  IDX 
               PERFORM 31011-PAGE-HENSYU-07-KENSU-SEC
               IF    ( SPA-R01-NYUGAIKBN   =   "2"                   )
               AND   ( SPA-R01-SRYYM       <   WRK-CONS-SRYYM-201204 )
                   ADD     1               TO  IDX 
                   PERFORM 31011-PAGE-HENSYU-07-KENSU-SEC
               END-IF
      *
               ADD     1               TO  IDW
           END-PERFORM
           .
       3101-PAGE-HENSYU-07-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保遍綴順頁編集処理
      *****************************************************************
       31011-PAGE-HENSYU-07-KENSU-SEC             SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-TPAGE  (IDW IDX)
                                       SPA-GMN-TKENSU (IDW IDX)
           INITIALIZE                  SPA-NAI-TPAGE-G(IDW IDX)
      *
      *    ページ数設定
           INITIALIZE                  QUERY-RECE08-REC
           MOVE    SPA-HOSPNUM     TO  QUERY-RECE08-HOSPNUM
           MOVE    SPA-R01-RECE07-PRTID
                                   TO  QUERY-RECE08-PRTID
           MOVE    SPA-R01-SRYYM   TO  QUERY-RECE08-RECEYM
           MOVE    SPA-R01-RECE07-CREYMD
                                   TO  QUERY-RECE08-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS
                                   TO  QUERY-RECE08-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                   TO  QUERY-RECE08-NYUGAIKBN
           MOVE    "1"             TO  QUERY-RECE08-TEISYUTUSAKI
           IF      IDX             =   1
               MOVE    "9"             TO  QUERY-RECE08-ZAITAKU
           ELSE
               MOVE    "1"             TO  QUERY-RECE08-ZAITAKU
           END-IF
           EVALUATE    SPA-GMN-SYUKA
               WHEN    SPACE
               WHEN    "00"
                   MOVE    "%%"        TO  QUERY-RECE08-SRYKA
               WHEN    OTHER
                   IF      SPA-R01-RECE07-SRYKA
                                   NOT =   "00"
                       MOVE    SPA-GMN-SYUKA   TO  QUERY-RECE08-SRYKA
                   ELSE
                      MOVE    "%%"             TO  QUERY-RECE08-SRYKA
                   END-IF 
           END-EVALUATE
           MOVE    SPA-R01-RECE07-HKNJANUM-SYAHO (IDYY)
                                       TO  QUERY-RECE08-HKNJANUM-S
           MOVE    QUERY-RECE08-REC    TO  MCPDATA-REC
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-QRECEPRT
           END-IF
      *
           PERFORM                     UNTIL   FLG-QRECEPRT  =   1
               ADD     QUERY-RECE08-PAGE
                                       TO  SPA-GMN-TPAGE (IDW IDX)
                                           SPA-GMN-PAGEH
      *
      *        主科別の頁編集処理
               IF      SPA-GMN-SYUKA-MAX   >       ZERO
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                       IF      QUERY-RECE08-SRYKA
                                           =   SPA-GMN-SYUKAL (IDXX)
                           ADD     QUERY-RECE08-PAGE     TO
                                   SPA-NAI-TPAGE-SYUKA (IDW IDX IDXX)
                           MOVE    SPA-GMN-SYUKA-MAX     TO  IDXX
                       END-IF
                   END-PERFORM
               END-IF
      *
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           END-PERFORM    
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    件数設定
           INITIALIZE                      QUERY-RECE08-REC
           MOVE    SPA-HOSPNUM         TO  QUERY-RECE08-HOSPNUM
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  QUERY-RECE08-PRTID
           MOVE    SPA-R01-SRYYM       TO  QUERY-RECE08-RECEYM
           MOVE    SPA-R01-RECE07-CREYMD
                                       TO  QUERY-RECE08-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS
                                       TO  QUERY-RECE08-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  QUERY-RECE08-NYUGAIKBN
           MOVE    "1"                 TO  QUERY-RECE08-TEISYUTUSAKI
           IF      IDX             =   1
               MOVE    "9"             TO  QUERY-RECE08-ZAITAKU
           ELSE
               MOVE    "1"             TO  QUERY-RECE08-ZAITAKU
           END-IF
           EVALUATE    SPA-GMN-SYUKA
               WHEN    SPACE
               WHEN    "00"
                   MOVE    "%%"        TO  QUERY-RECE08-SRYKA
               WHEN    OTHER
                   IF      SPA-R01-RECE07-SRYKA
                                       NOT =   "00"
                       MOVE    SPA-GMN-SYUKA   TO  QUERY-RECE08-SRYKA
                   ELSE
                       MOVE    "%%"            TO  QUERY-RECE08-SRYKA
                   END-IF 
           END-EVALUATE
           MOVE    SPA-R01-RECE07-HKNJANUM-SYAHO (IDYY)
                                       TO  QUERY-RECE08-HKNJANUM-S
           MOVE    QUERY-RECE08-REC    TO  MCPDATA-REC
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-QRECEPRT
           END-IF
      *
           PERFORM                     UNTIL   FLG-QRECEPRT  =   1
               ADD     QUERY-RECE08-KENSU
                                       TO  SPA-GMN-TKENSU(IDW IDX)
                                           SPA-GMN-KENSUH
      *
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           END-PERFORM    
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    プリンタ設定
           IF      FLG-PRTNM-SET       =   ZERO
               MOVE    SPA-NAI-PRTNM-G (1)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
               MOVE    SPA-NAI-PRTNM-G (2)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
           END-IF
      *
           MOVE    IDW                 TO  SPA-GMN-PAGE-MAX
           .
       31011-PAGE-HENSYU-07-KENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁・件数の編集処理（ターミナルデジット）
      *****************************************************************
       3101-PAGE-HENSYU-08-SEC            SECTION.
      *
      *    表示ページ・行の設定
           MOVE    1               TO  IDW
           MOVE    1               TO  IDX 
      *
           MOVE    "01"            TO  SPA-NAI-PRTJYUNI    (IDW IDX)
           MOVE    "1"             TO  SPA-NAI-TEISYUTUSAKI(IDW IDX)
           MOVE    "9999"          TO  SPA-NAI-SYUBETU     (IDW IDX 1)
           IF      SPA-R01-RECE07-SRYYM    <   WRK-CONS-SRYYM-200804
               MOVE    "社保・国保"
                                   TO  SPA-GMN-TSYURUINAME (IDW IDX)
           ELSE
               MOVE    "社保・国保・広域"
                                   TO  SPA-GMN-TSYURUINAME (IDW IDX)
           END-IF
           MOVE    "T"             TO  SPA-GMN-TCHK-VALUE  (IDW IDX)
      *
      *    ページ数・件数設定
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "%"                 TO  RECE07-TEISYUTUSAKI
           MOVE    SPA-R01-NYUGAIKBN   TO  RECE07-NYUGAIKBN
           EVALUATE    SPA-GMN-SYUKA
               WHEN    SPACE
               WHEN    "00"
                   MOVE    "%%"            TO  RECE07-SRYKA
               WHEN    OTHER
                   MOVE    SPA-GMN-SYUKA   TO  RECE07-SRYKA
           END-EVALUATE
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               IF      RECE07-RECESYUBETU  =   "6666"
                   CONTINUE
               ELSE
                   PERFORM 31011-SYUKA-PAGE-HENSYU-SEC
               END-IF
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    プリンタ設定
           IF      FLG-PRTNM-SET           =   ZERO
               EVALUATE    SPA-NAI-TEISYUTUSAKI  (IDW IDX)
                   WHEN    "1"
                       MOVE    SPA-NAI-PRTNM-G (1)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (2)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                   WHEN    "2"
                       MOVE    SPA-NAI-PRTNM-G (3)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (4)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
                   WHEN    "6"
                       MOVE    SPA-NAI-PRTNM-G (5)
                                       TO  SPA-GMN-TPRTNM-G   (IDW IDX)
                       MOVE    SPA-NAI-PRTNM-G (6)
                                       TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
               END-EVALUATE
           END-IF
           .
       3101-PAGE-HENSYU-08-EXT.
           EXIT.
      *
      *****************************************************************
      *    頁編集処理
      *****************************************************************
       31011-SYUKA-PAGE-HENSYU-SEC             SECTION.
      *
           ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (IDW IDX)
                                       SPA-GMN-PAGEH
           ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(IDW IDX)
                                       SPA-GMN-KENSUH
      *
      *    主科別の頁編集処理
           IF      SPA-GMN-SYUKA-MAX   >       ZERO
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                   IF      RECE07-SRYKA    =   SPA-GMN-SYUKAL (IDXX)
                       ADD     RECE07-PAGE     TO
                                   SPA-NAI-TPAGE-SYUKA (IDW IDX IDXX)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       31011-SYUKA-PAGE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    特別療養費・自費・不明分の編集処理
      *****************************************************************
       31011-PAGE-HENSYU-SEC            SECTION.
      *
      *    特別療養費（広域連合）の編集     
           IF      SPA-R01-RECE07-SRYYM    >=  WRK-CONS-SRYYM-200804
               MOVE    "33"              TO  SPA-NAI-PRTJYUNI    (1 33)
               MOVE    "6"               TO  SPA-NAI-TEISYUTUSAKI(1 33)
               MOVE    "6666"            TO  SPA-NAI-SYUBETU    (1 33 1)
               MOVE    "特別療養費（広域）"
                                         TO  SPA-GMN-TSYURUINAME (1 33)
               MOVE    "T"               TO  SPA-GMN-TCHK-VALUE  (1 33)
               IF      FLG-PRTNM-SET           =   ZERO
                   MOVE    SPA-NAI-PRTNM-G (1)
                                         TO  SPA-GMN-TPRTNM-G    (1 33)
                   MOVE    SPA-NAI-PRTNM-G (2)
                                         TO  SPA-GMN-TPRTNM-N-G  (1 33)
               END-IF
           END-IF
      *
      *    特別療養費（国保）の編集     
           MOVE    "34"              TO  SPA-NAI-PRTJYUNI    (1 34)
           MOVE    "2"               TO  SPA-NAI-TEISYUTUSAKI(1 34)
           MOVE    "6666"            TO  SPA-NAI-SYUBETU     (1 34 1)
           MOVE    "特別療養費（国保）"
                                     TO  SPA-GMN-TSYURUINAME (1 34)
           MOVE    "T"               TO  SPA-GMN-TCHK-VALUE  (1 34)
           IF      FLG-PRTNM-SET           =   ZERO
               MOVE    SPA-NAI-PRTNM-G (1)
                                     TO  SPA-GMN-TPRTNM-G    (1 34)
               MOVE    SPA-NAI-PRTNM-G (2)
                                     TO  SPA-GMN-TPRTNM-N-G  (1 34)
           END-IF
      *
      *    自費の編集     
           MOVE    "35"              TO  SPA-NAI-PRTJYUNI    (1 35)
           MOVE    "1"               TO  SPA-NAI-TEISYUTUSAKI(1 35)
           MOVE    "8888"            TO  SPA-NAI-SYUBETU     (1 35 1)
           MOVE    "自費"            TO  SPA-GMN-TSYURUINAME (1 35)
           IF    ( SPA-GMN-SYOKBN    =   "T" )
           AND   ( SPA-NAI-2005-JIHI-PRTKBN
                                     =   "0" )
               MOVE    "T"               TO  SPA-GMN-TCHK-VALUE  (1 35)
           ELSE
               MOVE    "F"               TO  SPA-GMN-TCHK-VALUE  (1 35)
           END-IF
           IF      FLG-PRTNM-SET           =   ZERO
               MOVE    SPA-NAI-PRTNM-G (1)
                                     TO  SPA-GMN-TPRTNM-G    (1 35)
               MOVE    SPA-NAI-PRTNM-G (2)
                                     TO  SPA-GMN-TPRTNM-N-G  (1 35)
           END-IF
      *
      *    レセプト種別不明の編集     
           MOVE    "36"              TO  SPA-NAI-PRTJYUNI    (1 36)
           MOVE    "1"               TO  SPA-NAI-TEISYUTUSAKI(1 36)
           MOVE    "0000"            TO  SPA-NAI-SYUBETU     (1 36 1)
           MOVE    "レセプト種別不明"
                                     TO  SPA-GMN-TSYURUINAME (1 36)
           MOVE    "T"               TO  SPA-GMN-TCHK-VALUE  (1 36)
           IF      FLG-PRTNM-SET           =   ZERO
               MOVE    SPA-NAI-PRTNM-G (1)
                                         TO  SPA-GMN-TPRTNM-G    (1 36)
               MOVE    SPA-NAI-PRTNM-G (2)
                                         TO  SPA-GMN-TPRTNM-N-G  (1 36)
           END-IF 
      *                                     
      *    特別療養費（広域連合）レセプトのページ数     
           IF      SPA-R01-RECE07-SRYYM    >=  WRK-CONS-SRYYM-200804
               INITIALIZE                  RECE07-REC
               MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
               MOVE    "6"                 TO  RECE07-TEISYUTUSAKI 
               MOVE    "6666"              TO  RECE07-RECESYUBETU
               MOVE    "%%"                TO  RECE07-SRYKA 
               MOVE    RECE07-REC          TO  MCPDATA-REC                             
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_recekanri"     TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 900-RECE07-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
      *
               PERFORM                     UNTIL   FLG-RECE07  =   1
                   ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 33)
                                               SPA-GMN-PAGEH
                   ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 33)
                                               SPA-GMN-KENSUH
      *
                   MOVE    "tbl_recekanri"     TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 900-RECE07-NEXT-SEC
               END-PERFORM    
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *                                     
      *    特別療養費（国保）レセプトのページ数     
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "2"                 TO  RECE07-TEISYUTUSAKI 
           MOVE    "6666"              TO  RECE07-RECESYUBETU
           MOVE    "%%"                TO  RECE07-SRYKA 
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 34)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 34)
                                           SPA-GMN-KENSUH
      *
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *                                     
      *    自費レセプトのページ数     
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
           MOVE    "8888"              TO  RECE07-RECESYUBETU
           MOVE    "%%"                TO  RECE07-SRYKA 
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 35)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 35)
                                           SPA-GMN-KENSUH
      *
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "2"                 TO  RECE07-TEISYUTUSAKI 
           MOVE    "8888"              TO  RECE07-RECESYUBETU
           MOVE    "%%"                TO  RECE07-SRYKA 
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 35)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 35)
                                           SPA-GMN-KENSUH
      *
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *                                     
      *    レセプト種別不明のレセプトのページ数     
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "1"                 TO  RECE07-TEISYUTUSAKI
           MOVE    "0000"              TO  RECE07-RECESYUBETU
           MOVE    "%%"                TO  RECE07-SRYKA 
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 36)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 36)
                                           SPA-GMN-KENSUH
      *
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "2"                 TO  RECE07-TEISYUTUSAKI 
           MOVE    "0000"              TO  RECE07-RECESYUBETU
           MOVE    "%%"                TO  RECE07-SRYKA 
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 36)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 36)
                                           SPA-GMN-KENSUH
      *
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           INITIALIZE                  RECE07-REC
           MOVE    SPA-R01-RECE07-KEY  TO  RECE07-KEY
           MOVE    "6"                 TO  RECE07-TEISYUTUSAKI 
           MOVE    "0000"              TO  RECE07-RECESYUBETU
           MOVE    "%%"                TO  RECE07-SRYKA 
           MOVE    RECE07-REC          TO  MCPDATA-REC                             
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           PERFORM                     UNTIL   FLG-RECE07  =   1
               ADD     RECE07-PAGE     TO  SPA-GMN-TPAGE (1 36)
                                           SPA-GMN-PAGEH
               ADD     RECE07-KENSU    TO  SPA-GMN-TKENSU(1 36)
                                           SPA-GMN-KENSUH
      *
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           END-PERFORM    
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       31011-PAGE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師別編集処理
      *****************************************************************
       3102-DRCD-HENSYU-SEC            SECTION.
      *
      *    医師別頁                                     
           INITIALIZE                      QUERY-RECE08-REC
           MOVE    SPA-HOSPNUM         TO  QUERY-RECE08-HOSPNUM
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  QUERY-RECE08-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  QUERY-RECE08-RECEYM
           ELSE
               MOVE    ZERO                TO  QUERY-RECE08-RECEYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD
                                       TO  QUERY-RECE08-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS
                                       TO  QUERY-RECE08-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  QUERY-RECE08-NYUGAIKBN
           MOVE    QUERY-RECE08-REC    TO  MCPDATA-REC
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-QRECEPRT
           END-IF
      *
           PERFORM                     UNTIL   FLG-QRECEPRT  =   1
               PERFORM VARYING IDXY    FROM    1   BY  1
                       UNTIL   IDXY    >       100
                   IF      QUERY-RECE08-DRCD   =   SPACE
                       MOVE    "XXXXX"         TO  QUERY-RECE08-DRCD
                   END-IF
                   IF      SPA-NAI-DRCD-TDRCD (IDXY)
                                               =   SPACE
                       MOVE    QUERY-RECE08-DRCD   TO
                                               SPA-NAI-DRCD-TDRCD (IDXY)
                   END-IF    
                   IF      QUERY-RECE08-DRCD   =   SPA-NAI-DRCD-TDRCD
                                                                  (IDXY)
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                           IF      QUERY-RECE08-DRCD-SRYKA
                                           =   SPA-GMN-SYUKAL (IDXX)
                               ADD     QUERY-RECE08-PAGE
                                                       TO
                                           SPA-NAI-DRCD-TPAGE(IDXY IDXX)
                                           SPA-NAI-DRCD-TPAGE(IDXY 101)
                               MOVE    100             TO  IDXY
                               MOVE    SPA-GMN-SYUKA-MAX
                                                       TO  IDXX
                           END-IF    
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           END-PERFORM    
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    医師別件数                                     
           INITIALIZE                      QUERY-RECE08-REC
grpsys     MOVE    SPA-HOSPNUM         TO  QUERY-RECE08-HOSPNUM
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  QUERY-RECE08-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  QUERY-RECE08-RECEYM
           ELSE
               MOVE    ZERO                TO  QUERY-RECE08-RECEYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD
                                       TO  QUERY-RECE08-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS
                                       TO  QUERY-RECE08-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  QUERY-RECE08-NYUGAIKBN
           MOVE    QUERY-RECE08-REC          TO  MCPDATA-REC
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-QRECEPRT
           END-IF
      *
           PERFORM                     UNTIL   FLG-QRECEPRT  =   1
               PERFORM VARYING IDXY    FROM    1   BY  1
                       UNTIL   IDXY    >       100
                   IF      QUERY-RECE08-DRCD   =   SPACE
                       MOVE    "XXXXX"         TO  QUERY-RECE08-DRCD
                   END-IF
      *
                   IF      SPA-NAI-DRCD-TDRCD (IDXY)
                                               =   SPACE
                       MOVE    QUERY-RECE08-DRCD   TO
                                               SPA-NAI-DRCD-TDRCD (IDXY)
                   END-IF    
                   IF      QUERY-RECE08-DRCD   =   SPA-NAI-DRCD-TDRCD
                                                                  (IDXY)
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                           IF      QUERY-RECE08-DRCD-SRYKA
                                           =   SPA-GMN-SYUKAL (IDXX)
                               ADD     QUERY-RECE08-KENSU
                                                       TO
                                       SPA-NAI-DRCD-TKENSU(IDXY IDXX)
                                       SPA-NAI-DRCD-TKENSU(IDXY 101)
                               MOVE    100             TO  IDXY
                               MOVE    SPA-GMN-SYUKA-MAX
                                                       TO  IDXX
                           END-IF    
                       END-PERFORM
                   END-IF
               END-PERFORM
      *
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           END-PERFORM    
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       3102-DRCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟別編集処理
      *****************************************************************
       3104-BTUNUM-HENSYU-SEC            SECTION.
      *
      *    医師別頁                                     
           INITIALIZE                      QUERY-RECE08-REC
           MOVE    SPA-HOSPNUM         TO  QUERY-RECE08-HOSPNUM
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  QUERY-RECE08-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  QUERY-RECE08-RECEYM
           ELSE
               MOVE    ZERO                TO  QUERY-RECE08-RECEYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD
                                       TO  QUERY-RECE08-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS
                                       TO  QUERY-RECE08-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  QUERY-RECE08-NYUGAIKBN
           MOVE    QUERY-RECE08-REC    TO  MCPDATA-REC
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-QRECEPRT
           END-IF
      *
           PERFORM                     UNTIL   FLG-QRECEPRT  =   1
               PERFORM VARYING IDXY    FROM    1   BY  1
                       UNTIL   IDXY    >       100
                   IF      QUERY-RECE08-SORT-1 =   SPACE
                       MOVE    "XXXXXXXXXX"    TO  QUERY-RECE08-SORT-1
                   END-IF
                   IF      SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                                               =   SPACE
                       MOVE    QUERY-RECE08-SORT-1 (1:2)   TO
                                           SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                   END-IF    
                   IF      QUERY-RECE08-SORT-1 (1:2)   =
                                           SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                       ADD     QUERY-RECE08-PAGE   TO
                                         SPA-NAI-BTUNUM-TPAGE(IDXY 101)
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                           IF      QUERY-RECE08-SRYKA
                                           =   SPA-GMN-SYUKAL (IDXX)
                               ADD     QUERY-RECE08-PAGE
                                                       TO
                                         SPA-NAI-BTUNUM-TPAGE(IDXY IDXX)
                               MOVE    SPA-GMN-SYUKA-MAX
                                                       TO  IDXX
                           END-IF    
                       END-PERFORM
                       MOVE    100             TO  IDXY
                   END-IF
               END-PERFORM
      *
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key2"               TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           END-PERFORM    
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    病棟別件数                                     
           INITIALIZE                      QUERY-RECE08-REC
grpsys     MOVE    SPA-HOSPNUM         TO  QUERY-RECE08-HOSPNUM
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  QUERY-RECE08-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  QUERY-RECE08-RECEYM
           ELSE
               MOVE    ZERO                TO  QUERY-RECE08-RECEYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD
                                       TO  QUERY-RECE08-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS
                                       TO  QUERY-RECE08-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  QUERY-RECE08-NYUGAIKBN
           MOVE    QUERY-RECE08-REC          TO  MCPDATA-REC
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-QRECEPRT
           END-IF
      *
           PERFORM                     UNTIL   FLG-QRECEPRT  =   1
               PERFORM VARYING IDXY    FROM    1   BY  1
                       UNTIL   IDXY    >       100
                   IF      QUERY-RECE08-SORT-1 =   SPACE
                       MOVE    "XXXXXXXXXX"    TO  QUERY-RECE08-SORT-1
                   END-IF
      *
                   IF      SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                                               =   SPACE
                       MOVE    QUERY-RECE08-SORT-1 (1:2)   TO
                                           SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                   END-IF    
                   IF      QUERY-RECE08-SORT-1 (1:2)   =
                                           SPA-NAI-BTUNUM-TBTUNUM (IDXY)
                       ADD     QUERY-RECE08-KENSU      TO
                                       SPA-NAI-BTUNUM-TKENSU(IDXY 101)
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                           IF      QUERY-RECE08-SRYKA
                                           =   SPA-GMN-SYUKAL (IDXX)
                               ADD     QUERY-RECE08-KENSU
                                                       TO
                                       SPA-NAI-BTUNUM-TKENSU(IDXY IDXX)
                               MOVE    SPA-GMN-SYUKA-MAX
                                                       TO  IDXX
                           END-IF    
                       END-PERFORM
                       MOVE    100             TO  IDXY
                   END-IF
               END-PERFORM
      *
               MOVE    "query_receprt"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-QUERY-RECE08-NEXT-SEC
           END-PERFORM    
           MOVE    "query_receprt"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       3104-BTUNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-RID1-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    =   "OK"
               EVALUATE    SPA-RIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"           TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1003"    
                       PERFORM 440-PRINT-KENSU-SEC
                   WHEN    "8888"
                       CONTINUE    
               END-EVALUATE        
           END-IF
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
           MOVE    SPACE           TO  SPA-RIDCD      
           .
       330-RID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＲＩＤ３）ＯＫ処理
      *****************************************************************
       330-RID3-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    NOT =   SPACE
               EVALUATE    SPA-RIDCD3
                   WHEN    "1001"
                   WHEN    "1002"    
                       PERFORM 450-PRINT-SEC
               END-EVALUATE
           ELSE        
               MOVE    SPACE           TO  SPA-RID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-RIDCD3      
           .
       330-RID3-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-R99-SET-SEC            SECTION.
      *
           IF      SPA-R99-FLG     =   "OK"
               PERFORM 4501-NEXT-GMN-SEC
           END-IF    
      *
      *    印刷済フラグの設定、印刷チェックの解除
           IF      SPA-R99-CANCEL  =   SPACE
               IF    ( SPA-GMN-SYOKBN  =   "F"         )
               AND   ( SPA-GMN-PRTKBN  =   "1"         )
               AND   ( SPA-GMN-PREFKBN =   "1" OR  "4" )
               AND   ( SPA-RID1-FLG    =   "OK"        )
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       SPA-GMN-PAGE-MAX
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       WRK-CONS-LINE-MAX
                           IF      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                               CONTINUE
                           ELSE
                               IF    ( SPA-GMN-TCHK-VALUE  (IDW IDX)
                                                           =   "T"   )
                               AND   ( SPA-GMN-TSTDATA (IDW IDX)
                                                           =   SPACE )
                               AND   ( SPA-GMN-TEDDATA (IDW IDX)
                                                           =   SPACE )
                                   MOVE    "1"     TO
                                               SPA-GMN-TPRTFLG (IDW IDX)
                                   IF      SPA-NAI-2005-PRT-CHECKKBN
                                                               =   "1"
                                       MOVE    "F"             TO
                                           SPA-GMN-TCHK-VALUE (IDW IDX)
                                   END-IF
                               END-IF
                           END-IF 
                       END-PERFORM
                   END-PERFORM
               END-IF
           END-IF     
      *
           MOVE    SPACE           TO  SPA-R99-FLG
           MOVE    SPACE           TO  SPA-RIDCD
           MOVE    SPACE           TO  SPA-RIDCD2      
           MOVE    SPACE           TO  SPA-RIDCD3
           .
       340-R99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ設定画面戻り処理
      *****************************************************************
       340-R06-SET-SEC             SECTION.
      *
           EVALUATE    SPA-RID1-FLG
               WHEN    SPACE
                   CONTINUE
               WHEN    "AL" 
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       SPA-GMN-PAGE-MAX
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       WRK-CONS-LINE-MAX
                           IF      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                               CONTINUE
                           ELSE
                               MOVE    SPA-R01-PRTNM-G     TO
                                           SPA-GMN-TPRTNM-G   (IDW IDX)
                               MOVE    SPA-R01-PRTNM-N-G   TO
                                           SPA-GMN-TPRTNM-N-G (IDW IDX)
                           END-IF 
                       END-PERFORM
                   END-PERFORM
               WHEN    "PG" 
                   MOVE    SPA-GMN-PAGE    TO  IDW
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-CONS-LINE-MAX
                       IF      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                           CONTINUE
                       ELSE
                           MOVE    SPA-R01-PRTNM-G     TO
                                           SPA-GMN-TPRTNM-G   (IDW IDX)
                           MOVE    SPA-R01-PRTNM-N-G   TO
                                           SPA-GMN-TPRTNM-N-G (IDW IDX)
                       END-IF 
                   END-PERFORM
           END-EVALUATE     
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
           MOVE    SPACE           TO  SPA-R01-PRTNM-G
                                       SPA-R01-PRTNM-N-G
           MOVE    SPACE           TO  SPA-R01-MOTOPG
           .
       340-R06-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.       
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R02"               TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    SPA-LNK-XC01-KEY    TO  LNK-XC01-KEY
           MOVE    1                   TO  LNK-XC01-SHORI-RENNUM
           MOVE    1                   TO  LNK-XC01-ST-PAGE
           MOVE    99999               TO  LNK-XC01-ED-PAGE             
           MOVE    "1"                 TO  LNK-XC01-MODE            
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       4501-NEXT-GMN-EXT.
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
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    件数印刷
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-PRINT-KENSU-GIDMSG-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *     処理区分
               WHEN    "CLICKED"       ALSO    "B08"
               WHEN    "CLICKED"       ALSO    "SYOKBN"
                   PERFORM 410-SYOKBN-CHK-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *    印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-PRINT-GIDMSG-SEC
      *    編綴情報表示
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-HENTETU-SEC
      *    プリンタ一括設定
               WHEN    "CLICKED"       ALSO    "B14"
                   PERFORM 490-PRINTER-SETTEI-SEC
      *    処理確認
               WHEN    "CLICKED"       ALSO    "B24"
                   PERFORM 490-KAKUNIN-SEC
      *     個別出力
               WHEN    "CLICKED"       ALSO    "CHK01"
               WHEN    "CLICKED"       ALSO    "CHK02"
               WHEN    "CLICKED"       ALSO    "CHK03"
               WHEN    "CLICKED"       ALSO    "CHK04"
               WHEN    "CLICKED"       ALSO    "CHK05"
               WHEN    "CLICKED"       ALSO    "CHK06"
               WHEN    "CLICKED"       ALSO    "CHK07"
               WHEN    "CLICKED"       ALSO    "CHK08"
               WHEN    "CLICKED"       ALSO    "CHK09"
               WHEN    "CLICKED"       ALSO    "CHK10"
               WHEN    "CLICKED"       ALSO    "CHK11"
               WHEN    "CLICKED"       ALSO    "CHK12"
               WHEN    "CLICKED"       ALSO    "CHK13"
               WHEN    "CLICKED"       ALSO    "CHK14"
               WHEN    "CLICKED"       ALSO    "CHK15"
               WHEN    "CLICKED"       ALSO    "CHK16"
               WHEN    "CLICKED"       ALSO    "CHK17"
               WHEN    "CLICKED"       ALSO    "CHK18"
               WHEN    "CLICKED"       ALSO    "CHK19"
               WHEN    "CLICKED"       ALSO    "CHK20"
               WHEN    "CLICKED"       ALSO    "CHK21"
               WHEN    "CLICKED"       ALSO    "CHK22"
               WHEN    "CLICKED"       ALSO    "CHK23"
               WHEN    "CLICKED"       ALSO    "CHK24"
               WHEN    "CLICKED"       ALSO    "CHK25"
               WHEN    "CLICKED"       ALSO    "CHK26"
               WHEN    "CLICKED"       ALSO    "CHK27"
               WHEN    "CLICKED"       ALSO    "CHK28"
               WHEN    "CLICKED"       ALSO    "CHK29"
               WHEN    "CLICKED"       ALSO    "CHK30"
               WHEN    "CLICKED"       ALSO    "CHK31"
               WHEN    "CLICKED"       ALSO    "CHK32"
               WHEN    "CLICKED"       ALSO    "CHK33"
               WHEN    "CLICKED"       ALSO    "CHK34"
               WHEN    "CLICKED"       ALSO    "CHK35"
               WHEN    "CLICKED"       ALSO    "CHK36"
                   PERFORM 420-CHK-CHK-SEC
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
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "PRTJYUN"
                   PERFORM 4105-PRTJYUN-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTJYUN1"
                   PERFORM 4108-PRTJYUN1-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PRTJYUN2"
                   PERFORM 4108-PRTJYUN2-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SYUKA"
                   PERFORM 4109-SYUKA-CHK-SEC
               WHEN    OTHER
                   PERFORM 410-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入力処理
      *****************************************************************
       410-ENTRY-SYORI-SEC       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       410-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *   入力チェック
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET (1:6)
               WHEN    "ACTIVATE"      ALSO    "STDATA"
               WHEN    "ACTIVATE"      ALSO    "EDDATA"
      *            全件印刷のとき
                   IF      SPA-GMN-SYOKBN  =   "T"
                       MOVE    "0006"          TO  SPA-ERRCD
                   END-IF
      *            入力順印刷指定のとき
                   IF      SPA-GMN-PRTJYUN =   "05"    OR  "13"
                       MOVE    "0011"          TO  SPA-ERRCD
                   END-IF
      *            指定印刷のとき
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET3         TO  IDX 
                   IF    ( SPA-GMN-SYOKBN  =   "F" )
                   AND   ( SPA-GMN-TCHK-VALUE (SPA-GMN-PAGE IDX)
                                           =   "F" )   
                       MOVE    "0014"          TO  SPA-ERRCD
                   END-IF
               WHEN    "ACTIVATE"      ALSO    "PRTKBN"
                   IF      SPA-GMN-PRTJYUN =   "05"    OR  "07"
                                                       OR  "08"
                                                       OR  "11"
                                                       OR  "12"
                                                       OR  "13"
                       MOVE    "0013"          TO  SPA-ERRCD
                   END-IF    
           END-EVALUATE
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    199             TO  SPA-GMN-CUR
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      * 
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    R02-PREFKBN         TO  SPA-GMN-PREFKBN-G
           MOVE    R02-PRTKBN          TO  SPA-GMN-PRTKBN-G
      *
           MOVE    SPA-GMN-PAGE        TO  IDW
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       36
               MOVE    R02-TSTDATA  (IDX)  TO
                                       SPA-GMN-TSTDATA    (IDW IDX)
               MOVE    R02-TEDDATA  (IDX)  TO
                                       SPA-GMN-TEDDATA    (IDW IDX)
               MOVE    R02-TPRTNM   (IDX)  TO
                                       SPA-GMN-TPRTNM-G   (IDW IDX)
               MOVE    R02-TPRTNM-N (IDX)  TO
                                       SPA-GMN-TPRTNM-N-G (IDW IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
      *    県内・県外
           PERFORM 4106-PREFKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
      *    出力内容
            PERFORM 4107-PRTKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-PAGE        TO  IDW
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       36
      *        出力開始（個別）
               PERFORM 4103-TSTDATA-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *
      *        出力終了（個別）
               PERFORM 4104-TEDDATA-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *       プリンタ名
               PERFORM 4110-TPRTNM-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
      *        プリンタ名（続紙）
               PERFORM 4120-TPRTNM-N-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-PERFORM
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順（社保）入力処理
      *****************************************************************
       4105-PRTJYUN-CHK-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *
           MOVE    R02-PRTJYUN         TO  SPA-GMN-PRTJYUN-G
      *
           IF      SPA-GMN-PRTJYUNMEI  =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-PRTJYUN-MAX
                       OR      SPA-GMN-PRTJYUNLST  (IDX)   =   SPACE
                   IF      SPA-GMN-PRTJYUN =   SPA-GMN-PRTJYUNL (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    SPA-GMN-PRTJYUN-MAX TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF    
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-PRTJYUNLST (1)
                                           TO SPA-GMN-PRTJYUN-G
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF   
      *
           IF      SPA-R01-FILEKBN     =   "1"
               IF      SPA-GMN-PRTJYUN     =   "07"    OR  "08"
                                                       OR  "11"
                                                       OR  "12"
                                                       OR  "13"
                   MOVE    SPA-GMN-PRTJYUN-G   TO  SPA-GMN-PRTJYUN1-G
                                                   SPA-GMN-PRTJYUN2-G
               ELSE
                   IF      SPA-GMN-PRTJYUN-OLD  =  "07"    OR  "08"
                                                           OR  "11"
                                                           OR  "12"
                                                           OR  "13"
                       IF      SPA-GMN-PRTJYUN     =   "06"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >   SPA-GMN-PRTJYUN1-MAX
                                   OR      SPA-GMN-PRTJYUN1L (IDX)
                                               =   SPACE
                               IF      SPA-NAI-2005-PRTJYUNKBN3
                                           =   SPA-GMN-PRTJYUN1L (IDX)
                                   MOVE    SPA-GMN-PRTJYUN1LST(IDX)
                                               TO  SPA-GMN-PRTJYUN1-G
                                   MOVE    SPA-GMN-PRTJYUN1-MAX
                                               TO  IDX
                               END-IF
                           END-PERFORM
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >   SPA-GMN-PRTJYUN2-MAX
                                   OR      SPA-GMN-PRTJYUN2L (IDX)
                                               =   SPACE
                               IF      SPA-NAI-200501-PRTJYUNKBN5
                                           =   SPA-GMN-PRTJYUN2L (IDX)
                                   MOVE    SPA-GMN-PRTJYUN2LST(IDX)
                                               TO  SPA-GMN-PRTJYUN2-G
                                   MOVE    SPA-GMN-PRTJYUN2-MAX
                                               TO  IDX
                               END-IF
                           END-PERFORM
                       ELSE
                           MOVE    SPA-GMN-PRTJYUN-G
                                                TO  SPA-GMN-PRTJYUN1-G
                                                    SPA-GMN-PRTJYUN2-G
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE     SPA-GMN-PRTJYUN-G  TO  SPA-GMN-PRTJYUN1-G
                                               SPA-GMN-PRTJYUN2-G 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-PRTJYUN     TO  SPA-GMN-PRTJYUN-OLD
      *        医師別印刷設定時
               IF      SPA-GMN-PRTJYUN     =  "11"
                   MOVE    SPA-GMN-SYUKALST (1) TO  SPA-GMN-SYUKA-G
               ELSE
      *          ターミナルデジット設定時
                 IF      SPA-GMN-PRTJYUN     =  "13"
                   MOVE    SPACE           TO  SPA-GMN-SYUKA-G
                 ELSE
                   IF    ( SPA-R01-RECE07-SRYKA
                                           =   "00" )
                   AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                           =   "00" )
                   AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                           =   "00" )
                       MOVE    SPACE           TO  SPA-GMN-SYUKA-G
                   ELSE
      *                主科設定時は全科に戻す
                       IF      SPA-GMN-PRTJYUN     =  "07"    OR  "08"
                           IF    ( SPA-R01-RECE07-SRYKA
                                                   NOT =   "00" )
                           AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                  NOT =   "00" )
                           AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                  NOT =   "00" )
                               IF    ( SPA-GMN-SYUKA-MAX   >   ZERO )
                               AND   ( SPA-GMN-SYUKA   NOT =   "00" )
                                   MOVE    SPA-GMN-SYUKALST (1)
                                                   TO  SPA-GMN-SYUKA-G
                               END-IF
                           ELSE
                               MOVE    SPACE       TO  SPA-GMN-SYUKA-G
                           END-IF 
                           MOVE    SPACE       TO  SPA-GMN-SYUKA-G
                       ELSE
                           IF    ( SPA-GMN-SYUKA-MAX   >   ZERO )
                           AND   ( SPA-GMN-SYUKA   NOT =   "00" )
                               MOVE    SPA-GMN-SYUKALST (1)
                                                   TO  SPA-GMN-SYUKA-G
                           END-IF
                       END-IF
                   END-IF
                 END-IF
               END-IF    
      *        頁・件数の編集処理
               MOVE    "T"                 TO  SPA-GMN-SYOKBN
               MOVE    ZERO                TO  FLG-PRTNM-SET
               PERFORM 3101-HENSYU-SEC
           END-IF         
           .
       4105-PRTJYUN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順（国保）入力処理
      *****************************************************************
       4108-PRTJYUN1-CHK-SEC             SECTION.
      *
           IF      SPA-R01-FILEKBN     =   "2"
               GO  TO  4108-PRTJYUN1-CHK-EXT
           END-IF
      *
           MOVE    180                  TO  SPA-GMN-CUR
      *
           MOVE    R02-PRTJYUN1         TO  SPA-GMN-PRTJYUN1-G
      *
           IF      SPA-GMN-PRTJYUN1MEI  =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-PRTJYUN1-MAX
                       OR      SPA-GMN-PRTJYUN1LST  (IDX)   =   SPACE
                   IF      SPA-GMN-PRTJYUN1 =   SPA-GMN-PRTJYUN1L (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    SPA-GMN-PRTJYUN1-MAX
                                                   TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF    
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-PRTJYUN1LST (1)
                                           TO SPA-GMN-PRTJYUN1-G
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF   
      *
           IF      SPA-ERRCD           =   SPACE
      *        主科設定時は全科に戻す
               IF    ( SPA-GMN-SYUKA-MAX   >   ZERO )
                   IF      SPA-GMN-SYUKA   NOT =   "00"
                       MOVE    SPA-GMN-SYUKALST (1) TO  SPA-GMN-SYUKA-G
                   END-IF
                   IF    ( SPA-R01-RECE07-SRYKA
                                           =   "00" )
                   AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                           =   "00" )
                   AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                           =   "00" )
                       MOVE    SPACE           TO  SPA-GMN-SYUKA-G
                   END-IF
               END-IF
      *        頁・件数の編集処理
               MOVE    "T"                 TO  SPA-GMN-SYOKBN
               MOVE    ZERO                TO  FLG-PRTNM-SET
               PERFORM 3101-HENSYU-SEC
           END-IF         
           .
       4108-PRTJYUN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順（広域連合）入力処理
      *****************************************************************
       4108-PRTJYUN2-CHK-SEC             SECTION.
      *
           IF      SPA-R01-FILEKBN     =   "2"
               GO  TO  4108-PRTJYUN2-CHK-EXT
           END-IF
      *
           MOVE    181                  TO  SPA-GMN-CUR
      *
           MOVE    R02-PRTJYUN2         TO  SPA-GMN-PRTJYUN2-G
      *
           IF      SPA-GMN-PRTJYUN2MEI  =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-PRTJYUN2-MAX
                       OR      SPA-GMN-PRTJYUN2LST  (IDX)   =   SPACE
                   IF      SPA-GMN-PRTJYUN2 =   SPA-GMN-PRTJYUN2L (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    SPA-GMN-PRTJYUN2-MAX
                                                   TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF    
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-PRTJYUN2LST (1)
                                           TO SPA-GMN-PRTJYUN2-G
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF   
      *
           IF      SPA-ERRCD           =   SPACE
      *        主科設定時は全科に戻す
               IF    ( SPA-GMN-SYUKA-MAX   >   ZERO )
                   IF      SPA-GMN-SYUKA   NOT =   "00"
                       MOVE    SPA-GMN-SYUKALST (1) TO  SPA-GMN-SYUKA-G
                   END-IF
                   IF    ( SPA-R01-RECE07-SRYKA
                                           =   "00" )
                   AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                           =   "00" )
                   AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                           =   "00" )
                       MOVE    SPACE           TO  SPA-GMN-SYUKA-G
                   END-IF
               END-IF
      *        頁・件数の編集処理
               MOVE    "T"                 TO  SPA-GMN-SYOKBN
               MOVE    ZERO                TO  FLG-PRTNM-SET
               PERFORM 3101-HENSYU-SEC
           END-IF         
           .
       4108-PRTJYUN2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科入力処理
      *****************************************************************
       4109-SYUKA-CHK-SEC             SECTION.
      *
           MOVE    181                 TO  SPA-GMN-CUR
      *
           MOVE    SPA-GMN-SYUKA       TO  WRK-SYUKA-BACK
      *
           MOVE    R02-SYUKA           TO  SPA-GMN-SYUKA-G
      * 
           IF      SPA-GMN-SYUKA       =   SPACE   
               MOVE    SPACE               TO  SPA-GMN-SYUKAMEI
           ELSE
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL   IDY >      SPA-GMN-SYUKA-MAX
                   IF      SPA-GMN-SYUKA  =   SPA-GMN-SYUKAL (IDY)
                       MOVE    SPA-GMN-SYUKALST (IDY)
                                          TO  SPA-GMN-SYUKA-G
                       MOVE    SPA-GMN-SYUKA-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
               IF      SPA-GMN-SYUKAMEI    =   SPACE
                   MOVE    "0012"              TO  SPA-ERRCD
               END-IF    
           END-IF
      *
           IF    ( SPA-ERRCD       NOT =   SPACE          )
           OR    ( SPA-GMN-SYUKA       =   WRK-SYUKA-BACK )
               GO  TO  4109-SYUKA-CHK-EXT
           END-IF
      *    
      *    頁・件数の編集処理
           MOVE    SPA-GMN-SYUKA-G    TO  WRK-GMN-SYUKA-G
           IF      SPA-GMN-SYUKA      =   SPACE
               MOVE    "00"               TO  SPA-GMN-SYUKA
           END-IF
           MOVE    SPA-GMN-PAGE       TO  WRK-GMN-PAGE
           MOVE    1                  TO  FLG-PRTNM-SET
           PERFORM 3101-HENSYU-SEC
           MOVE    WRK-GMN-SYUKA-G    TO  SPA-GMN-SYUKA-G
      *        
           INITIALIZE                  SPA-GMN-TBL
      *
           EVALUATE    SPA-GMN-SYOKBN
      *            *指定印刷
               WHEN    "F"
                   MOVE    4               TO  SPA-GMN-CUR
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       WRK-CONS-PAGE-MAX
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >       WRK-CONS-LINE-MAX
                             OR      SPA-NAI-PRTJYUNI (IDW IDX)
                                           =   SPACE
                       MOVE    "F"     TO  SPA-GMN-TCHK-VALUE (IDW IDX)
                     END-PERFORM 
                   END-PERFORM
                   MOVE    "F"      TO  SPA-GMN-TCHK-VALUE (1 33)
                                        SPA-GMN-TCHK-VALUE (1 34)
                                        SPA-GMN-TCHK-VALUE (1 35)
                                        SPA-GMN-TCHK-VALUE (1 36)
      *            *全件印刷
               WHEN    "T"
                   MOVE    199             TO  SPA-GMN-CUR
                   IF    ( SPA-GMN-SYUKA   =   "00"    OR  SPACE )
                   OR    ( SPA-GMN-PRTJYUN =   "11"    OR  "12"  )
                       PERFORM VARYING IDW FROM    1   BY  1
                               UNTIL   IDW >       WRK-CONS-PAGE-MAX
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       WRK-CONS-LINE-MAX
                                   OR      SPA-NAI-PRTJYUNI (IDW IDX)
                                                       =   SPACE
                               MOVE    "T"     TO
                                           SPA-GMN-TCHK-VALUE (IDW IDX)
                           END-PERFORM
                       END-PERFORM
                       MOVE    "T"      TO  SPA-GMN-TCHK-VALUE (1 33)
                                            SPA-GMN-TCHK-VALUE (1 34)
                                            SPA-GMN-TCHK-VALUE (1 35)
                                            SPA-GMN-TCHK-VALUE (1 36)
                       IF      SPA-NAI-2005-JIHI-PRTKBN
                                            =   "1"
                           MOVE    "F"  TO  SPA-GMN-TCHK-VALUE  (1 35)
                       END-IF
                   ELSE
                       PERFORM VARYING IDW FROM    1   BY  1
                               UNTIL   IDW >       WRK-CONS-PAGE-MAX
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       WRK-CONS-LINE-MAX
                                   OR      SPA-NAI-PRTJYUNI (IDW IDX)
                                                        =   SPACE
                               EVALUATE    SPA-NAI-TEISYUTUSAKI(IDW IDX)
                                   WHEN    "1"
                                       IF  SPA-R01-RECE07-SRYKA
                                                           NOT =   "00"
                                           MOVE    "T"     TO
                                             SPA-GMN-TCHK-VALUE(IDW IDX)
                                       ELSE
                                           MOVE    "F"     TO
                                             SPA-GMN-TCHK-VALUE(IDW IDX)
                                       END-IF
                                   WHEN    "2"
                                       IF  SPA-R01-RECE07-SRYKA-KOKUHO
                                                           NOT =   "00"
                                           MOVE    "T"     TO
                                             SPA-GMN-TCHK-VALUE(IDW IDX)
                                       ELSE
                                           MOVE    "F"     TO
                                             SPA-GMN-TCHK-VALUE(IDW IDX)
                                       END-IF
                                   WHEN    "6"
                                       IF  SPA-R01-RECE07-SRYKA-KOUIKI
                                                           NOT =   "00"
                                           MOVE    "T"     TO
                                             SPA-GMN-TCHK-VALUE(IDW IDX)
                                       ELSE
                                           MOVE    "F"     TO
                                             SPA-GMN-TCHK-VALUE(IDW IDX)
                                       END-IF
                               END-EVALUATE
                           END-PERFORM
                       END-PERFORM
                       MOVE    "F"         TO  SPA-GMN-TCHK-VALUE (1 33)
                                               SPA-GMN-TCHK-VALUE (1 34)
                                               SPA-GMN-TCHK-VALUE (1 35)
                                               SPA-GMN-TCHK-VALUE (1 36)
                   END-IF    
           END-EVALUATE
      *
           IF      SPA-GMN-PAGE-MAX    =   1
               CONTINUE
           ELSE
             IF      SPA-GMN-PRTJYUN1        =   "06"
               IF      SPA-R01-RECE07-SRYKA    NOT =   "00"
                   MOVE    1                   TO  SPA-GMN-PAGE
               ELSE
                   IF      SPA-R01-RECE07-SRYKA-KOKUHO
                                                   NOT =   "00"
                       MOVE    2                   TO  SPA-GMN-PAGE
                   ELSE
                       MOVE    WRK-GMN-PAGE        TO  SPA-GMN-PAGE
                   END-IF
               END-IF    
             ELSE  
               IF    ( SPA-R01-RECE07-SRYKA    NOT =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                               NOT =   "00" )
      *********AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
      *********                                NOT =   "00" )
                   IF      WRK-GMN-PAGE        =   SPA-GMN-PAGE-MAX
                       MOVE    WRK-GMN-PAGE        TO  SPA-GMN-PAGE
                   ELSE
                       IF      WRK-GMN-PAGE        <   3
                           MOVE    WRK-GMN-PAGE        TO  SPA-GMN-PAGE
                       ELSE
                           MOVE    2                   TO  SPA-GMN-PAGE
                       END-IF
                   END-IF
               END-IF    
               IF    ( SPA-R01-RECE07-SRYKA        =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                               NOT =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                               NOT =   "00" )
                   IF      WRK-GMN-PAGE        =   1
                       MOVE    2                   TO  SPA-GMN-PAGE
                   ELSE
                       MOVE    WRK-GMN-PAGE        TO  SPA-GMN-PAGE
                   END-IF
               END-IF    
               IF    ( SPA-R01-RECE07-SRYKA    NOT =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                   =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                               NOT =   "00" )
                   IF      WRK-GMN-PAGE        =   1
                       MOVE    1                   TO  SPA-GMN-PAGE
                   ELSE
                       MOVE    WRK-GMN-PAGE        TO  SPA-GMN-PAGE
                   END-IF
               END-IF    
               IF    ( SPA-R01-RECE07-SRYKA    NOT =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                   =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                   =   "00" )
                   MOVE    1                   TO  SPA-GMN-PAGE
               END-IF
               IF    ( SPA-R01-RECE07-SRYKA        =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                               NOT =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                   =   "00" )
                   MOVE    2                   TO  SPA-GMN-PAGE
               END-IF
               IF    ( SPA-R01-RECE07-SRYKA        =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                   =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                               NOT =   "00" )
                   MOVE    SPA-GMN-PAGE-MAX    TO  SPA-GMN-PAGE
               END-IF
             END-IF    
           END-IF    
           .
       4109-SYUKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力開始（個別）チェック処理
      *****************************************************************
       4103-TSTDATA-CHK-SEC          SECTION.
      *
           EVALUATE    SPA-NAI-TEISYUTUSAKI (IDW IDX)
               WHEN    "1"
                   MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
               WHEN    "2"
                   IF      SPA-NAI-SYUBETU  (IDW IDX 1) =   "6666"
                       MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
                   ELSE
                       MOVE    SPA-GMN-PRTJYUN1   TO  WRK-PRTJYUN
                   END-IF
               WHEN    "6"
                   IF      SPA-NAI-SYUBETU  (IDW IDX 1) =   "6666"
                       MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
                   ELSE
                       MOVE    SPA-GMN-PRTJYUN2   TO  WRK-PRTJYUN
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-SRT-LAST
           IF      WRK-PRTJYUN         =   "06"
               EVALUATE    SPA-NAI-TEISYUTUSAKI (IDW IDX)
                   WHEN    "1"
                       MOVE    "2"                 TO  WRK-SRT-LAST
                   WHEN    "2"
                       IF      SPA-NAI-2008-HYOUJI =   1
                           MOVE    "2"                 TO  WRK-SRT-LAST
                       ELSE
                           IF      SPA-NAI-TPREFKBN-SRT (IDW IDX)
                                                       =   "1"
                               MOVE    SPA-R01-RECE07-SRT-LAST (1)
                                                       TO  WRK-SRT-LAST
                           ELSE
                               MOVE    SPA-R01-RECE07-SRT-LAST (2)
                                                       TO  WRK-SRT-LAST
                           END-IF
                       END-IF
               END-EVALUATE
           END-IF
           IF      WRK-PRTJYUN         =   "11"
               MOVE    SPA-NAI-2005-DRCD-PRTJYUNKBN
                                           TO  WRK-SRT-LAST
           END-IF
           IF      WRK-PRTJYUN         =   "12"
               MOVE    SPA-NAI-2005-BTUNUM-PRTJYUNKBN
                                           TO  WRK-SRT-LAST
           END-IF
      *             
           IF      SPA-GMN-TSTDATA (IDW IDX)  NOT =   SPACE
               EVALUATE    WRK-PRTJYUN ALSO    WRK-SRT-LAST
                   WHEN    "01"        ALSO    ANY
                   WHEN    "03"        ALSO    ANY
                   WHEN    "07"        ALSO    ANY
                   WHEN    "09"        ALSO    ANY
                   WHEN    "06"        ALSO    "1"
                   WHEN    "11"        ALSO    "1"
                   WHEN    "11"        ALSO    "3"
                   WHEN    "12"        ALSO    "1"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TSTDATA (IDW IDX)
                                                   TO  KANACHK-MAE-INPUT
                       MOVE    100                 TO  KANACHK-MAX-CNT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       MOVE    KANACHK-MAE-INPUT   TO
                                               SPA-GMN-TSTDATA (IDW IDX)
                       IF      KANACHK-RC2         =   7
                           MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TSTDATA (IDW IDX)
                       END-IF
                       IF    ( KANACHK-RC          =   ZERO )
                       AND   ( KANACHK-SYUBETU     =   2    )
                           MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TSTDATA (IDW IDX)
                           IF      SPA-GMN-TSTDATA (IDW IDX)(1:2)
                                                  >=  "ア"    AND
                                                  <=  "ヵ"
                               CONTINUE
                           ELSE
                               MOVE    "0001"         TO  SPA-ERRCD
                           END-IF 
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
                   WHEN    "02"        ALSO    ANY
                   WHEN    "04"        ALSO    ANY
                   WHEN    "06"        ALSO    "2"
                   WHEN    "08"        ALSO    ANY
                   WHEN    "10"        ALSO    ANY
                   WHEN    "11"        ALSO    "2"
                   WHEN    "11"        ALSO    "4"
                   WHEN    "12"        ALSO    "2"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TSTDATA (IDW IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "1"
                           IF      SPA-GMN-TSTDATA (IDW IDX)(1:1)
                                              <  "0"    OR
                                              >  "z"
                               MOVE    "0001"             TO  SPA-ERRCD
                           END-IF                        
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
               END-EVALUATE
           END-IF   
      *
           IF      SPA-ERRCD       NOT =   SPACE
               COMPUTE SPA-GMN-CUR        =   2   +
                                              IDX *  3
           END-IF 
           .
       4103-TSTDATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力終了（個別）チェック処理
      *****************************************************************
       4104-TEDDATA-CHK-SEC          SECTION.
      *
           EVALUATE    SPA-NAI-TEISYUTUSAKI (IDW IDX)
               WHEN    "1"
                   MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
               WHEN    "2"
                   IF      SPA-NAI-SYUBETU  (IDW IDX 1) =   "6666"
                       MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
                   ELSE
                       MOVE    SPA-GMN-PRTJYUN1   TO  WRK-PRTJYUN
                   END-IF
               WHEN    "6"
                   IF      SPA-NAI-SYUBETU  (IDW IDX 1) =   "6666"
                       MOVE    SPA-GMN-PRTJYUN    TO  WRK-PRTJYUN
                   ELSE
                       MOVE    SPA-GMN-PRTJYUN2   TO  WRK-PRTJYUN
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-SRT-LAST
           IF      WRK-PRTJYUN         =   "06"
               EVALUATE    SPA-NAI-TEISYUTUSAKI (IDW IDX)
                   WHEN    "1"
                       MOVE    "2"                 TO  WRK-SRT-LAST
                   WHEN    "2"
                       IF      SPA-NAI-2008-HYOUJI =   1
                           MOVE    "2"                 TO  WRK-SRT-LAST
                       ELSE
                           IF      SPA-NAI-TPREFKBN-SRT (IDW IDX)
                                                       =   "1"
                               MOVE    SPA-R01-RECE07-SRT-LAST (1)
                                                       TO  WRK-SRT-LAST
                           ELSE
                               MOVE    SPA-R01-RECE07-SRT-LAST (2)
                                                       TO  WRK-SRT-LAST
                           END-IF
                       END-IF
               END-EVALUATE
           END-IF
           IF      WRK-PRTJYUN         =   "11"
               MOVE    SPA-NAI-2005-DRCD-PRTJYUNKBN
                                           TO  WRK-SRT-LAST
           END-IF
           IF      WRK-PRTJYUN         =   "12"
               MOVE    SPA-NAI-2005-BTUNUM-PRTJYUNKBN
                                           TO  WRK-SRT-LAST
           END-IF
      *
           IF      SPA-GMN-TEDDATA (IDW IDX)  NOT =   SPACE   
               EVALUATE    WRK-PRTJYUN ALSO    WRK-SRT-LAST
                   WHEN    "01"        ALSO    ANY
                   WHEN    "03"        ALSO    ANY
                   WHEN    "07"        ALSO    ANY
                   WHEN    "09"        ALSO    ANY
                   WHEN    "06"        ALSO    "1"
                   WHEN    "11"        ALSO    "1"
                   WHEN    "11"        ALSO    "3"
                   WHEN    "12"        ALSO    "1"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TEDDATA (IDW IDX)
                                                   TO  KANACHK-MAE-INPUT
                       MOVE    100                 TO  KANACHK-MAX-CNT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       MOVE    KANACHK-MAE-INPUT   TO
                                               SPA-GMN-TEDDATA (IDW IDX)
                       IF      KANACHK-RC2         =   7
                           MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TEDDATA (IDW IDX)
                       END-IF
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "2"
                           MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-TEDDATA (IDW IDX)
                           IF      SPA-GMN-TEDDATA (IDW IDX)(1:2)
                                                  >=  "ア"    AND
                                                  <=  "ヵ"
                               CONTINUE
                           ELSE
                               MOVE    "0001"         TO  SPA-ERRCD
                           END-IF 
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
                   WHEN    "02"        ALSO    ANY
                   WHEN    "04"        ALSO    ANY
                   WHEN    "06"        ALSO    "2"
                   WHEN    "08"        ALSO    ANY
                   WHEN    "10"        ALSO    ANY
                   WHEN    "11"        ALSO    "2"
                   WHEN    "11"        ALSO    "4"
                   WHEN    "12"        ALSO    "2"
      *
      *                半角チェック
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "1"                 TO  KANACHK-SYORI 
                       MOVE    SPA-GMN-TEDDATA (IDW IDX)
                                                   TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"       USING
                                                   ORCSKANACHKAREA
                       IF      KANACHK-RC          =   ZERO
                       AND     KANACHK-SYUBETU     =   "1"
                           IF      SPA-GMN-TEDDATA (IDW IDX)(1:1)
                                              <  "0"    OR
                                              >  "z"
                               MOVE    "0001"         TO  SPA-ERRCD
                           END-IF                        
                       ELSE
                           MOVE    "0001"             TO  SPA-ERRCD
                       END-IF
               END-EVALUATE
           END-IF   
      *
           IF      SPA-ERRCD       NOT =   SPACE
               COMPUTE SPA-GMN-CUR        =   3   +
                                              IDX *  3
           END-IF 
           .
       4104-TEDDATA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    県内・県外入力処理
      *****************************************************************
       4106-PREFKBN-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-PREFKBNMEI   =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                       OR      SPA-GMN-PREFKBNLST  (IDX)   =   SPACE
                   IF      SPA-GMN-PREFKBN =   SPA-GMN-PREFKBNL (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    4                   TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF 
           .
       4106-PREFKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力内容入力処理
      *****************************************************************
       4107-PRTKBN-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-PRTKBNMEI   =   SPACE
               MOVE    "0001"               TO  SPA-ERRCD
           ELSE    
               MOVE    ZERO                 TO  WRK-HENFLG
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       3
                       OR      SPA-GMN-PRTKBNLST  (IDX)   =   SPACE
                   IF      SPA-GMN-PRTKBN =   SPA-GMN-PRTKBNL (IDX)
                       MOVE    1                   TO  WRK-HENFLG
                       MOVE    3                   TO  IDX
                   END-IF
               END-PERFORM
               IF      WRK-HENFLG          =   ZERO     
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF    
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    200                TO  SPA-GMN-CUR
           END-IF 
           .
       4107-PRTKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンター入力処理
      *****************************************************************
       4110-TPRTNM-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-TPRTNM (IDW IDX)
                                       =   SPACE   
               MOVE    SPACE           TO  SPA-GMN-TPRTNM-G (IDW IDX)
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                   IF      SPA-GMN-TPRTNM (IDW IDX)
                                           =   SPA-GMN-PRTNML (IDY)
                       MOVE    SPA-GMN-PRTNMLST (IDY)
                                          TO  SPA-GMN-TPRTNM-G (IDW IDX)
                       MOVE    SPA-GMN-PRTNM-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( SPA-GMN-TPRTNMMEI (IDW IDX)
                                          =   SPACE )
           AND   ( SPA-NAI-PRTJYUNI  (IDW IDX)
                                      NOT =   SPACE )
               MOVE    SPA-GMN-PRTNMLST (1)
                                          TO  SPA-GMN-TPRTNM-G (IDW IDX)
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               COMPUTE SPA-GMN-CUR         =   201      +
                                              (IDX - 1) * 2 
           END-IF 
           .
       4110-TPRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンター（続紙）入力処理
      *****************************************************************
       4120-TPRTNM-N-CHK-SEC             SECTION.
      * 
           IF      SPA-GMN-TPRTNM-N (IDW IDX)
                                       =   SPACE   
               MOVE    SPACE           TO  SPA-GMN-TPRTNM-N-G (IDW IDX)
           ELSE
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                   IF      SPA-GMN-TPRTNM-N (IDW IDX)
                                           =   SPA-GMN-PRTNML (IDY)
                       MOVE    SPA-GMN-PRTNMLST (IDY)
                                          TO  SPA-GMN-TPRTNM-N-G
                                                               (IDW IDX)
                       MOVE    SPA-GMN-PRTNM-MAX
                                          TO  IDY
                   END-IF
               END-PERFORM
           END-IF
           IF    ( SPA-GMN-TPRTNM-NMEI (IDW IDX)
                                          =   SPACE )
           AND   ( SPA-NAI-PRTJYUNI  (IDW IDX)
                                      NOT =   SPACE )
               MOVE    SPA-GMN-PRTNMLST (1)
                                          TO  SPA-GMN-TPRTNM-N-G
                                                               (IDW IDX)
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               COMPUTE SPA-GMN-CUR         =   202      +
                                              (IDX - 1) * 2 
           END-IF 
           .
       4120-TPRTNM-N-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *   
      *    レセプトが選択されているか
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       SPA-GMN-PAGE-MAX
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       WRK-CONS-LINE-MAX
               IF      SPA-GMN-TCHK-VALUE (IDW IDX)    =   "T"
                   MOVE    1                   TO  IDZ
                   MOVE    WRK-CONS-LINE-MAX   TO  IDX
               END-IF
             END-PERFORM  
           END-PERFORM
           IF      IDZ                 =   ZERO
               MOVE    "0008"          TO  SPA-ERRCD
           END-IF
      *
      *    主科設定のとき主科が選択されているか
      ****     IF      SPA-GMN-SYUKA-MAX   >   ZERO
      ****         IF      SPA-GMN-SYUKAMEI    =   SPACE
      ****             MOVE    "0012"              TO  SPA-ERRCD
      ****             MOVE    181                 TO  SPA-GMN-CUR
      ****         END-IF
      ****     END-IF
      *
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る前処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
      *********AND     JOB-TERMID      =   SPA-TERMID
                   MOVE    "6666"          TO  SPA-RIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
      *    一時保存テーブル削除処理
           PERFORM 900-SPATMP-DBDELETE-SEC
      *
           MOVE    SPACE           TO  SPA-R99-FLG
           MOVE    SPACE           TO  SPA-RIDCD
           MOVE    SPACE           TO  SPA-RIDCD2      
           MOVE    SPACE           TO  SPA-RIDCD3
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           MOVE    SPACE           TO  SPA-RID1-FLG
                                       SPA-R99-CANCEL
      *
           MOVE    "R03"               TO  SPA-SAKIPG
           MOVE    "R02"               TO  SPA-MOTOPG
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
      *    処理区分  入力処理
      *****************************************************************
       410-SYOKBN-CHK-SEC                SECTION.
      *
           IF    ( SPA-GMN-PRTJYUN    =   "05" 
                                      OR  "13" )
           AND   ( SPA-GMN-SYOKBN     =   "T"  )
               MOVE    "0011"             TO  SPA-ERRCD
               MOVE    199                TO  SPA-GMN-CUR
               GO  TO  410-SYOKBN-CHK-EXT
           END-IF
      *   
           MOVE    1                   TO  SPA-GMN-CUR
      *
           EVALUATE    SPA-GMN-SYOKBN
      *            *指定印刷
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-SYOKBN
      *            *全件印刷
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-SYOKBN
           END-EVALUATE
      *
           MOVE    SPA-GMN-PAGE        TO  WRK-GMN-PAGE
      *
           IF    ( SPA-GMN-SYUKA-MAX   >   ZERO )
           AND   ( SPA-GMN-SYUKA   NOT =   "00" )
      *********MOVE    SPA-GMN-SYUKALST (1) TO  SPA-GMN-SYUKA-G
      *        頁・件数の編集処理
               MOVE    1                    TO  FLG-PRTNM-SET
               PERFORM 3101-HENSYU-SEC
           END-IF
      *
           INITIALIZE                  SPA-GMN-TBL
           EVALUATE    SPA-GMN-SYOKBN
      *            *指定印刷
               WHEN    "F"
                   MOVE    4               TO  SPA-GMN-CUR
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       WRK-CONS-PAGE-MAX
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >       WRK-CONS-LINE-MAX
                             OR      SPA-NAI-PRTJYUNI (IDW IDX)
                                           =   SPACE
                       MOVE    "F"     TO  SPA-GMN-TCHK-VALUE (IDW IDX)
                     END-PERFORM 
                   END-PERFORM
                   MOVE    "F"      TO  SPA-GMN-TCHK-VALUE (1 34)
                                        SPA-GMN-TCHK-VALUE (1 35)
                                        SPA-GMN-TCHK-VALUE (1 36)
                                        SPA-GMN-TCHK-VALUE (1 33)
      *            *全件印刷
               WHEN    "T"
                   MOVE    199             TO  SPA-GMN-CUR
                   INITIALIZE                  SPA-GMN-TBL
                   PERFORM VARYING IDW FROM    1   BY  1
                           UNTIL   IDW >       WRK-CONS-PAGE-MAX
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >       WRK-CONS-LINE-MAX
                             OR      SPA-NAI-PRTJYUNI (IDW IDX)
                                                 =   SPACE
                       MOVE    "T"     TO  SPA-GMN-TCHK-VALUE (IDW IDX)
                     END-PERFORM
                   END-PERFORM
                   IF      SPA-GMN-SYUKA   =   "00"    OR  SPACE
                       MOVE    "T"      TO  SPA-GMN-TCHK-VALUE (1 34)
                                            SPA-GMN-TCHK-VALUE (1 35)
                                            SPA-GMN-TCHK-VALUE (1 36)
                                            SPA-GMN-TCHK-VALUE (1 33)
                       IF      SPA-NAI-2005-JIHI-PRTKBN
                                            =   "1"
                           MOVE    "F"  TO  SPA-GMN-TCHK-VALUE  (1 35)
                       END-IF
                   ELSE
                       MOVE    "F"      TO  SPA-GMN-TCHK-VALUE (1 34)
                                            SPA-GMN-TCHK-VALUE (1 35)
                                            SPA-GMN-TCHK-VALUE (1 36)
                                            SPA-GMN-TCHK-VALUE (1 33)
                   END-IF 
           END-EVALUATE
      *
           MOVE    WRK-GMN-PAGE        TO  SPA-GMN-PAGE
      *
           .
       410-SYOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別出力  入力処理
      *****************************************************************
       420-CHK-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  420-CHK-CHK-EXT
           END-IF
      *
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
           MOVE    WRK-WIDGET2         TO  IDX 
      *
           IF      SPA-GMN-SYOKBN     =   "T"
               MOVE    "0006"             TO  SPA-ERRCD
               MOVE    199                TO  SPA-GMN-CUR
               GO  TO  420-CHK-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-PRTJYUN    =   "05"    OR  "13"
               MOVE    "0011"             TO  SPA-ERRCD
               MOVE    199                TO  SPA-GMN-CUR
               GO  TO  420-CHK-CHK-EXT
           END-IF
      *
           MOVE    1                  TO  SPA-GMN-CUR
      *
           COMPUTE SPA-GMN-CUR        =   SPA-GMN-CUR     +
                                          IDX *  3
           MOVE    SPA-GMN-PAGE       TO  IDW                               
      *
           IF    ( R02-TCHK-VALUE (IDX)   =   "T"  )
           AND   ( SPA-GMN-TPAGE  (IDW IDX)
                                          =   ZERO )
               MOVE    "0002"                 TO  SPA-ERRCD
               MOVE    199                    TO  SPA-GMN-CUR
           ELSE     
               MOVE    R02-TCHK-VALUE (IDX)   TO
                                          SPA-GMN-TCHK-VALUE (IDW IDX)
           END-IF                               
      *
           IF      SPA-ERRCD          =   SPACE
               ADD     1                  TO  SPA-GMN-CUR
           END-IF
           .
       420-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    件数出力前処理
      *****************************************************************
       440-PRINT-KENSU-GIDMSG-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
                   GO  TO  440-PRINT-KENSU-GIDMSG-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   GO  TO  440-PRINT-KENSU-GIDMSG-EXT
               END-IF
           END-IF    
      *
      *    ガイドメッセージセット
           MOVE    "1003"              TO  SPA-RIDCD
      *
           .
       440-PRINT-KENSU-GIDMSG-EXT.
           EXIT.
      *
      *----(05.00.00)--UPD-START---
      *****************************************************************
      *    出力処理
      *****************************************************************
       440-PRINT-KENSU-SEC                SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"  USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
             OR  ( SPA-PRTCONF         =   "1" )
               INITIALIZE                  ORCSPRTCTRLAREA
               CALL    "ORCSPRTCTRL"       USING
                                           SPA-AREA
                                           ORCSPRTCTRLAREA
                                           MCPAREA
           END-IF
      *
      *----(03.04.01)--UPD-START---
           MOVE    "01"                TO  SPA-PRT-FLG
           EVALUATE   SPA-R01-RECE07-NYUGAIKBN
               WHEN   "1"
                   CALL    "ORCHCM35"  USING   SPA-AREA
                                               SPA-NAI-2005-SYAPRTNM-N
                                               SPA-R02
                                               SPA-SCR-REC
      *                                         SPA-GMN-FILEKBN
      *                                         SPA-GMN-TBL1
               WHEN   "2"
                   CALL    "ORCHCM35"  USING   SPA-AREA
                                               SPA-NAI-2005-SYAPRTNM
                                               SPA-R02
                                               SPA-SCR-REC
      *                                         SPA-GMN-FILEKBN
      *                                         SPA-GMN-TBL1
           END-EVALUATE
           MOVE    SPACE               TO  SPA-PRT-FLG
      *----(03.04.01)--UPD-END-----
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
           .
       440-PRINT-KENSU-EXT.
           EXIT.
      *
      *----(05.00.00)--UPD-END-----
      *****************************************************************
      *    出力前処理
      *****************************************************************
       450-PRINT-GIDMSG-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  450-PRINT-GIDMSG-EXT
           END-IF
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "7777"          TO  SPA-ERRCD
                   GO  TO  450-PRINT-GIDMSG-EXT
               END-IF
           END-IF    
      *
      *    ガイドメッセージセット
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "T"
                   MOVE    "1001"              TO  SPA-RIDCD3
               WHEN    "F"
                   MOVE    "1002"              TO  SPA-RIDCD3
           END-EVALUATE
      *
           .
       450-PRINT-GIDMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       450-PRINT-SEC                SECTION.
      *
      *    フラグ初期化
           MOVE    ZERO            TO  SPA-R01-JOBCNT       
                                       SPA-R01-STOPFLG    
           MOVE    SPACE           TO  SPA-R99-CANCEL
      *
      *    ＨＡＯＲＩのときＭＯＮＢＬＯＢ−ＩＤを設定
           IF      SPA-R01-JOB-PARAM
                               NOT =   SPACE
               MOVE    "MONBLOBNEW"    TO  MCP-FUNC
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "monblob"       TO  MCP-TABLE
               CALL    "MONFUNC"       USING
                                           MCPAREA
                                           MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  450-PRINT-EXT
               END-IF
               MOVE    MONBLOB-ID      TO  SPA-R01-MONBLOB-ID
           END-IF
      *
      *    レセ印刷ＤＢの初期化
           INITIALIZE                  RECE08-REC
grpsys     MOVE    SPA-R01-RECE07-HOSPNUM
                                       TO  RECE08-HOSPNUM
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  RECE08-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  RECE08-RECEYM
           ELSE
               MOVE    ZERO                TO  RECE08-RECEYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD
                                       TO  RECE08-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS
                                       TO  RECE08-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  RECE08-NYUGAIKBN
           MOVE    RECE08-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
      *    印刷ＤＢのＫＥＹ用     
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
           MOVE    SMCNDATE-HMS        TO  SYS-TIME
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP                                
           MOVE    WRK-SYSYMD          TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (1:8)
           MOVE    SYS-TIME            TO
                                   WRK-CONS-PRTKANRI-TBL-GROUP (9:)
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SPA-R01-PRTKANRI-TBL-GROUP
      *
      *    ジョブ開始セット
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-R01-JOB-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    SPA-R01-JOB-ORCA-UID
                                   TO  JOB-ORCA-UID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    帳票ファイル作成処理
           MOVE    ZERO            TO  WRK-RECE08-PRTJYUN  
           MOVE    SPACE           TO  WRK-SRT-LAST
      *
      *    入力順指定
           IF      SPA-GMN-PRTJYUN =   "05"
               PERFORM 4502-RECEPRT-INPUT-HENSYU-SEC
           ELSE
            EVALUATE    SPA-GMN-PRTJYUN
             WHEN   "07"
             WHEN   "08"
             WHEN   "13"
      *        全体指定
               EVALUATE    SPA-GMN-SYUKA
                   WHEN    "00"
                       PERFORM VARYING IDXX    FROM    2   BY  1
                               UNTIL   IDXX    >   SPA-GMN-SYUKA-MAX
                               OR      SPA-ERRCD   NOT =   SPACE
                           MOVE    SPA-GMN-SYUKAL (IDXX)
                                                   TO  WRK-SYUKA
                           PERFORM 4504-ALL-PRTJYUN-PRINT-SEC
                       END-PERFORM
                   WHEN    SPACE
                       MOVE    "%%"             TO  WRK-SYUKA
                       PERFORM 4504-ALL-PRTJYUN-PRINT-SEC
                   WHEN    OTHER
                       MOVE    SPA-GMN-SYUKA   TO  WRK-SYUKA
                       PERFORM VARYING IDXX    FROM    2   BY  1
                               UNTIL   IDXX    >   SPA-GMN-SYUKA-MAX
                           IF      WRK-SYUKA   =
                                           SPA-GMN-SYUKAL (IDXX)
                               PERFORM 4504-ALL-PRTJYUN-PRINT-SEC
      *                        MOVE    SPA-GMN-SYUKA-MAX   TO  IDXX
                           END-IF
                       END-PERFORM
               END-EVALUATE
             WHEN   "11"
      *        医師別指定
               MOVE    SPA-NAI-2005-DRCD-PRTJYUNKBN
                                           TO  WRK-SRT-LAST
               EVALUATE    SPA-GMN-SYUKA
                   WHEN    "00"
                   WHEN    SPACE
                       MOVE    "%%"             TO  WRK-SYUKA
                       PERFORM 4505-DRCD-PRTJYUN-PRINT-SEC
                   WHEN    OTHER
                       MOVE    SPA-GMN-SYUKA   TO  WRK-SYUKA
                       PERFORM VARYING IDXX    FROM    2   BY  1
                               UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                           IF      WRK-SYUKA   =
                                               SPA-GMN-SYUKAL (IDXX)
                               PERFORM 4505-DRCD-PRTJYUN-PRINT-SEC
                               MOVE    SPA-GMN-SYUKA-MAX    TO  IDXX
                           END-IF
                       END-PERFORM        
               END-EVALUATE
             WHEN   "12"
      *        病棟別指定
               MOVE    SPA-NAI-2005-BTUNUM-PRTJYUNKBN
                                        TO  WRK-SRT-LAST
               MOVE    "1"              TO  WRK-TEISYUTUSAKI
               MOVE    "%"              TO  WRK-PREFKBN
      *
               PERFORM VARYING IDW FROM    1   BY  1
                       UNTIL   IDW >       SPA-GMN-PAGE-MAX1
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-CONS-LINE-MAX
                           OR      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                           OR      SPA-ERRCD               NOT =   SPACE
                       IF    ( IDW                 =   1   )
                       AND   ( IDX                 >=  33  )
                           CONTINUE
                       ELSE 
                           IF    ( SPA-GMN-TCHK-VALUE  (IDW IDX)
                                                               =   "T" )
                           AND   ( SPA-GMN-TPAGE       (IDW IDX)
                                                               >   ZERO)
                               MOVE    1               TO  IDY
                               PERFORM 4506-BTUNUM-PRTJYUN-PRINT-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
             WHEN   OTHER
      *        社保出力
               IF      SPA-GMN-PRTJYUN =   "03"    OR  "04"
                   MOVE    1               TO  WRK-PAGE-MAX
               ELSE
                   MOVE    SPA-R01-RECE07-HKNJANUM-MAX
                                           TO  WRK-PAGE-MAX
               END-IF
               PERFORM VARYING IDW     FROM    1   BY  1
                       UNTIL   IDW     >       WRK-PAGE-MAX
                 IF      SPA-R01-RECE07-SRYKA    =   "00"
                   MOVE    "%%"             TO  WRK-SYUKA
                   PERFORM 4501-SYAHO-PRTJYUN-PRINT-SEC
                 ELSE
                   EVALUATE    SPA-GMN-SYUKA
                       WHEN    "00"
                           PERFORM VARYING IDXX    FROM    2   BY  1
                                   UNTIL   IDXX    >   SPA-GMN-SYUKA-MAX
                                   OR      SPA-ERRCD   NOT =   SPACE
                               MOVE    SPA-GMN-SYUKAL (IDXX)
                                                       TO  WRK-SYUKA
                               PERFORM 4501-SYAHO-PRTJYUN-PRINT-SEC
                           END-PERFORM
                       WHEN    SPACE
                           MOVE    "%%"             TO  WRK-SYUKA
                           PERFORM 4501-SYAHO-PRTJYUN-PRINT-SEC
                       WHEN    OTHER
                           MOVE    SPA-GMN-SYUKA   TO  WRK-SYUKA
                           PERFORM VARYING IDXX    FROM    2   BY  1
                                   UNTIL   IDXX    >   SPA-GMN-SYUKA-MAX
                               IF      WRK-SYUKA   =
                                               SPA-GMN-SYUKAL (IDXX)
                                   PERFORM 4501-SYAHO-PRTJYUN-PRINT-SEC
      *
                                   MOVE    SPA-GMN-SYUKA-MAX   TO  IDXX
                               END-IF
                           END-PERFORM
                   END-EVALUATE
                 END-IF              
               END-PERFORM
      *
      *        国保出力
               IF      SPA-ERRCD           =   SPACE
                 IF      SPA-R01-RECE07-SRYKA-KOKUHO     =   "00"
                   MOVE    "%%"             TO  WRK-SYUKA
                   PERFORM 4501-KOKUHO-PRTJYUN-PRINT-SEC
                 ELSE
                   EVALUATE    SPA-GMN-SYUKA
                       WHEN    "00"
                           PERFORM VARYING IDXX    FROM    2   BY  1
                                   UNTIL   IDXX    >   SPA-GMN-SYUKA-MAX
                                   OR      SPA-ERRCD   NOT =   SPACE
                               MOVE    SPA-GMN-SYUKAL (IDXX)
                                                       TO  WRK-SYUKA
                               PERFORM 4501-KOKUHO-PRTJYUN-PRINT-SEC
                           END-PERFORM
                       WHEN    SPACE
                           MOVE    "%%"             TO  WRK-SYUKA
                           PERFORM 4501-KOKUHO-PRTJYUN-PRINT-SEC
                       WHEN    OTHER
                           MOVE    SPA-GMN-SYUKA   TO  WRK-SYUKA
                           PERFORM VARYING IDXX    FROM    2   BY  1
                               UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                               IF      WRK-SYUKA   =
                                               SPA-GMN-SYUKAL (IDXX)
                                   PERFORM 4501-KOKUHO-PRTJYUN-PRINT-SEC
      *
                                   MOVE    SPA-GMN-SYUKA-MAX    TO  IDXX
                               END-IF
                           END-PERFORM        
                   END-EVALUATE
                 END-IF
               END-IF 
      *
      *        広域連合出力
               IF      SPA-ERRCD           =   SPACE
                 IF      SPA-R01-RECE07-SRYKA-KOUIKI   =   "00"
                   MOVE    "%%"             TO  WRK-SYUKA
                   PERFORM 4501-KOUIKI-PRTJYUN-PRINT-SEC
                 ELSE
                   EVALUATE    SPA-GMN-SYUKA
                       WHEN    "00"
                           PERFORM VARYING IDXX    FROM    2   BY  1
                                   UNTIL   IDXX    >   SPA-GMN-SYUKA-MAX
                                   OR      SPA-ERRCD   NOT =   SPACE
                               MOVE    SPA-GMN-SYUKAL (IDXX)
                                                       TO  WRK-SYUKA
                               PERFORM 4501-KOUIKI-PRTJYUN-PRINT-SEC
                           END-PERFORM
                       WHEN    SPACE
                           MOVE    "%%"             TO  WRK-SYUKA
                           PERFORM 4501-KOUIKI-PRTJYUN-PRINT-SEC
                       WHEN    OTHER
                           MOVE    SPA-GMN-SYUKA   TO  WRK-SYUKA
                           PERFORM VARYING IDXX    FROM    2   BY  1
                               UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                               IF      WRK-SYUKA   =
                                               SPA-GMN-SYUKAL (IDXX)
                                   PERFORM 4501-KOUIKI-PRTJYUN-PRINT-SEC
      *
                                   MOVE    SPA-GMN-SYUKA-MAX    TO  IDXX
                               END-IF
                           END-PERFORM        
                   END-EVALUATE
                 END-IF
               END-IF 
            END-EVALUATE
      *
            IF      SPA-GMN-PRTJYUN =   "13"
               CONTINUE
            ELSE
               MOVE    "%%"             TO  WRK-SYUKA
      *        特別療養費（広域連合）レセプト出力
               IF    ( SPA-ERRCD        =   SPACE )
                   IF    ( SPA-GMN-TCHK-VALUE  (1 33)   =   "T"  )
                   AND   ( SPA-GMN-TPAGE       (1 33)   >   ZERO )
                       MOVE    1           TO  IDW
                       MOVE    "6"         TO  WRK-TEISYUTUSAKI
                       MOVE    "%"         TO  WRK-PREFKBN
                       MOVE    33          TO  IDX
                       MOVE    1           TO  IDY
                       PERFORM 45011-RECEPRT-HENSYU-SEC
                   END-IF
               END-IF
      *        特別療養費（国保）レセプト出力
               IF    ( SPA-ERRCD        =   SPACE )
                   IF    ( SPA-GMN-TCHK-VALUE  (1 34)   =   "T"  )
                   AND   ( SPA-GMN-TPAGE       (1 34)   >   ZERO )
                       MOVE    1           TO  IDW
                       MOVE    "2"         TO  WRK-TEISYUTUSAKI
                       MOVE    "%"         TO  WRK-PREFKBN
                       MOVE    34          TO  IDX
                       MOVE    1           TO  IDY
                       PERFORM 45011-RECEPRT-HENSYU-SEC
                   END-IF
               END-IF
      *
      *        自費レセプト出力
               IF    ( SPA-ERRCD        =   SPACE )
                   IF      SPA-GMN-TCHK-VALUE  (1 35)   =   "T"
                   AND     SPA-GMN-TPAGE       (1 35)   >   ZERO
                       MOVE    1               TO  IDW
                       MOVE    "%"             TO  WRK-TEISYUTUSAKI
                       MOVE    "%"             TO  WRK-PREFKBN
                       MOVE    35              TO  IDX
                       MOVE    1               TO  IDY
                       PERFORM 45011-RECEPRT-HENSYU-SEC
                   END-IF
               END-IF
      *
      *        レセプト不明分出力
               IF    ( SPA-ERRCD        =   SPACE )
                   IF      SPA-GMN-TCHK-VALUE  (1 36)   =   "T"
                   AND     SPA-GMN-TPAGE       (1 36)   >   ZERO
                       MOVE    1               TO  IDW
                       MOVE    "%"             TO  WRK-TEISYUTUSAKI
                       MOVE    "%"             TO  WRK-PREFKBN
                       MOVE    36              TO  IDX
                       MOVE    1               TO  IDY
                       PERFORM 45011-RECEPRT-HENSYU-SEC
                   END-IF
               END-IF
            END-IF
           END-IF
      *
      *    印刷処理開始（ＯＲＣＲ０１０２）
      *
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept2.sh"    TO  SHELLTBL-NAME
      *     
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG2
           MOVE    ZERO            TO  WRK-SHELL-SRYYM
           MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
           MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG3 
           MOVE    "1"             TO  SHELLTBL-ARG4
           IF      SPA-R01-FILEKBN =   "1"
               MOVE    "1"             TO  SHELLTBL-ARG5
           ELSE
               MOVE    "2"             TO  SHELLTBL-ARG5
           END-IF    
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG6
           MOVE    WRK-RECE09-KEY  TO  SHELLTBL-ARG7 
           MOVE    "00000"         TO  SHELLTBL-ARG8
           MOVE    "99999"         TO  SHELLTBL-ARG9
      *****EVALUATE   SPA-R01-RECE07-NYUGAIKBN
      *       WHEN   "1"
      *           MOVE    SPA-NAI-2005-SYAPRTNM-N
      *                            TO  SHELLTBL-ARG11
      *           MOVE    SPA-NAI-2005-KKHPRTNM-N
      *                            TO  SHELLTBL-ARG12
      *       WHEN   "2"
      *           MOVE    SPA-NAI-2005-SYAPRTNM
      *                            TO  SHELLTBL-ARG11
      *           MOVE    SPA-NAI-2005-KKHPRTNM
      *                            TO  SHELLTBL-ARG12
      *****END-EVALUATE
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           MOVE    1               TO  LNK-PRTKANRI-RENNUM
                                       SPA-LNK-XC01-RENNUM
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY
                                       SPA-LNK-XC01-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
                                       SPA-LNK-XC01-TBL-GROUP
           MOVE    SPA-R01-RECE07-SRYYM
                                   TO  LNK-PRTKANRI-SRYYM 
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "recept2.sh"    TO  LNK-PRTKANRI-SHELLID
           IF      SPA-R01-RECE07-NYUGAIKBN    =   "1"
               MOVE    1               TO  LNK-PRTKANRI-PRIORITY
           ELSE
               MOVE    2               TO  LNK-PRTKANRI-PRIORITY
           END-IF
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
      *****MOVE    SYS-1031-PRTNM (5)
      *****                            TO  LNK-PRTKANRI-PRTNM
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG13       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG14     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG15       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG16       
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG17       
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG18       
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG19       
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG20      
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG21       
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG22      
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG23
           MOVE    SPA-GMN-PRTKBN  TO  SHELLTBL-ARG24
      *****EVALUATE   SPA-R01-RECE07-NYUGAIKBN
      *        WHEN   "1"
      *            MOVE    SPA-NAI-2005-SYAPRTNM-NEXT-N
      *                            TO  SHELLTBL-ARG25
      *            MOVE    SPA-NAI-2005-KKHPRTNM-NEXT-N
      *                            TO  SHELLTBL-ARG26
      *        WHEN   "2"
      *            MOVE    SPA-NAI-2005-SYAPRTNM-NEXT
      *                            TO  SHELLTBL-ARG25
      *            MOVE    SPA-NAI-2005-KKHPRTNM-NEXT
      *                            TO  SHELLTBL-ARG26
      *****END-EVALUATE
           EVALUATE    SPA-RID1-FLG
               WHEN    "PV"
                   MOVE    "4"             TO  SHELLTBL-ARG27
               WHEN    "NO"
                   MOVE    "3"             TO  SHELLTBL-ARG27
               WHEN    "OK"
                   MOVE    "2"             TO  SHELLTBL-ARG27
           END-EVALUATE
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG30
           IF      SPA-R01-JOB-PARAM
                               NOT =   SPACE
               MOVE    SPA-R01-JOB-PARAM
                                       TO  SHELLTBL-ARG31
               MOVE    SPA-R01-MONBLOB-ID
                                       TO  SHELLTBL-ARG32
               MOVE    "receipt_print" TO  SHELLTBL-ARG33
      *
               DISPLAY "SPA-R01-MONBLOB-ID=" SPA-R01-MONBLOB-ID "#"
           ELSE
               MOVE    "orca"          TO  SHELLTBL-ARG31
           END-IF
      *
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           COMPUTE SPA-R01-JOBCNT  =   WRK-RECE08-PRTJYUN  +   1
      *                         
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       450-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順出力処理（社保）
      *****************************************************************
       4501-SYAHO-PRTJYUN-PRINT-SEC    SECTION.
      *
      **** MOVE    1                TO  IDW 
           MOVE    "1"              TO  WRK-TEISYUTUSAKI
           MOVE    "0"              TO  WRK-PREFKBN
           IF      SPA-GMN-PRTJYUN     =   "06"
               MOVE    "2"                 TO  WRK-SRT-LAST
           ELSE
               MOVE    SPACE               TO  WRK-SRT-LAST
           END-IF
      *
           PERFORM 4501-PRTJYUN-PRINT-SEC
           .
       4501-SYAHO-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順出力処理（国保）
      *****************************************************************
       4501-KOKUHO-PRTJYUN-PRINT-SEC    SECTION.
      *
      *    編綴順指定
           IF      SPA-GMN-PRTJYUN1    =   "06"
               COMPUTE IDWW    =   SPA-R01-RECE07-HKNJANUM-MAX
                               +   1
               MOVE    "2"              TO  WRK-TEISYUTUSAKI
               IF      SPA-NAI-2008-HYOUJI =   1
                   PERFORM 4503-RECEPRT-INPUT-HENSYU-SEC
               ELSE
                   PERFORM 4503-RECEPRT-INPUT-SYOSAI-SEC
               END-IF
           ELSE
      *        国保対象頁設定
               IF    ( SPA-GMN-PRTJYUN     =   "03"   OR  "04" )
               OR    ( SPA-R01-RECE07-HKNJANUM-SRT
                                           =   "9"             )
                   IF    ( SPA-GMN-PRTJYUN1    =   "01"   OR  "02"   OR
                                                   "09"   OR  "10"     )
                   OR    ( SPA-GMN-PRTJYUN     =   "01"   OR  "02"   OR
                                                   "09"   OR  "10"     )
                       MOVE    2               TO  IDW
                   ELSE
                       MOVE    1               TO  IDW
                   END-IF
               ELSE
                   COMPUTE IDW =   SPA-R01-RECE07-HKNJANUM-MAX
                               +   1
               END-IF
      *
               MOVE    "2"             TO  WRK-TEISYUTUSAKI
      *        国保（県内・県外混在）出力
               IF      SPA-GMN-PREFKBN =   "4"
                   MOVE    "%"             TO  WRK-PREFKBN
                   PERFORM 4501-PRTJYUN-PRINT-SEC
               ELSE
      *
      *            国保（県内分）出力
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF      SPA-GMN-PREFKBN =   "1" OR  "2"
                           MOVE    "1"     TO  WRK-PREFKBN
                           PERFORM 4501-PRTJYUN-PRINT-SEC
                       END-IF
                   END-IF
      *
      *            国保（県外分）出力
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF      SPA-GMN-PREFKBN =   "1" OR  "3"
                           MOVE    "2"      TO  WRK-PREFKBN
                           PERFORM 4501-PRTJYUN-PRINT-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       4501-KOKUHO-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    印刷順出力処理（広域連合）
      *****************************************************************
       4501-KOUIKI-PRTJYUN-PRINT-SEC    SECTION.
      *
      *    編綴順指定
      *****IF      SPA-GMN-PRTJYUN2    =   "06"
      *****    MOVE    "6"              TO  WRK-TEISYUTUSAKI
      *****    IF      SPA-NAI-2008-HYOUJI =   1
      *****        PERFORM 4503-RECEPRT-INPUT-HENSYU-SEC
      *****    ELSE
      *****        PERFORM 4503-RECEPRT-INPUT-SYOSAI-SEC
      *****    END-IF
      *****ELSE
      *        広域連合対象頁設定
               MOVE    SPA-GMN-PAGE-MAX    TO  IDW
      *
               MOVE    "6"             TO  WRK-TEISYUTUSAKI
      *        国保（県内・県外混在）出力
               IF      SPA-GMN-PREFKBN =   "4"
                   MOVE    "%"             TO  WRK-PREFKBN
                   PERFORM 4501-PRTJYUN-PRINT-SEC
               ELSE
      *
      *            広域連合（県内分）出力
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF      SPA-GMN-PREFKBN =   "1" OR  "2"
                           MOVE    "1"     TO  WRK-PREFKBN
                           PERFORM 4501-PRTJYUN-PRINT-SEC
                       END-IF
                   END-IF
      *
      *            広域連合（県外分）出力
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF      SPA-GMN-PREFKBN =   "1" OR  "3"
                           MOVE    "2"      TO  WRK-PREFKBN
                           PERFORM 4501-PRTJYUN-PRINT-SEC
                       END-IF
                   END-IF
               END-IF
      *****END-IF
           .
       4501-KOKUHO-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順出力処理
      *****************************************************************
       4501-PRTJYUN-PRINT-SEC         SECTION.
      *
      *    印刷順にファイル読み込み
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-LINE-MAX
                   OR      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                   OR      SPA-ERRCD               NOT =   SPACE    
               IF      SPA-GMN-TCHK-VALUE  (IDW IDX)   =   "T"
               AND     SPA-GMN-TPAGE       (IDW IDX)   >   ZERO
               AND     SPA-NAI-TEISYUTUSAKI(IDW IDX)
                                               =   WRK-TEISYUTUSAKI
      *
      *            診療科指定のとき
                   IF    ( SPA-GMN-SYUKA       =   "00" )
                   AND   ( WRK-SYUKA       NOT =   "%%" )
                   AND   ( SPA-NAI-TPAGE-SYUKA (IDW IDX IDXX)
                                               =   0    )
                       CONTINUE
                   ELSE
      *★★    
                       PERFORM VARYING IDY FROM    1   BY  1
      **                       UNTIL   IDY >       5
                               UNTIL   IDY >       1
                               OR      SPA-NAI-SYUBETU (IDW IDX IDY)
                                                           =   SPACE
                               OR      SPA-ERRCD       NOT =   SPACE
                           PERFORM 45011-RECEPRT-HENSYU-SEC  
                       END-PERFORM
                   END-IF
               END-IF
           END-PERFORM
           .
       4501-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       45011-RECEPRT-HENSYU-SEC         SECTION.
      *
      *    印刷用パラメタ
           INITIALIZE                      WRK-RECE09-REC
                                           WRK-RECE09-STDATA
                                           WRK-RECE09-EDDATA
      *
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  WRK-RECE09-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  WRK-RECE09-SRYYM
           ELSE
               MOVE    ZERO                TO  WRK-RECE09-SRYYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD   TO  WRK-RECE09-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS   TO  WRK-RECE09-CREHMS
      *
           MOVE    SPA-NAI-SYUBETU (IDW IDX IDY)
                                       TO  WRK-RECE09-RECESYUBETU
           IF    ( SPA-GMN-PRTJYUN     =   "07"    OR  "08"
                                                   OR  "11"
                                                   OR  "12"
                                                   OR  "13" )
           AND   ( SPA-NAI-SYUBETU (IDW IDX IDY)
                                       =   "9999"           )        
               MOVE    "%"                 TO  WRK-RECE09-TEISYUTUSAKI
           ELSE
               MOVE    WRK-TEISYUTUSAKI    TO  WRK-RECE09-TEISYUTUSAKI
           END-IF
           MOVE    WRK-PREFKBN         TO  WRK-RECE09-PREFKBN
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  WRK-RECE09-NYUGAIKBN
           IF      SPA-NAI-SYUBETU (IDW IDX IDY)
                                       =   "6666"
               MOVE    SPA-GMN-PRTJYUN     TO  WRK-RECE09-PRTJYUN
           ELSE
               IF      WRK-TEISYUTUSAKI    =   "6"
                   MOVE    SPA-GMN-PRTJYUN2    TO  WRK-RECE09-PRTJYUN
               ELSE
                   IF      WRK-TEISYUTUSAKI    =   "2"
                       MOVE    SPA-GMN-PRTJYUN1
                                               TO  WRK-RECE09-PRTJYUN
                   ELSE
                       MOVE    SPA-GMN-PRTJYUN TO  WRK-RECE09-PRTJYUN
                   END-IF
               END-IF
           END-IF
      *
           IF    ( WRK-TEISYUTUSAKI    =   "1" )
           AND   ( SPA-R01-RECE07-HKNJANUM-SRT
                                       =   "8" ) 
               MOVE    SPA-NAI-THKNJANUM-SYAHO (IDW)
                                           TO  WRK-RECE09-HKNJANUM
           ELSE
               MOVE    "%%%%%%%%"          TO  WRK-RECE09-HKNJANUM
           END-IF
      *
           MOVE    SPA-GMN-TSTDATA (IDW IDX)
                                       TO  WRK-RECE09-STDATA
           MOVE    SPA-GMN-TEDDATA (IDW IDX)
                                       TO  WRK-RECE09-EDDATA 
           IF      WRK-RECE09-STDATA   =   SPACE
               EVALUATE    WRK-RECE09-PRTJYUN  ALSO    WRK-SRT-LAST
                   WHEN    "01"                ALSO    ANY
                   WHEN    "03"                ALSO    ANY
                   WHEN    "07"                ALSO    ANY
                   WHEN    "09"                ALSO    ANY
                   WHEN    "06"                ALSO    "1"
                   WHEN    "11"                ALSO    "1"
                   WHEN    "11"                ALSO    "3"
                   WHEN    "12"                ALSO    "1"
                       MOVE    SPACE       TO  WRK-RECE09-STDATA
                   WHEN    OTHER
                       MOVE    "0"         TO  WRK-RECE09-STDATA (1:1)
               END-EVALUATE
           ELSE     
               EVALUATE    WRK-RECE09-PRTJYUN  ALSO    WRK-SRT-LAST
                   WHEN    "02"                ALSO    ANY
                   WHEN    "04"                ALSO    ANY
                   WHEN    "06"                ALSO    "2"
                   WHEN    "08"                ALSO    ANY
                   WHEN    "10"                ALSO    ANY
                   WHEN    "11"                ALSO    "2"
                   WHEN    "11"                ALSO    "4"
                   WHEN    "12"                ALSO    "2"
                   WHEN    "13"                ALSO    ANY
                       PERFORM VARYING IDZ FROM    20  BY  -1
                               UNTIL   IDZ <       1
                           IF      WRK-RECE09-STDATA(IDZ:1)
                                                   NOT =   SPACE
                               MOVE    1   TO  IDZ 
                           ELSE  
                               MOVE    "Z" TO  WRK-RECE09-STDATA(IDZ:1)
                           END-IF
                       END-PERFORM
               END-EVALUATE
           END-IF
           IF      WRK-RECE09-EDDATA   =   SPACE
               EVALUATE    WRK-RECE09-PRTJYUN  ALSO    WRK-SRT-LAST
                   WHEN    "01"                ALSO    ANY
                   WHEN    "03"                ALSO    ANY
                   WHEN    "07"                ALSO    ANY
                   WHEN    "09"                ALSO    ANY
                   WHEN    "06"                ALSO    "1"
                   WHEN    "11"                ALSO    "1"
                   WHEN    "11"                ALSO    "3"
                   WHEN    "12"                ALSO    "1"
                       IF      SPA-ENCODING        =   2
                           MOVE    "ｚ"    TO  WRK-RECE09-EDDATA (1:2)
                       ELSE
                           MOVE    "ヵ"    TO  WRK-RECE09-EDDATA (1:2)
                       END-IF
                   WHEN    OTHER
                       MOVE    "zzzzzzzzzzzzzzzzzzzz"
                                       TO  WRK-RECE09-EDDATA (1:20)
               END-EVALUATE
           ELSE     
               EVALUATE    WRK-RECE09-PRTJYUN  ALSO    WRK-SRT-LAST
                   WHEN    "02"                ALSO    ANY
                   WHEN    "04"                ALSO    ANY
                   WHEN    "06"                ALSO    "2"
                   WHEN    "08"                ALSO    ANY
                   WHEN    "10"                ALSO    ANY
                   WHEN    "11"                ALSO    "2"
                   WHEN    "11"                ALSO    "4"
                   WHEN    "12"                ALSO    "2"
                   WHEN    "13"                ALSO    ANY
                       PERFORM VARYING IDZ FROM    20  BY  -1
                               UNTIL   IDZ <       1
                           IF      WRK-RECE09-EDDATA(IDZ:1)
                                                       NOT =   SPACE
                               MOVE    1   TO  IDZ 
                           ELSE  
                               MOVE    "Z" TO  WRK-RECE09-EDDATA(IDZ:1)
                           END-IF
                       END-PERFORM
               END-EVALUATE
           END-IF
      *
           IF      WRK-RECE09-PRTJYUN  =   "06"
               EVALUATE    WRK-TEISYUTUSAKI
                   WHEN    "2"
                       IF    ( IDW             =   1  )
                       AND   ( IDX             =   34 )
                           MOVE    "%%%%%%%%"  TO  WRK-RECE09-HKNJANUM
                       ELSE
                           MOVE    SPA-NAI-THKNJANUM (IDW IDX)
                                               TO  WRK-RECE09-HKNJANUM
                           MOVE    WRK-CHOUKI  TO  WRK-RECE09-CHOUKI
                           MOVE    WRK-ZAITAKU TO  WRK-RECE09-ZAITAKU
                           MOVE    WRK-KOUGAKU TO  WRK-RECE09-KOUGAKU
                           IF      WRK-KOUGAKU =   SPACE
                               MOVE  "A"       TO  WRK-RECE09-KOUGAKU
                           END-IF
                           MOVE    WRK-SRT-LAST
                                               TO  WRK-RECE09-SRT-LAST
                       END-IF
                   WHEN    "1"
                       IF      SPA-R01-NYUGAIKBN   =   "2"
                           IF      IDX                 =   1
                               MOVE    "9"     TO  WRK-RECE09-ZAITAKU
                           ELSE
                               MOVE    "1"     TO  WRK-RECE09-ZAITAKU
                           END-IF
                       ELSE
                           MOVE    "%"         TO  WRK-RECE09-ZAITAKU
                       END-IF
                       MOVE    WRK-SRT-LAST    TO  WRK-RECE09-SRT-LAST
                       MOVE    "%"             TO  WRK-RECE09-KOUGAKU
                                                   WRK-RECE09-CHOUKI
                       IF      SPA-R01-RECE07-HKNJANUM-SRT
                                               =   "8" 
                           MOVE    SPA-NAI-THKNJANUM-SYAHO (IDW)
                                               TO  WRK-RECE09-HKNJANUM
                       ELSE
                           MOVE    "%%%%%%%%"  TO  WRK-RECE09-HKNJANUM
                       END-IF
               END-EVALUATE 
           END-IF
      *
           IF      WRK-RECE09-PRTJYUN  =   "11"
               MOVE    SPA-NAI-2005-DRCD-PRTJYUNKBN
                                       TO  WRK-RECE09-DRCD-PRTJYUNKBN
               MOVE    "00000"         TO  WRK-RECE09-DRCD
               IF      SPA-NAI-SYUBETU (IDW IDX 1) 
                                   NOT =   "0000" AND "6666" AND
                                           "8888"    
                   MOVE    SPA-NAI-TDRCD (IDW IDX)
                                           TO  WRK-RECE09-DRCD
               END-IF
           END-IF
      *
           IF      WRK-RECE09-PRTJYUN  =   "12"
               MOVE    SPA-NAI-2005-BTUNUM-PRTJYUNKBN
                                       TO  WRK-RECE09-BTUNUM-PRTJYUNKBN
               MOVE    "00"            TO  WRK-RECE09-BTUNUM
               IF      SPA-NAI-SYUBETU (IDW IDX 1) 
                                   NOT =   "0000" AND "6666" AND
                                           "8888"    
                   MOVE    SPA-NAI-TBTUNUM (IDW IDX)
                                           TO  WRK-RECE09-BTUNUM
               END-IF
           END-IF
      *
           INSPECT WRK-RECE09-HKNJANUM  REPLACING   ALL " "
                                                    BY  "#"
      *
      *　　（ＯＲＣＲ０１０１）　
      *
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
           ADD     1               TO  WRK-RECE08-PRTJYUN                 
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept2.sh"    TO  SHELLTBL-NAME
      *     
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG2
           MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
           MOVE    SPA-R01-RECE07-SRYYM
                                   TO  WRK-SHELL-SRYYM    
           MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG3 
           MOVE    "0"             TO  SHELLTBL-ARG4
           MOVE    "0"             TO  SHELLTBL-ARG5
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG6
           MOVE    WRK-RECE09-REC  TO  SHELLTBL-ARG7 
           MOVE    WRK-RECE09-STDATA
                                   TO  SHELLTBL-ARG8
           MOVE    WRK-RECE09-EDDATA
                                   TO  SHELLTBL-ARG9
           MOVE    WRK-RECE08-PRTJYUN
                                   TO  SHELLTBL-ARG10
           MOVE    SPA-GMN-TPRTNMMEI (IDW IDX)
                                   TO  SHELLTBL-ARG11
           MOVE    SPA-GMN-TPRTNM-NMEI (IDW IDX)
                                   TO  SHELLTBL-ARG12
           MOVE    SPA-GMN-PRTKBN  TO  SHELLTBL-ARG24
           IF      WRK-RECE09-PRTJYUN  =   "13"
               MOVE    SPA-NAI-2005-JIHI-PRTKBN
                                       TO  SHELLTBL-ARG28
           ELSE
               MOVE    SPA-GMN-PREFKBN TO  SHELLTBL-ARG28
           END-IF
           MOVE    WRK-SYUKA       TO  SHELLTBL-ARG29
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG30
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       45011-RECEPRT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力順出力処理（個別作成時）
      *****************************************************************
       4502-RECEPRT-INPUT-HENSYU-SEC       SECTION.
      *
      *    印刷用パラメタ
           INITIALIZE                  WRK-RECE09-REC
                                       WRK-RECE09-STDATA
                                       WRK-RECE09-EDDATA
      *
           MOVE    SPA-R01-RECE07-PRTID
                                       TO  WRK-RECE09-PRTID
           IF      SPA-R01-RECE07-SYOKBN   =   "1"
               MOVE    SPA-R01-SRYYM       TO  WRK-RECE09-SRYYM
           ELSE
               MOVE    ZERO                TO  WRK-RECE09-SRYYM
           END-IF                                    
           MOVE    SPA-R01-RECE07-CREYMD   TO  WRK-RECE09-CREYMD
           MOVE    SPA-R01-RECE07-CREHMS   TO  WRK-RECE09-CREHMS
           MOVE    SPA-R01-RECE07-NYUGAIKBN
                                       TO  WRK-RECE09-NYUGAIKBN
      *    初期値をセット
           MOVE    "0000"              TO  WRK-RECE09-RECESYUBETU
           MOVE    "0"                 TO  WRK-RECE09-TEISYUTUSAKI
           MOVE    "0"                 TO  WRK-RECE09-PREFKBN
           MOVE    SPA-GMN-PRTJYUN     TO  WRK-RECE09-PRTJYUN
           MOVE    "0"             TO  WRK-RECE09-STDATA (1:1)
           MOVE    "Z"             TO  WRK-RECE09-EDDATA (1:1)
      *
      *　　（ＯＲＣＲ０１０１）　
      *
           MOVE    WRK-CONS-FILE-ID    TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    SPACE               TO  RECEERR
           STRING  MCP-TEMPDIR         DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE
                   ERRFLPARA-BASENAME  DELIMITED   BY  SPACE
                                       INTO        RECEERR
           END-STRING
      *
           ADD     1               TO  WRK-RECE08-PRTJYUN                 
      *
           MOVE    SPACE           TO  SHELLTBL
           MOVE    "recept2.sh"    TO  SHELLTBL-NAME
      *     
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG1
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG2
           MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
           MOVE    SPA-R01-RECE07-SRYYM
                                   TO  WRK-SHELL-SRYYM    
           MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
           MOVE    WRK-SHELL-ARG1  TO  SHELLTBL-ARG3 
           MOVE    "0"             TO  SHELLTBL-ARG4
           MOVE    "0"             TO  SHELLTBL-ARG5
           MOVE    ERRFLPARA-BASENAME
                                   TO  SHELLTBL-ARG6
           MOVE    WRK-RECE09-REC  TO  SHELLTBL-ARG7 
           MOVE    WRK-RECE09-STDATA
                                   TO  SHELLTBL-ARG8
           MOVE    WRK-RECE09-EDDATA
                                   TO  SHELLTBL-ARG9
           MOVE    WRK-RECE08-PRTJYUN  TO  SHELLTBL-ARG10
           EVALUATE   SPA-R01-RECE07-NYUGAIKBN
               WHEN   "1"
                   MOVE    SPA-NAI-2005-SYAPRTNM-N
                                   TO  SHELLTBL-ARG11
                   MOVE    SPA-NAI-2005-SYAPRTNM-NEXT-N
                                   TO  SHELLTBL-ARG12
                   STRING  SPA-NAI-2005-KKHPRTNM-N DELIMITED  BY  SPACE
                           "#/"                    DELIMITED  BY  SIZE
                           SPA-NAI-2005-KOUIKIPRTNM-N
                                                   DELIMITED  BY  SPACE
                                           INTO    SHELLTBL-ARG25
                   END-STRING
                   STRING  SPA-NAI-2005-KKHPRTNM-NEXT-N
                                                   DELIMITED  BY  SPACE
                           "#/"                    DELIMITED  BY  SIZE
                           SPA-NAI-2005-KOUIKIPRTNM-NEXT-N
                                                   DELIMITED  BY  SPACE
                                           INTO    SHELLTBL-ARG26
                   END-STRING
               WHEN   "2"
                   MOVE    SPA-NAI-2005-SYAPRTNM
                                   TO  SHELLTBL-ARG11
                   MOVE    SPA-NAI-2005-SYAPRTNM-NEXT
                                   TO  SHELLTBL-ARG12
                   STRING  SPA-NAI-2005-KKHPRTNM   DELIMITED  BY  SPACE
                           "#/"                    DELIMITED  BY  SIZE
                           SPA-NAI-2005-KOUIKIPRTNM
                                                   DELIMITED  BY  SPACE
                                           INTO    SHELLTBL-ARG25
                   END-STRING
                   STRING  SPA-NAI-2005-KKHPRTNM-NEXT
                                                   DELIMITED  BY  SPACE
                           "#/"                    DELIMITED  BY  SIZE
                           SPA-NAI-2005-KOUIKIPRTNM-NEXT
                                                   DELIMITED  BY  SPACE
                                           INTO    SHELLTBL-ARG26
                   END-STRING
           END-EVALUATE
           MOVE    SPA-GMN-PRTKBN  TO  SHELLTBL-ARG24
      *****MOVE    SPA-GMN-PREFKBN TO  SHELLTBL-ARG28
           MOVE    SPA-NAI-2005-JIHI-PRTKBN
                                   TO  SHELLTBL-ARG28
           MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG30
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "monthly"       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       4502-RECEPRT-INPUT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴出力処理（国保のみ）
      *****************************************************************
       4503-RECEPRT-INPUT-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-KOUGAKU
           MOVE    "0"             TO  WRK-SRT-LAST
      *     
      *    県内・県外の括りで出力
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       2
               EVALUATE    IDX1
                   WHEN    1   
                       MOVE    "1"                 TO  WRK-PREFKBN
                   WHEN    2   
                       MOVE    "2"                 TO  WRK-PREFKBN
               END-EVALUATE
      *    
      *        長期の設定が県内・県外の先頭のとき
               IF      SPA-NAI-2008-CHOUKI     =   "1"
                   MOVE    "1"                 TO  WRK-CHOUKI
                   MOVE    "%"                 TO  WRK-ZAITAKU
      *
                   PERFORM 45031-RECEPRT-HENSYU-SEC
               END-IF
      *
      *        在総診の設定が県内・県外の先頭のとき（入院外のみ）
               IF    ( SPA-NAI-2008-ZAITAKU    =   "1" )
               AND   ( SPA-R01-RECE07-NYUGAIKBN
                                               =   "2" )
                   MOVE    "1"                 TO  WRK-ZAITAKU
                   EVALUATE    SPA-NAI-2008-CHOUKI
                       WHEN    "1"
                       WHEN    "2" 
                           MOVE    "9"             TO  WRK-CHOUKI
                       WHEN    SPACE 
                           MOVE    "%"             TO  WRK-CHOUKI
                   END-EVALUATE
      *
                   PERFORM 45031-RECEPRT-HENSYU-SEC
               END-IF
      *
               EVALUATE    SPA-NAI-2008-CHOUKI
                   WHEN    "1"
                       MOVE    "9"                 TO  WRK-CHOUKI
                   WHEN    "2"
                   WHEN    SPACE
                       MOVE    "%"                 TO  WRK-CHOUKI
               END-EVALUATE
               EVALUATE    SPA-NAI-2008-ZAITAKU
                   WHEN    "1"
                       IF      SPA-NAI-2008-CHOUKI =   "2"
                           MOVE    "%"                 TO  WRK-ZAITAKU
                       ELSE
                           MOVE    "9"                 TO  WRK-ZAITAKU
                       END-IF
                   WHEN    "2"
                   WHEN    SPACE
                       MOVE    "%"                 TO  WRK-ZAITAKU
               END-EVALUATE
     
               PERFORM 45031-RECEPRT-HENSYU-SEC
           END-PERFORM
           .
       4503-RECEPRT-INPUT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴詳細設定出力処理（国保のみ）
      *****************************************************************
       4503-RECEPRT-INPUT-SYOSAI-SEC       SECTION.
      *
      *    県内・県外の括りで出力
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       2
               EVALUATE    IDX1
                   WHEN    1   
                       MOVE    "1"                 TO  WRK-PREFKBN
                   WHEN    2   
                       MOVE    "2"                 TO  WRK-PREFKBN
               END-EVALUATE
      *
               MOVE    SPA-R01-RECE07-SRT-LAST (IDX1)
                                   TO  WRK-SRT-LAST
      *    
      *        高額の設定が県内・県外の先頭のとき
               IF      SPA-R01-RECE07-KOUGAKU-TEN (IDX1)
                                           >   ZERO
                   MOVE    "1"                 TO  WRK-KOUGAKU
                   MOVE    "%"                 TO  WRK-ZAITAKU
                                                   WRK-CHOUKI
      *
                   PERFORM 45031-RECEPRT-HENSYU-SEC
               END-IF
      *    
      *        長期の設定が県内・県外の先頭のとき
               IF      SPA-R01-RECE07-CHOUKI-KBN (IDX1)
                                           =   "1"
                   MOVE    "1"                 TO  WRK-CHOUKI
                   MOVE    "%"                 TO  WRK-ZAITAKU
                   IF      SPA-R01-RECE07-KOUGAKU-TEN (IDX1)
                                           >   ZERO
                       MOVE    "9"             TO  WRK-KOUGAKU
                   ELSE
                       MOVE    "%"             TO  WRK-KOUGAKU
                   END-IF
      *
                   PERFORM 45031-RECEPRT-HENSYU-SEC
               END-IF
      *
      *        在総診の設定が県内・県外の先頭のとき（入院外のみ）
               IF    ( SPA-R01-RECE07-ZAITAKU-KBN (IDX1)
                                           =   "1" )
               AND   ( SPA-R01-RECE07-NYUGAIKBN
                                           =   "2" )
                   MOVE    "1"                 TO  WRK-ZAITAKU
                   EVALUATE    SPA-R01-RECE07-CHOUKI-KBN (IDX1)
                       WHEN    "1"
                       WHEN    "2" 
                           MOVE    "9"             TO  WRK-CHOUKI
                       WHEN    SPACE 
                           MOVE    "%"             TO  WRK-CHOUKI
                   END-EVALUATE
                   IF      SPA-R01-RECE07-KOUGAKU-TEN (IDX1)
                                           >   ZERO
                       MOVE    "9"             TO  WRK-KOUGAKU
                   ELSE
                       MOVE    "%"             TO  WRK-KOUGAKU
                   END-IF
      *
                   PERFORM 45031-RECEPRT-HENSYU-SEC
               END-IF
      *
               IF      SPA-R01-RECE07-KOUGAKU-TEN (IDX1)
                                           >   ZERO
                   MOVE    "9"             TO  WRK-KOUGAKU
               ELSE
                   MOVE    "%"             TO  WRK-KOUGAKU
               END-IF
               EVALUATE    SPA-R01-RECE07-CHOUKI-KBN (IDX1)
                   WHEN    "1"
                       MOVE    "9"                 TO  WRK-CHOUKI
                   WHEN    "2"
                   WHEN    SPACE
                       MOVE    "%"                 TO  WRK-CHOUKI
               END-EVALUATE
               EVALUATE    SPA-R01-RECE07-ZAITAKU-KBN (IDX1)
                   WHEN    "1"
                       IF      SPA-R01-RECE07-CHOUKI-KBN (IDX1)
                                                   =   "2"
                           MOVE    "%"             TO  WRK-ZAITAKU
                       ELSE
                           MOVE    "9"             TO  WRK-ZAITAKU
                       END-IF
                   WHEN    "2"
                   WHEN    SPACE
                       MOVE    "%"                 TO  WRK-ZAITAKU
               END-EVALUATE
      *
               PERFORM 45031-RECEPRT-HENSYU-SEC
           END-PERFORM
           .
       4503-RECEPRT-INPUT-SYOSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴編集処理
      *****************************************************************
       45031-RECEPRT-HENSYU-SEC             SECTION.
      *
           PERFORM VARYING IDW FROM    IDWW    BY  1
                   UNTIL   IDW >       SPA-GMN-PAGE-MAX1
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-LINE-MAX
                       OR      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                       OR      SPA-ERRCD               NOT =   SPACE
                   IF    ( SPA-GMN-TCHK-VALUE  (IDW IDX)   =   "T"   )
                   AND   ( SPA-GMN-TPAGE       (IDW IDX)   >   ZERO  )
                   AND   ( WRK-PREFKBN                     =
                                         SPA-NAI-TPREFKBN-SRT (IDW IDX))
      *
      *                診療科指定のとき
                       IF    ( SPA-GMN-SYUKA       =   "00" )
                       AND   ( WRK-SYUKA       NOT =   "%%" )
                       AND   ( SPA-NAI-TPAGE-SYUKA (IDW IDX IDXX)
                                                  =   0    )
                           CONTINUE
                       ELSE    
                           MOVE    1               TO  IDY
                           IF      WRK-CHOUKI      =   "1"
                               IF      SPA-NAI-TPAGE-CHOUKI (IDW IDX)
                                                          >   ZERO
      *                          診療科指定のとき
                                 IF    ( SPA-GMN-SYUKA       =   "00" )
                                 AND   ( WRK-SYUKA       NOT =   "%%" )
                                 AND   ( SPA-NAI-TPAGE-CHOUKI-SYUKA
                                                     (IDW IDX IDXX)
                                                             =   0    )
                                     CONTINUE
                                 ELSE    
                                     PERFORM 45011-RECEPRT-HENSYU-SEC
                                 END-IF
                               END-IF
                           ELSE
                               IF      WRK-ZAITAKU     =   "1"
                                 IF      SPA-NAI-TPAGE-ZAITAKU
                                                           (IDW IDX)
                                                              >   ZERO
      *                            診療科指定のとき
                                   IF    ( SPA-GMN-SYUKA       =   "00")
                                   AND   ( WRK-SYUKA       NOT =   "%%")
                                   AND   ( SPA-NAI-TPAGE-ZAITAKU-SYUKA
                                                       (IDW IDX IDXX)
                                                               =   0   )
                                       CONTINUE
                                   ELSE    
                                       PERFORM 45011-RECEPRT-HENSYU-SEC
                                   END-IF
                                 END-IF  
                               ELSE
                                   PERFORM 45011-RECEPRT-HENSYU-SEC
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       45031-RECEPRT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷順出力処理（全体）
      *****************************************************************
       4504-ALL-PRTJYUN-PRINT-SEC    SECTION.
      *
           MOVE    1                TO  IDW 
           MOVE    "1"              TO  WRK-TEISYUTUSAKI
           MOVE    "%"              TO  WRK-PREFKBN
           PERFORM 4501-PRTJYUN-PRINT-SEC
           .
       4504-ALL-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医師別順出力処理
      *****************************************************************
       4505-DRCD-PRTJYUN-PRINT-SEC    SECTION.
      *
           MOVE    "1"              TO  WRK-TEISYUTUSAKI
           MOVE    "%"              TO  WRK-PREFKBN
      *
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       SPA-GMN-PAGE-MAX1
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       WRK-CONS-LINE-MAX
                       OR      SPA-NAI-PRTJYUNI(IDW IDX)   =   SPACE
                       OR      SPA-ERRCD               NOT =   SPACE
                   IF    ( IDW                 =   1   )
                   AND   ( IDX                 >=  33  )
                       CONTINUE
                   ELSE 
                       IF    ( SPA-GMN-TCHK-VALUE  (IDW IDX)   =   "T" )
                       AND   ( SPA-GMN-TPAGE       (IDW IDX)   >   ZERO)
                           MOVE    1               TO  IDY
                           PERFORM 45011-RECEPRT-HENSYU-SEC
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       4505-DRCD-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟別順出力処理
      *****************************************************************
       4506-BTUNUM-PRTJYUN-PRINT-SEC    SECTION.
      *
           EVALUATE    SPA-GMN-SYUKA
               WHEN    SPACE
                   MOVE    "%%"            TO  WRK-SYUKA
                   PERFORM 45011-RECEPRT-HENSYU-SEC
               WHEN    "00"
                   PERFORM VARYING IDXX    FROM    2   BY  1
                           UNTIL   IDXX    >   SPA-GMN-SYUKA-MAX
                           OR      SPA-ERRCD   NOT =   SPACE
                       IF      SPA-NAI-TPAGE-SYUKA (IDW IDX IDXX)
                                                   =   0
                           CONTINUE
                       ELSE
                           MOVE    SPA-GMN-SYUKAL (IDXX)   TO  WRK-SYUKA
                           PERFORM 45011-RECEPRT-HENSYU-SEC
                       END-IF     
                   END-PERFORM
               WHEN    OTHER
                   MOVE    SPA-GMN-SYUKA   TO  WRK-SYUKA
                   PERFORM VARYING IDXX    FROM    2   BY  1
                           UNTIL   IDXX    >       SPA-GMN-SYUKA-MAX
                       IF      WRK-SYUKA   =   SPA-GMN-SYUKAL (IDXX)
                           IF      SPA-NAI-TPAGE-SYUKA (IDW IDX IDXX)
                                                       =   0
                               CONTINUE
                           ELSE
                               PERFORM 45011-RECEPRT-HENSYU-SEC
                               MOVE    SPA-GMN-SYUKA-MAX    TO  IDXX
                           END-IF
                       END-IF
                   END-PERFORM        
           END-EVALUATE
           .
       4506-BTUNUM-PRTJYUN-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE       =   1
               MOVE    "0009"             TO  SPA-ERRCD
               GO  TO  460-MAEGMN-EXT
           END-IF
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    -   1
      *
           MOVE    199                  TO  SPA-GMN-CUR
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
          IF      SPA-GMN-PAGE       =   SPA-GMN-PAGE-MAX
               MOVE    "0010"             TO  SPA-ERRCD
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    +   1       
      *
           MOVE    199                  TO  SPA-GMN-CUR
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    編綴情報表示
      *****************************************************************
       490-HENTETU-SEC                    SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-HENTETU-EXT
           END-IF
      *
           MOVE    SPACE               TO  R05
      *
           INITIALIZE                      R05
           EVALUATE    SPA-NAI-2008-PREFKBN-PRTJYUN
               WHEN    "1"
                   MOVE    "県内・県外"    TO  R05-PR-PRTJYUN
               WHEN    "2"
                   MOVE    "県外・県内"    TO  R05-PR-PRTJYUN
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-HKNJANUM-PRTJYUN
               WHEN    "1"
                   MOVE
                       "市町村国保・国保組合・県内扱いの県外国保組合"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "2"
                   MOVE
                       "市町村国保・県内扱いの県外国保組合・国保組合"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "3"
                   MOVE
                       "国保組合・市町村国保・県内扱いの県外国保組合"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "4"
                   MOVE
                       "国保組合・県内扱いの県外国保組合・市町村国保"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "5"
                   MOVE
                       "県内扱いの県外国保組合・市町村国保・国保組合"
                                           TO  R05-HKN-PRTJYUN
               WHEN    "6"
                   MOVE
                       "県内扱いの県外国保組合・国保組合・市町村国保"
                                           TO  R05-HKN-PRTJYUN
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-SYUBETU-PRTJYUN
               WHEN    "1"
                   MOVE    "国保一般・老人保健・退職国保"
                                           TO  R05-REC-PRTJYUN
               WHEN    "2"
                   MOVE    "国保一般・退職国保・老人保健"
                                           TO  R05-REC-PRTJYUN
               WHEN    "3"
                   MOVE    "老人保健・国保一般・退職国保"
                                           TO  R05-REC-PRTJYUN
               WHEN    "4"
                   MOVE    "老人保健・退職国保・国保一般"
                                           TO  R05-REC-PRTJYUN
               WHEN    "5"
                   MOVE    "退職国保・国保一般・老人保健"
                                           TO  R05-REC-PRTJYUN
               WHEN    "6"
                   MOVE    "退職国保・老人保健・国保一般" 
                                           TO  R05-REC-PRTJYUN
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-ZAITAKU
               WHEN    "1"
                    MOVE    "県内・県外の先頭に印刷" 
                                           TO  R05-ZAITAKU
               WHEN    "2"
                   MOVE    "保険者の先頭に印刷"
                                           TO  R05-ZAITAKU
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-CHOUKI
               WHEN    "1"
                    MOVE    "県内・県外の先頭に印刷" 
                                           TO  R05-CHOUKI
               WHEN    "2"
                   MOVE    "保険者の先頭に印刷"
                                           TO  R05-CHOUKI
           END-EVALUATE
      *
           EVALUATE    SPA-NAI-2008-HKNJANUM-PRTKBN
               WHEN    "1"
                   MOVE    "市町村国保の先頭に印刷" 
                                           TO  R05-HKNJ-PRTKBN
               WHEN    "2"
                   MOVE    "市町村国保の最後に印刷"
                                           TO  R05-HKNJ-PRTKBN
           END-EVALUATE
      *
           MOVE    SPA-NAI-2008-HKNJANUM   TO  R05-HKNJANUM
           MOVE    SPA-NAI-2008-HKNJANAME  TO  R05-HKNJANAME
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "R05"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "R05"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       490-HENTETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ一括設定処理
      *****************************************************************
       490-PRINTER-SETTEI-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-PRINTER-SETTEI-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-GMN-TPRTNM-G   (SPA-GMN-PAGE 1)
                                       TO  SPA-R01-PRTNM-G 
           MOVE    SPA-GMN-TPRTNM-N-G (SPA-GMN-PAGE 1)
                                       TO  SPA-R01-PRTNM-N-G
           MOVE    "R02"               TO  SPA-R01-MOTOPG
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-R02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGR06"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
           MOVE    SPA-FREE        TO  SPA-R02
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "R06"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "R06"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *                                      
           .
       490-PRINTER-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-KAKUNIN-EXT
           END-IF
      *                                      
      *    画面移行用のパラメタ
           PERFORM 4901-PARA-HENSYU-SEC
      *
           MOVE    SPACE               TO  R99
           INITIALIZE                      R99
           MOVE    ZERO                TO  R99-COUNT
                                           R99-PROGRESSBAR-VALUE
           MOVE    "black"             TO  R99-MSG-STYLE
      *
           IF      SPA-RID1-FLG    =   "PV"
               MOVE    "F12:プレビュー"    TO  R99-B12-LABEL
      *****ELSE
      *****    MOVE    "F12:印    刷"      TO  R99-B12-LABEL
           END-IF
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           IF      SJOBKANRI-RETURN
                                   =   ZERO
               MOVE    1               TO  IDX                     
               MOVE    JOB-JOBID       TO  R99-RENNUM   (IDX)
               MOVE    JOB-SHELLMSG    TO  R99-SHORINM  (IDX)
               MOVE    JOB-STARTTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-STARTTIME(IDX)
               MOVE    JOB-ENDTIME(1:2)
                                       TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                       TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                       TO  WRK-SS
               MOVE    WRK-HENTIME     TO  R99-ENDTIME  (IDX)
               MOVE    JOB-YOBI        TO  R99-ERRMSG   (IDX)
               COMPUTE R99-PROGRESSBAR-VALUE  =
                       JOB-STEPCNT  /   SPA-R01-JOBCNT *   100
               IF      JOB-ENDYMD      =   SPACE
                   MOVE    "処理中です"    TO  R99-MSG
               ELSE
                   IF      JOB-ERRCD       =   SPACE
                       CONTINUE
                   ELSE
                       STRING  JOB-YOBI    DELIMITED  BY  SPACE
                               "【"        DELIMITED  BY  SIZE
                              JOB-SHELLMSG DELIMITED  BY  SPACE
                              "】"         DELIMITED  BY  SIZE      
                                           INTO  R99-MSG
                       END-STRING
                   END-IF                          
               END-IF    
               MOVE    IDX                     TO  R99-COUNT    
           END-IF
      *
           MOVE    WIDGET-INSENSITIVE   TO  R99-B12-STATE
           MOVE    WIDGET-INSENSITIVE   TO  R99-B21-STATE
           IF      R99-MSG              =   SPACE
               MOVE    "B01"                TO  MCP-WIDGET
               IF      R99-COUNT            >   ZERO    
                   IF      JOB-UPDCNT           =   ZERO
                       MOVE    "該当のレセプトはありませんでした"
                                                TO  R99-MSG
                   ELSE
                       IF      JOB-STOPFLG      NOT =   ZERO
                           MOVE    "印刷処理が中止されました"
                                                        TO  R99-MSG
                       ELSE
                           MOVE    JOB-UPDCNT           TO  WRK-PAGE
                           STRING
                               "処理は正常に終了しました　処理枚数は"
                                                DELIMITED  BY  SIZE
                               WRK-PAGE         DELIMITED  BY  SIZE
                               "枚です"         DELIMITED  BY  SIZE
                                                INTO  R99-MSG
                           END-STRING
                       END-IF
                   END-IF
                   MOVE   100               TO  R99-PROGRESSBAR-VALUE
                   IF    ( SPA-RID1-FLG    =   "PV" )
                   AND   ( JOB-UPDCNT      >   ZERO )
                   AND   ( JOB-STOPFLG     =   ZERO )
                       MOVE    WIDGET-NORMAL    TO  R99-B12-STATE
                       MOVE    "B12"            TO  MCP-WIDGET
                   END-IF
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  R99-MSG
               END-IF                                 
           ELSE                                        
               IF      JOB-STOPFLG          =   ZERO
                   MOVE    WIDGET-NORMAL        TO  R99-B21-STATE
               END-IF
               MOVE    "B11"                TO  MCP-WIDGET
           END-IF
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "R99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "R99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面移行用のパラメタ変更処理
      *****************************************************************
       4901-PARA-HENSYU-SEC            SECTION.       
      *    
      *****MOVE    SPACE               TO  SPA-R01KYOUTU
      *****INITIALIZE    SPA-R01KYOUTU
      *
           MOVE    "R02"               TO  SPA-R01-GMNKBN
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           .
       4901-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-JYOUHOU-DEL-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R02"               TO  SPA-MOTOPG
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
           IF      SPA-RIDCD       NOT =   SPACE
               PERFORM 520-RIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-RIDCD3      NOT =   SPACE
               PERFORM 530-RID3SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R02    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE     R02
      *
           MOVE    SPA-GMN-SYOKBN         TO  R02-SYOKBN-VALUE
           EVALUATE    SPA-GMN-SYOKBN
               WHEN    "T"
                   MOVE    "全件印刷"         TO  R02-SYOKBN-LABEL
               WHEN    "F"
                   MOVE    "指定印刷"         TO  R02-SYOKBN-LABEL
           END-EVALUATE      
      *
           MOVE    SPA-GMN-FILEKBN     TO  R02-FILEKBN
      *
           MOVE    SPA-GMN-MSG         TO  R02-MSG
           MOVE    "red"               TO  R02-MSG-STYLE
      *
           MOVE    SPA-GMN-CREYMD      TO  R02-CREYMD  
      *
           MOVE    SPA-GMN-PAGEH       TO  R02-PAGE  
      *
           MOVE    SPA-GMN-KENSUH      TO  R02-KENSU  
      *
           MOVE    SPA-GMN-PRTJYUN-G   TO  R02-PRTJYUN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-PRTJYUN-MAX
                   OR          SPA-GMN-PRTJYUNL    (IDX)   =   SPACE 
               MOVE    SPA-GMN-PRTJYUNLST(IDX) TO  R02-PRTJYUNLST (IDX)
               MOVE    IDX                     TO  R02-PRTJYUN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-PRTJYUN1-G   TO  R02-PRTJYUN1
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-PRTJYUN1-MAX
                   OR          SPA-GMN-PRTJYUN1L (IDX) =   SPACE 
               MOVE    SPA-GMN-PRTJYUN1LST(IDX)
                                           TO  R02-PRTJYUN1LST (IDX)
               MOVE    IDX                 TO  R02-PRTJYUN1-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-PRTJYUN2-G   TO  R02-PRTJYUN2
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-PRTJYUN2-MAX
                   OR          SPA-GMN-PRTJYUN2L (IDX) =   SPACE 
               MOVE    SPA-GMN-PRTJYUN2LST(IDX)
                                           TO  R02-PRTJYUN2LST (IDX)
               MOVE    IDX                 TO  R02-PRTJYUN2-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-SYUKA-G     TO  R02-SYUKA
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-SYUKA-MAX
               IF    ( SPA-GMN-PRTJYUN         =   "11" )
               AND   ( SPA-GMN-SYUKAL(IDX)     =   "XX" )
                   CONTINUE
               ELSE
                  MOVE    SPA-GMN-SYUKALST(IDX)
                                              TO  R02-SYUKALST (IDX)
                  MOVE    IDX                 TO  R02-SYUKA-COUNT
               END-IF
           END-PERFORM
      *
           IF      SPA-R01-FILEKBN     =   "1"
               MOVE    WIDGET-NORMAL       TO  R02-PRTJYUN1-STATE
                                               R02-PRTJYUN2-STATE
               MOVE    "  社保"            TO  R02-LABEL1
               MOVE    "  国保"            TO  R02-LABEL2
               MOVE    "  広域"            TO  R02-LABEL3
               IF      SPA-GMN-PRTJYUN     =   "07"    OR  "08"   OR
                                               "11"    OR  "12"   OR
                                               "13"
                   MOVE    WIDGET-INSENSITIVE  TO  R02-PRTJYUN1-STATE
                                                   R02-PRTJYUN2-STATE
               END-IF
           ELSE 
               MOVE    WIDGET-INSENSITIVE  TO  R02-PRTJYUN1-STATE
                                               R02-PRTJYUN2-STATE
               MOVE    "出力順"            TO  R02-LABEL1
               MOVE    SPACE               TO  R02-LABEL2
                                               R02-LABEL3
           END-IF
           IF      SPA-R01-RECE07-SRYYM    <   WRK-CONS-SRYYM-200804
               MOVE    WIDGET-INSENSITIVE  TO  R02-PRTJYUN2-STATE
               MOVE    SPACE               TO  R02-PRTJYUN2
           END-IF
      *
           MOVE    SPA-GMN-PREFKBN-G   TO  R02-PREFKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   4
                   OR          SPA-GMN-PREFKBNL    (IDX)   =   SPACE 
               MOVE    SPA-GMN-PREFKBNLST(IDX) TO  R02-PREFKBNLST (IDX)
               MOVE    IDX                     TO  R02-PREFKBN-COUNT
           END-PERFORM
           IF    ( SPA-GMN-PRTJYUN1    =   "05"  OR  "06"  OR  "07"
                                                 OR  "08"  OR  "11"
                                                 OR  "12"  OR  "13" )
           OR    ( SPA-GMN-PRTJYUN2    =   "05"  OR  "06"  OR  "07"
                                                 OR  "08"  OR  "11"
                                                 OR  "12"  OR  "13" )
               MOVE    WIDGET-INSENSITIVE  TO  R02-PREFKBN-STATE
           ELSE 
               MOVE    WIDGET-NORMAL       TO  R02-PREFKBN-STATE
           END-IF
      *
           MOVE    SPA-GMN-PRTKBN-G   TO  R02-PRTKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   3
                   OR          SPA-GMN-PRTKBNL    (IDX)   =   SPACE 
               MOVE    SPA-GMN-PRTKBNLST(IDX) TO  R02-PRTKBNLST (IDX)
               MOVE    IDX                     TO  R02-PRTKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-PAGE        TO  IDW
      *
           MOVE    SPA-GMN-THKNJANAME-SYAHO (IDW)
                                       TO  R02-HKNJANAME
           MOVE    "blue"              to  R02-HKNJANAME-STYLE
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       WRK-CONS-LINE-MAX
               IF      SPA-NAI-PRTJYUNI (IDW IDX)  =   SPACE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  R02-TCHK-STATE    (IDX)
                                               R02-TSTDATA-STATE (IDX)
                                               R02-TEDDATA-STATE (IDX)
                                               R02-TPRTNM-STATE  (IDX)
                                               R02-TPRTNM-N-STATE(IDX)
               ELSE 
                   MOVE    WIDGET-NORMAL   TO  R02-TCHK-STATE    (IDX)
                                               R02-TSTDATA-STATE (IDX)
                                               R02-TEDDATA-STATE (IDX)
                   IF       SPA-GMN-PRTJYUN    =   "05"
                       MOVE    WIDGET-INSENSITIVE
                                           TO  R02-TPRTNM-STATE  (IDX)
                                               R02-TPRTNM-N-STATE(IDX)
                   ELSE
                       MOVE    WIDGET-NORMAL
                                           TO  R02-TPRTNM-STATE  (IDX)
                                               R02-TPRTNM-N-STATE(IDX)
                   END-IF
               END-IF    
               MOVE    SPA-GMN-TSTDATA     (IDW IDX)
                                           TO  R02-TSTDATA    (IDX)
               MOVE    SPA-GMN-TEDDATA     (IDW IDX) 
                                           TO  R02-TEDDATA    (IDX)
               MOVE    SPA-GMN-TKENSU      (IDW IDX)
                                           TO  R02-TKENSU     (IDX)
               MOVE    SPA-GMN-TPAGE       (IDW IDX)
                                           TO  R02-TPAGE      (IDX)
               MOVE    SPA-GMN-TCHK-VALUE  (IDW IDX)
                                           TO  R02-TCHK-VALUE (IDX) 
               MOVE    SPA-GMN-TSYURUINAME (IDW IDX)
                                           TO  R02-TCHK-LABEL (IDX)
               MOVE    SPA-GMN-TPRTNM-G    (IDW IDX)
                                           TO  R02-TPRTNM     (IDX)
               MOVE    SPA-GMN-TPRTNM-N-G  (IDW IDX)
                                           TO  R02-TPRTNM-N   (IDX)
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       SPA-GMN-PRTNM-MAX
                   MOVE    SPA-GMN-PRTNMLST (IDY)
                                           TO  R02-TPRTNMLST   (IDX IDY)
                                               R02-TPRTNM-NLST (IDX IDY)
               END-PERFORM
               MOVE    SPA-GMN-PRTNM-MAX   TO  R02-TPRTNM-COUNT   (IDX)
                                               R02-TPRTNM-N-COUNT (IDX) 
               IF      SPA-GMN-TPRTFLG     (IDW IDX)
                                           =   "1"
                   MOVE    "済"            TO  R02-TPRTFLG        (IDX)
                   MOVE    "blue"          TO  R02-TPRTFLG-STYLE  (IDX)
               ELSE
                   MOVE    "black"         TO  R02-TPRTFLG-STYLE  (IDX)
               END-IF
               IF    ( SPA-GMN-SYOKBN      =   "F"   )
               AND   ( SPA-GMN-TCHK-VALUE  (IDW IDX)
                                           =   "T"   )
                   MOVE    "blue-bar"      TO  R02-TCHK-STYLE   (IDX)
                   MOVE    "blue-rev"      TO  R02-TPAGE-STYLE  (IDX)
                                               R02-TKENSU-STYLE (IDX)
               ELSE
                   IF    ( SPA-NAI-THKNJANUM-ERR  (IDW IDX)
                                               =   "1"   )
                       MOVE    "red-bar"   TO  R02-TCHK-STYLE   (IDX)
                       MOVE    "black"     TO  R02-TPAGE-STYLE  (IDX)
                                               R02-TKENSU-STYLE (IDX)
                   ELSE
                       MOVE    "black"     TO  R02-TCHK-STYLE   (IDX)
                                               R02-TPAGE-STYLE  (IDX)
                                               R02-TKENSU-STYLE (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-PRTJYUN1    =   "06" )
           AND   ( SPA-GMN-PAGE-MAX    >   1    )
            IF    (SPA-NAI-2008-HYOUJI   =   1)
               MOVE    WIDGET-NORMAL       TO  R02-B13-STATE
            ELSE 
               MOVE    WIDGET-INSENSITIVE  TO  R02-B13-STATE
            END-IF
           ELSE 
               MOVE    WIDGET-INSENSITIVE  TO  R02-B13-STATE
           END-IF
      *
           IF    ( SPA-GMN-PRTJYUN1    =   "05" )
               MOVE    WIDGET-INSENSITIVE  TO  R02-B14-STATE
           ELSE 
               MOVE    WIDGET-NORMAL       TO  R02-B14-STATE
           END-IF
      *
           IF      SPA-GMN-SYUKA-MAX   >   ZERO
             IF      SPA-GMN-PRTJYUN     =   "11"
               MOVE    WIDGET-NORMAL       TO  R02-SYUKA-STATE
               MOVE    "医師別診療科"      TO  R02-SYUKA-MIDASI
             ELSE
               IF    ( SPA-R01-RECE07-SRYKA
                                           =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                           =   "00" )
               AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                           =   "00" )
                 MOVE    WIDGET-INSENSITIVE  TO  R02-SYUKA-STATE
               ELSE
                MOVE    "主科"               TO  R02-SYUKA-MIDASI
                 MOVE    WIDGET-NORMAL       TO  R02-SYUKA-STATE
      *          平成２０年４月以降の設定
                 IF      SPA-R01-RECE07-SRYYM  >=  WRK-CONS-SRYYM-200804
                  IF    ( SPA-R01-RECE07-SRYKA
                                          NOT =   "00" )
                  AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                          NOT =   "00" )
                  AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                          NOT =   "00" )
                    MOVE    WIDGET-NORMAL       TO  R02-SYUKA-STATE
                  ELSE
                    IF      SPA-GMN-PRTJYUN     =   "07"    OR  "08"
                                                            OR  "13"
                      MOVE    WIDGET-INSENSITIVE  TO  R02-SYUKA-STATE
                    ELSE
                      IF      SPA-GMN-PAGE-MAX    =   1
                          MOVE    WIDGET-NORMAL   TO  R02-SYUKA-STATE
                      ELSE
                       IF    ( SPA-R01-RECE07-SRYKA        =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                       NOT =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                       NOT =   "00" )
                           IF      SPA-GMN-PAGE        =   1
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  R02-SYUKA-STATE
                           ELSE
                               MOVE    WIDGET-NORMAL
                                                   TO  R02-SYUKA-STATE
                           END-IF
                       END-IF    
                       IF    ( SPA-R01-RECE07-SRYKA        =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                       NOT =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                           =   "00" )
                           IF      SPA-GMN-PAGE        =   1   OR
                                                       SPA-GMN-PAGE-MAX 
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  R02-SYUKA-STATE
                           ELSE
                               MOVE    WIDGET-NORMAL
                                                   TO  R02-SYUKA-STATE
                           END-IF
                       END-IF    
                       IF    ( SPA-R01-RECE07-SRYKA    NOT =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                           =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                           =   "00" )
                           IF      SPA-GMN-PAGE        =   1
                               MOVE    WIDGET-NORMAL
                                                   TO  R02-SYUKA-STATE
                           ELSE
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  R02-SYUKA-STATE
                           END-IF
                       END-IF
                       IF    ( SPA-R01-RECE07-SRYKA    NOT =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                       NOT =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                           =   "00" )
                           IF      SPA-GMN-PAGE    =   SPA-GMN-PAGE-MAX
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  R02-SYUKA-STATE
                           ELSE
                               MOVE    WIDGET-NORMAL
                                                   TO  R02-SYUKA-STATE
                           END-IF
                       END-IF
                       IF    ( SPA-R01-RECE07-SRYKA        =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                           =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                       NOT =   "00" )
                           IF      SPA-GMN-PAGE    =   SPA-GMN-PAGE-MAX
                               MOVE    WIDGET-NORMAL
                                                   TO  R02-SYUKA-STATE
                           ELSE
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  R02-SYUKA-STATE
                           END-IF
                       END-IF
                       IF    ( SPA-R01-RECE07-SRYKA    NOT =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                           =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOUIKI
                                                       NOT =   "00" )
                           IF      SPA-GMN-PAGE    =   1   OR
                                                       SPA-GMN-PAGE-MAX
                               MOVE    WIDGET-NORMAL
                                                   TO  R02-SYUKA-STATE
                           ELSE
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  R02-SYUKA-STATE
                           END-IF
                       END-IF
                      END-IF
                    END-IF
                  END-IF
                 ELSE
      *           平成２０年３月までの設定
                  IF    ( SPA-GMN-PRTJYUN     =   "07"    OR  "08" )
                  AND   ( SPA-R01-RECE07-SRYKA
                                              =   "00"             )
                  AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                              =   "00"             )
                      MOVE    WIDGET-INSENSITIVE  TO  R02-SYUKA-STATE
                  ELSE
                   IF      SPA-GMN-PAGE-MAX    =   1
                       MOVE    WIDGET-NORMAL       TO  R02-SYUKA-STATE
                   ELSE
                       IF    ( SPA-R01-RECE07-SRYKA    NOT =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                       NOT =   "00" )
                           MOVE    WIDGET-NORMAL   TO  R02-SYUKA-STATE
                       END-IF
                       IF    ( SPA-R01-RECE07-SRYKA        =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                       NOT =   "00" )
                           IF      SPA-GMN-PAGE        =   1
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  R02-SYUKA-STATE
                           ELSE
                               MOVE    WIDGET-NORMAL
                                                   TO  R02-SYUKA-STATE
                           END-IF
                       END-IF
                       IF    ( SPA-R01-RECE07-SRYKA    NOT =   "00" )
                       AND   ( SPA-R01-RECE07-SRYKA-KOKUHO
                                                           =   "00" )
                           IF      SPA-GMN-PAGE        =   1
                               MOVE    WIDGET-NORMAL
                                                   TO  R02-SYUKA-STATE
                           ELSE
                               MOVE    WIDGET-INSENSITIVE
                                                   TO  R02-SYUKA-STATE
                           END-IF
                       END-IF
                   END-IF
                  END-IF
                 END-IF    
               END-IF    
             END-IF    
           ELSE 
               MOVE    WIDGET-INSENSITIVE  TO  R02-SYUKA-STATE
           END-IF
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
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-CUR
             WHEN    1
                    MOVE  "SYOKBN"     TO  MCP-WIDGET
             WHEN    2
                    MOVE  "PREFKBN"    TO  MCP-WIDGET
             WHEN    3
                    MOVE  "PRTJYUN"    TO  MCP-WIDGET
             WHEN   200
                    MOVE  "PRTKBN"     TO  MCP-WIDGET
             WHEN    5
                    MOVE  "STDATA01"   TO  MCP-WIDGET
             WHEN    6
                    MOVE  "EDDATA01"   TO  MCP-WIDGET
             WHEN    4
                    MOVE  "CHK01"      TO  MCP-WIDGET
             WHEN    8
                    MOVE  "STDATA02"   TO  MCP-WIDGET
             WHEN    9
                    MOVE  "EDDATA02"   TO  MCP-WIDGET
             WHEN    7
                    MOVE  "CHK02"      TO  MCP-WIDGET
             WHEN    11
                    MOVE  "STDATA03"   TO  MCP-WIDGET
             WHEN    12
                    MOVE  "EDDATA03"   TO  MCP-WIDGET
             WHEN    10
                    MOVE  "CHK03"      TO  MCP-WIDGET
             WHEN    14
                    MOVE  "STDATA04"     TO  MCP-WIDGET
             WHEN    15
                    MOVE  "EDDATA04"     TO  MCP-WIDGET
             WHEN    13
                    MOVE  "CHK04"       TO  MCP-WIDGET
             WHEN    17
                    MOVE  "STDATA05"     TO  MCP-WIDGET
             WHEN    18
                    MOVE  "EDDATA05"     TO  MCP-WIDGET
             WHEN    16
                    MOVE  "CHK05"       TO  MCP-WIDGET
             WHEN    20
                    MOVE  "STDATA06"     TO  MCP-WIDGET
             WHEN    21
                    MOVE  "EDDATA06"     TO  MCP-WIDGET
             WHEN    19
                    MOVE  "CHK06"       TO  MCP-WIDGET
             WHEN    23
                    MOVE  "STDATA07"     TO  MCP-WIDGET
             WHEN    24
                    MOVE  "EDDATA07"     TO  MCP-WIDGET
             WHEN    22
                    MOVE  "CHK07"       TO  MCP-WIDGET
             WHEN    26
                    MOVE  "STDATA08"     TO  MCP-WIDGET
             WHEN    27
                    MOVE  "EDDATA08"     TO  MCP-WIDGET
             WHEN   25
                    MOVE  "CHK08"       TO  MCP-WIDGET
             WHEN    29
                    MOVE  "STDATA09"     TO  MCP-WIDGET
             WHEN    30
                    MOVE  "EDDATA09"     TO  MCP-WIDGET
             WHEN    28
                    MOVE  "CHK09"       TO  MCP-WIDGET
             WHEN    32
                    MOVE  "STDATA10"    TO  MCP-WIDGET
             WHEN    33
                    MOVE  "EDDATA10"    TO  MCP-WIDGET
             WHEN    31
                    MOVE  "CHK10"      TO  MCP-WIDGET
             WHEN    35
                    MOVE  "STDATA11"    TO  MCP-WIDGET
             WHEN    36
                    MOVE  "EDDATA11"    TO  MCP-WIDGET
             WHEN    34
                    MOVE  "CHK11"      TO  MCP-WIDGET
             WHEN    38
                    MOVE  "STDATA12"    TO  MCP-WIDGET
             WHEN    39
                    MOVE  "EDDATA12"    TO  MCP-WIDGET
             WHEN    37
                    MOVE  "CHK12"      TO  MCP-WIDGET
             WHEN    41
                    MOVE  "STDATA13"    TO  MCP-WIDGET
             WHEN    42
                    MOVE  "EDDATA13"    TO  MCP-WIDGET
             WHEN    40
                    MOVE  "CHK13"      TO  MCP-WIDGET
             WHEN    44
                    MOVE  "STDATA14"    TO  MCP-WIDGET
             WHEN    45
                    MOVE  "EDDATA14"    TO  MCP-WIDGET
             WHEN    43
                    MOVE  "CHK14"      TO  MCP-WIDGET
             WHEN    47
                    MOVE  "STDATA15"    TO  MCP-WIDGET
             WHEN    48
                    MOVE  "EDDATA15"    TO  MCP-WIDGET
             WHEN    46
                    MOVE  "CHK15"      TO  MCP-WIDGET
             WHEN    50
                    MOVE  "STDATA16"    TO  MCP-WIDGET
             WHEN    51
                    MOVE  "EDDATA16"    TO  MCP-WIDGET
             WHEN    49
                    MOVE  "CHK16"      TO  MCP-WIDGET
             WHEN    53
                    MOVE  "STDATA17"     TO  MCP-WIDGET
             WHEN    54
                    MOVE  "EDDATA17"     TO  MCP-WIDGET
             WHEN    52
                    MOVE  "CHK17"      TO  MCP-WIDGET
             WHEN    56
                    MOVE  "STDATA18"     TO  MCP-WIDGET
             WHEN    57
                    MOVE  "EDDATA18"     TO  MCP-WIDGET
             WHEN    55
                    MOVE  "CHK18"      TO  MCP-WIDGET
             WHEN    59
                    MOVE  "STDATA19"     TO  MCP-WIDGET
             WHEN    60
                    MOVE  "EDDATA19"     TO  MCP-WIDGET
             WHEN    58
                    MOVE  "CHK19"      TO  MCP-WIDGET
             WHEN    62
                    MOVE  "STDATA20"    TO  MCP-WIDGET
             WHEN    63
                    MOVE  "EDDATA20"    TO  MCP-WIDGET
             WHEN    61
                    MOVE  "CHK20"       TO  MCP-WIDGET
             WHEN    65
                    MOVE  "STDATA21"    TO  MCP-WIDGET
             WHEN    66
                    MOVE  "EDDATA21"    TO  MCP-WIDGET
             WHEN    64
                    MOVE  "CHK21"       TO  MCP-WIDGET
             WHEN    68
                    MOVE  "STDATA22"    TO  MCP-WIDGET
             WHEN    69
                    MOVE  "EDDATA22"    TO  MCP-WIDGET
             WHEN    67
                    MOVE  "CHK22"       TO  MCP-WIDGET
             WHEN    71
                    MOVE  "STDATA23"    TO  MCP-WIDGET
             WHEN    72
                    MOVE  "EDDATA23"    TO  MCP-WIDGET
             WHEN    70
                    MOVE  "CHK23"       TO  MCP-WIDGET
             WHEN    74
                    MOVE  "STDATA24"    TO  MCP-WIDGET
             WHEN    75
                    MOVE  "EDDATA24"    TO  MCP-WIDGET
             WHEN    73
                    MOVE  "CHK24"       TO  MCP-WIDGET
             WHEN    77
                    MOVE  "STDATA25"    TO  MCP-WIDGET
             WHEN    78
                    MOVE  "EDDATA25"    TO  MCP-WIDGET
             WHEN    76
                    MOVE  "CHK25"       TO  MCP-WIDGET
             WHEN    80
                    MOVE  "STDATA26"    TO  MCP-WIDGET
             WHEN    81
                    MOVE  "EDDATA26"    TO  MCP-WIDGET
             WHEN    79
                    MOVE  "CHK26"       TO  MCP-WIDGET
             WHEN    83
                    MOVE  "STDATA27"     TO  MCP-WIDGET
             WHEN    84
                    MOVE  "EDDATA27"     TO  MCP-WIDGET
             WHEN    82
                    MOVE  "CHK27"      TO  MCP-WIDGET
             WHEN    86
                    MOVE  "STDATA28"     TO  MCP-WIDGET
             WHEN    87
                    MOVE  "EDDATA28"     TO  MCP-WIDGET
             WHEN    85
                    MOVE  "CHK28"      TO  MCP-WIDGET
             WHEN    89
                    MOVE  "STDATA29"     TO  MCP-WIDGET
             WHEN    90
                    MOVE  "EDDATA29"     TO  MCP-WIDGET
             WHEN    88
                    MOVE  "CHK29"      TO  MCP-WIDGET
             WHEN    92
                    MOVE  "STDATA30"    TO  MCP-WIDGET
             WHEN    93
                    MOVE  "EDDATA30"    TO  MCP-WIDGET
             WHEN    91
                    MOVE  "CHK30"       TO  MCP-WIDGET
             WHEN    95
                    MOVE  "STDATA31"    TO  MCP-WIDGET
             WHEN    96
                    MOVE  "EDDATA31"    TO  MCP-WIDGET
             WHEN    94
                    MOVE  "CHK31"       TO  MCP-WIDGET
             WHEN    98
                    MOVE  "STDATA32"    TO  MCP-WIDGET
             WHEN    99
                    MOVE  "EDDATA32"    TO  MCP-WIDGET
             WHEN    97
                    MOVE  "CHK32"       TO  MCP-WIDGET
             WHEN    101
                    MOVE  "STDATA33"    TO  MCP-WIDGET
             WHEN    102
                    MOVE  "EDDATA33"    TO  MCP-WIDGET
             WHEN    100
                    MOVE  "CHK33"       TO  MCP-WIDGET
             WHEN    104
                    MOVE  "STDATA34"    TO  MCP-WIDGET
             WHEN    105
                    MOVE  "EDDATA34"    TO  MCP-WIDGET
             WHEN    103
                    MOVE  "CHK34"       TO  MCP-WIDGET
             WHEN    107
                    MOVE  "STDATA35"    TO  MCP-WIDGET
             WHEN    108
                    MOVE  "EDDATA35"    TO  MCP-WIDGET
             WHEN    106
                    MOVE  "CHK35"       TO  MCP-WIDGET
             WHEN    110
                    MOVE  "STDATA36"    TO  MCP-WIDGET
             WHEN    111
                    MOVE  "EDDATA36"    TO  MCP-WIDGET
             WHEN    109
                    MOVE  "CHK36"       TO  MCP-WIDGET
             WHEN    201
                    MOVE  "PRTNM01"    TO  MCP-WIDGET
             WHEN    203
                    MOVE  "PRTNM02"    TO  MCP-WIDGET
             WHEN    205
                    MOVE  "PRTNM03"    TO  MCP-WIDGET
             WHEN    207
                    MOVE  "PRTNM04"    TO  MCP-WIDGET
             WHEN    209
                    MOVE  "PRTNM05"    TO  MCP-WIDGET
             WHEN    211
                    MOVE  "PRTNM06"    TO  MCP-WIDGET
             WHEN    213
                    MOVE  "PRTNM07"    TO  MCP-WIDGET
             WHEN    215
                    MOVE  "PRTNM08"    TO  MCP-WIDGET
             WHEN    217
                    MOVE  "PRTNM09"    TO  MCP-WIDGET
             WHEN    219
                    MOVE  "PRTNM10"    TO  MCP-WIDGET
             WHEN    221
                    MOVE  "PRTNM11"    TO  MCP-WIDGET
             WHEN    223
                    MOVE  "PRTNM12"    TO  MCP-WIDGET
             WHEN    225
                    MOVE  "PRTNM13"    TO  MCP-WIDGET
             WHEN    227
                    MOVE  "PRTNM14"    TO  MCP-WIDGET
             WHEN    229
                    MOVE  "PRTNM15"    TO  MCP-WIDGET
             WHEN    231
                    MOVE  "PRTNM16"    TO  MCP-WIDGET
             WHEN    233
                    MOVE  "PRTNM17"    TO  MCP-WIDGET
             WHEN    235
                    MOVE  "PRTNM18"    TO  MCP-WIDGET
             WHEN    237
                    MOVE  "PRTNM19"    TO  MCP-WIDGET
             WHEN    239
                    MOVE  "PRTNM20"    TO  MCP-WIDGET
             WHEN    241
                    MOVE  "PRTNM21"    TO  MCP-WIDGET
             WHEN    243
                    MOVE  "PRTNM22"    TO  MCP-WIDGET
             WHEN    245
                    MOVE  "PRTNM23"    TO  MCP-WIDGET
             WHEN    247
                    MOVE  "PRTNM24"    TO  MCP-WIDGET
             WHEN    249
                    MOVE  "PRTNM25"    TO  MCP-WIDGET
             WHEN    251
                    MOVE  "PRTNM26"    TO  MCP-WIDGET
             WHEN    253
                    MOVE  "PRTNM27"    TO  MCP-WIDGET
             WHEN    255
                    MOVE  "PRTNM28"    TO  MCP-WIDGET
             WHEN    257
                    MOVE  "PRTNM29"    TO  MCP-WIDGET
             WHEN    259
                    MOVE  "PRTNM30"    TO  MCP-WIDGET
             WHEN    261
                    MOVE  "PRTNM31"    TO  MCP-WIDGET
             WHEN    263
                    MOVE  "PRTNM32"    TO  MCP-WIDGET
             WHEN    265
                    MOVE  "PRTNM33"    TO  MCP-WIDGET
             WHEN    267
                    MOVE  "PRTNM34"    TO  MCP-WIDGET
             WHEN    269
                    MOVE  "PRTNM35"    TO  MCP-WIDGET
             WHEN    271
                    MOVE  "PRTNM36"    TO  MCP-WIDGET
             WHEN    202
                    MOVE  "PRTNM_N01"  TO  MCP-WIDGET
             WHEN    204
                    MOVE  "PRTNM_N02"  TO  MCP-WIDGET
             WHEN    206
                    MOVE  "PRTNM_N03"  TO  MCP-WIDGET
             WHEN    208
                    MOVE  "PRTNM_N04"  TO  MCP-WIDGET
             WHEN    210
                    MOVE  "PRTNM_N05"  TO  MCP-WIDGET
             WHEN    212
                    MOVE  "PRTNM_N06"  TO  MCP-WIDGET
             WHEN    214
                    MOVE  "PRTNM_N07"  TO  MCP-WIDGET
             WHEN    216
                    MOVE  "PRTNM_N08"  TO  MCP-WIDGET
             WHEN    218
                    MOVE  "PRTNM_N09"  TO  MCP-WIDGET
             WHEN    220
                    MOVE  "PRTNM_N10"  TO  MCP-WIDGET
             WHEN    222
                    MOVE  "PRTNM_N11"  TO  MCP-WIDGET
             WHEN    224
                    MOVE  "PRTNM_N12"  TO  MCP-WIDGET
             WHEN    226
                    MOVE  "PRTNM_N13"  TO  MCP-WIDGET
             WHEN    228
                    MOVE  "PRTNM_N14"  TO  MCP-WIDGET
             WHEN    230
                    MOVE  "PRTNM_N15"  TO  MCP-WIDGET
             WHEN    232
                    MOVE  "PRTNM_N16"  TO  MCP-WIDGET
             WHEN    234
                    MOVE  "PRTNM_N17"  TO  MCP-WIDGET
             WHEN    236
                    MOVE  "PRTNM_N18"  TO  MCP-WIDGET
             WHEN    238
                    MOVE  "PRTNM_N19"  TO  MCP-WIDGET
             WHEN    240
                    MOVE  "PRTNM_N20"  TO  MCP-WIDGET
             WHEN    242
                    MOVE  "PRTNM_N21"  TO  MCP-WIDGET
             WHEN    244
                    MOVE  "PRTNM_N22"  TO  MCP-WIDGET
             WHEN    246
                    MOVE  "PRTNM_N23"  TO  MCP-WIDGET
             WHEN    248
                    MOVE  "PRTNM_N24"  TO  MCP-WIDGET
             WHEN    250
                    MOVE  "PRTNM_N25"  TO  MCP-WIDGET
             WHEN    252
                    MOVE  "PRTNM_N26"  TO  MCP-WIDGET
             WHEN    254
                    MOVE  "PRTNM_N27"  TO  MCP-WIDGET
             WHEN    256
                    MOVE  "PRTNM_N28"  TO  MCP-WIDGET
             WHEN    258
                    MOVE  "PRTNM_N29"  TO  MCP-WIDGET
             WHEN    260
                    MOVE  "PRTNM_N30"  TO  MCP-WIDGET
             WHEN    262
                    MOVE  "PRTNM_N31"  TO  MCP-WIDGET
             WHEN    264
                    MOVE  "PRTNM_N32"  TO  MCP-WIDGET
             WHEN    266
                    MOVE  "PRTNM_N33"  TO  MCP-WIDGET
             WHEN    268
                    MOVE  "PRTNM_N34"  TO  MCP-WIDGET
             WHEN    270
                    MOVE  "PRTNM_N35"  TO  MCP-WIDGET
             WHEN    272
                    MOVE  "PRTNM_N36"  TO  MCP-WIDGET
             WHEN    180 
                    MOVE  "PRTJYUN1"   TO  MCP-WIDGET
             WHEN    181
                    MOVE  "SYUKA"      TO  MCP-WIDGET
             WHEN    182 
                    MOVE  "PRTJYUN2"   TO  MCP-WIDGET
             WHEN    199
                    MOVE  "B12"        TO  MCP-WIDGET   
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SYOKBN"
               WHEN    "PREFKBN"
               WHEN    "PRTJYUN"
               WHEN    "PRTJYUN1"
               WHEN    "PRTJYUN2"
               WHEN    "PRTKBN"
               WHEN    "SYUKA"
                   MOVE    199         TO  SPA-GMN-CUR
               WHEN    "CHK01"
               WHEN    "CHK02"
               WHEN    "CHK03"
               WHEN    "CHK04"
               WHEN    "CHK05"
               WHEN    "CHK06"
               WHEN    "CHK07"
               WHEN    "CHK08"
               WHEN    "CHK09"
               WHEN    "CHK10"
               WHEN    "CHK11"
               WHEN    "CHK12"
               WHEN    "CHK13"
               WHEN    "CHK14"
               WHEN    "CHK15"
               WHEN    "CHK16"
               WHEN    "CHK17"
               WHEN    "CHK18"
               WHEN    "CHK19"
               WHEN    "CHK20"
               WHEN    "CHK21"
               WHEN    "CHK22"
               WHEN    "CHK23"
               WHEN    "CHK24"
               WHEN    "CHK25"
               WHEN    "CHK26"
               WHEN    "CHK27"
               WHEN    "CHK28"
               WHEN    "CHK29"
               WHEN    "CHK30"
               WHEN    "CHK31"
               WHEN    "CHK32"
               WHEN    "CHK33"
               WHEN    "CHK34"
               WHEN    "CHK35"
               WHEN    "CHK36"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET2         TO  IDX
                   COMPUTE SPA-GMN-CUR         =   2   +   IDX *   3
               WHEN    "STDATA01"
               WHEN    "STDATA02"
               WHEN    "STDATA03"
               WHEN    "STDATA04"
               WHEN    "STDATA05"
               WHEN    "STDATA06"
               WHEN    "STDATA07"
               WHEN    "STDATA08"
               WHEN    "STDATA09"
               WHEN    "STDATA10"
               WHEN    "STDATA11"
               WHEN    "STDATA12"
               WHEN    "STDATA13"
               WHEN    "STDATA14"
               WHEN    "STDATA15"
               WHEN    "STDATA16"
               WHEN    "STDATA17"
               WHEN    "STDATA18"
               WHEN    "STDATA19"
               WHEN    "STDATA20"
               WHEN    "STDATA21"
               WHEN    "STDATA22"
               WHEN    "STDATA23"
               WHEN    "STDATA24"
               WHEN    "STDATA25"
               WHEN    "STDATA26"
               WHEN    "STDATA27"
               WHEN    "STDATA28"
               WHEN    "STDATA29"
               WHEN    "STDATA30"
               WHEN    "STDATA31"
               WHEN    "STDATA32"
               WHEN    "STDATA33"
               WHEN    "STDATA34"
               WHEN    "STDATA35"
               WHEN    "STDATA36"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET3         TO  IDX 
                   COMPUTE SPA-GMN-CUR         =   3   +   IDX *   3
               WHEN    "EDDATA01"
               WHEN    "EDDATA02"
               WHEN    "EDDATA03"
               WHEN    "EDDATA04"
               WHEN    "EDDATA05"
               WHEN    "EDDATA06"
               WHEN    "EDDATA07"
               WHEN    "EDDATA08"
               WHEN    "EDDATA09"
               WHEN    "EDDATA10"
               WHEN    "EDDATA11"
               WHEN    "EDDATA12"
               WHEN    "EDDATA13"
               WHEN    "EDDATA14"
               WHEN    "EDDATA15"
               WHEN    "EDDATA16"
               WHEN    "EDDATA17"
               WHEN    "EDDATA18"
               WHEN    "EDDATA19"
               WHEN    "EDDATA20"
               WHEN    "EDDATA21"
               WHEN    "EDDATA22"
               WHEN    "EDDATA23"
               WHEN    "EDDATA24"
               WHEN    "EDDATA25"
               WHEN    "EDDATA26"
               WHEN    "EDDATA27"
               WHEN    "EDDATA28"
               WHEN    "EDDATA29"
               WHEN    "EDDATA30"
               WHEN    "EDDATA31"
               WHEN    "EDDATA32"
               WHEN    "EDDATA33"
               WHEN    "EDDATA34"
               WHEN    "EDDATA35"
               WHEN    "EDDATA36"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
                   MOVE    WRK-WIDGET3         TO  IDX 
                   COMPUTE SPA-GMN-CUR         =   201      +
                                                  (IDX - 1) * 2 
               WHEN    "PRTNM01"
               WHEN    "PRTNM02"
               WHEN    "PRTNM03"
               WHEN    "PRTNM04"
               WHEN    "PRTNM05"
               WHEN    "PRTNM06"
               WHEN    "PRTNM07"
               WHEN    "PRTNM08"
               WHEN    "PRTNM09"
               WHEN    "PRTNM10"
               WHEN    "PRTNM11"
               WHEN    "PRTNM12"
               WHEN    "PRTNM13"
               WHEN    "PRTNM14"
               WHEN    "PRTNM15"
               WHEN    "PRTNM16"
               WHEN    "PRTNM17"
               WHEN    "PRTNM18"
               WHEN    "PRTNM19"
               WHEN    "PRTNM20"
               WHEN    "PRTNM21"
               WHEN    "PRTNM22"
               WHEN    "PRTNM23"
               WHEN    "PRTNM24"
               WHEN    "PRTNM25"
               WHEN    "PRTNM26"
               WHEN    "PRTNM27"
               WHEN    "PRTNM28"
               WHEN    "PRTNM29"
               WHEN    "PRTNM30"
               WHEN    "PRTNM31"
               WHEN    "PRTNM32"
               WHEN    "PRTNM33"
               WHEN    "PRTNM34"
               WHEN    "PRTNM35"
               WHEN    "PRTNM36"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET (2:)
                   MOVE    WRK-WIDGET3         TO  IDX
                   COMPUTE SPA-GMN-CUR         =   202      +
                                                  (IDX - 1) * 2
               WHEN    "PRTNM_N01"
               WHEN    "PRTNM_N02"
               WHEN    "PRTNM_N03"
               WHEN    "PRTNM_N04"
               WHEN    "PRTNM_N05"
               WHEN    "PRTNM_N06"
               WHEN    "PRTNM_N07"
               WHEN    "PRTNM_N08"
               WHEN    "PRTNM_N09"
               WHEN    "PRTNM_N10"
               WHEN    "PRTNM_N11"
               WHEN    "PRTNM_N12"
               WHEN    "PRTNM_N13"
               WHEN    "PRTNM_N14"
               WHEN    "PRTNM_N15"
               WHEN    "PRTNM_N16"
               WHEN    "PRTNM_N17"
               WHEN    "PRTNM_N18"
               WHEN    "PRTNM_N19"
               WHEN    "PRTNM_N20"
               WHEN    "PRTNM_N21"
               WHEN    "PRTNM_N22"
               WHEN    "PRTNM_N23"
               WHEN    "PRTNM_N24"
               WHEN    "PRTNM_N25"
               WHEN    "PRTNM_N26"
               WHEN    "PRTNM_N27"
               WHEN    "PRTNM_N28"
               WHEN    "PRTNM_N29"
               WHEN    "PRTNM_N30"
               WHEN    "PRTNM_N31"
               WHEN    "PRTNM_N32"
               WHEN    "PRTNM_N33"
               WHEN    "PRTNM_N34"
               WHEN    "PRTNM_N35"
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET (2:)
                   MOVE    WRK-WIDGET4         TO  IDX 
                   COMPUTE SPA-GMN-CUR         =   4   +   IDX *  3
               WHEN    "PRTNM_N36"
                   MOVE    199         TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"            TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "印刷するレセプトがありません" 
                                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE  "印刷用パラメタファイルが作成できませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "一括出力のとき入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "個別出力のとき入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "印刷するレセプトが選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "前頁はありません"         TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "次頁はありません"         TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "選択されている出力順のときは変更できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "診療科入力エラー"          TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "選択されている出力順のときは入力できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "対象のレセプトが選択してありません"
                                                       TO  SPA-ERRMSG
               WHEN    "0100"
                   MOVE    "表示できない保険者がありました"
                                                       TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    "レセプト印刷またはレセ電処理中です"
                                                       TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
      *
               WHEN    "E88"
                   MOVE    "Ｄａｔａ＿Ｉｄが取得できません。"
                                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RERR
           MOVE    SPA-ERRCD            TO  RERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  RERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "RERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1003"
                   MOVE    "件数印刷をします"        TO  WRK-RIDMSG
               WHEN    "6666"
                   MOVE    "レセプト印刷中です"      TO  WRK-RIDMSG
               WHEN    "8888"
                   MOVE    "処理が正常に終了しました"
                                                     TO  WRK-RIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RID1
           INITIALIZE                       RID1
           MOVE    SPA-RIDCD            TO  RID1-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID1-ID1MSG
           MOVE    "戻る"               TO  RID1-B01-LABEL
           MOVE    "blue"               TO  RID1-LABEL-STYLE
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "RID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-RIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ３表示処理
      *****************************************************************
       530-RID3SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD3
               WHEN    "1001"
                   MOVE    "全件印刷をします"        TO  WRK-RIDMSG
               WHEN    "1002"
                   MOVE    "指定印刷をします"        TO  WRK-RIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RID3
           INITIALIZE                       RID3
           MOVE    SPA-RIDCD3           TO  RID3-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID3-ID1MSG
           MOVE    "blue"               TO  RID3-LABEL-STYLE
           IF      SPA-R01-PRTKBN       =   "2" OR  "3" OR  "4" OR  "5"
               MOVE    "【点検用レセプト】"     TO  RID3-LABEL
           END-IF
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "R02"                TO  SPA-MOTOPG
           MOVE    "RID3"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID3"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-RID3SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ編集処理
      *****************************************************************
       5001-PRTNM-HENSYU-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-GMN-PRTNM-G
      *
           IF      WRK-PRTNMMEI        =   SPACE
               MOVE    SPA-GMN-PRTNMLST (1)
                                           TO  WRK-GMN-PRTNM-G
           ELSE
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       SPA-GMN-PRTNM-MAX
                   IF      WRK-PRTNMMEI    =   SPA-GMN-PRTNMMEIL (IDXX)
                       MOVE    SPA-GMN-PRTNMLST (IDXX)
                                              TO  WRK-GMN-PRTNM-G
                       MOVE    SPA-GMN-PRTNM-MAX
                                              TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
           .
       5001-PRTNM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YM   (1:5)  NOT NUMERIC
               INSPECT     WRK-YM       REPLACING   ALL " "
                                                    BY  "0"
               MOVE    WRK-YM(1:1)      TO  WRK-WGO
               MOVE    WRK-YM(2:2)      TO  WRK-WYY
               MOVE    WRK-YM(5:2)      TO  WRK-WMM
           ELSE
               MOVE    WRK-YM(1:5)      TO  WRK-WYMD (1:5)
           END-IF
           MOVE    1                    TO  WRK-WDD
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "12"                TO  LNK-DAY1-IRAI
           MOVE    ZERO                TO  LNK-DAY1-YMD
           MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1     NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG1
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票管理ファイル読込
      *****************************************************************
       900-RECE07-READ-SEC             SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票管理ファイル読込
      *****************************************************************
       900-RECE07-SELECT-SEC             SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECE07-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           .
       900-RECE07-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票管理ファイル読込（ＫＥＹ２）
      *****************************************************************
       900-RECE07-NEXT-SEC             SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-RECE07
               MOVE    MCPDATA-REC         TO  RECE07-REC
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           .
       900-RECE07-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      * 
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトデータ読み込み
      *****************************************************************
       900-QUERY-RECE08-NEXT-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-QRECEPRT
               MOVE    MCPDATA-REC         TO  QUERY-RECE08-REC
           ELSE
               MOVE    1                   TO  FLG-QRECEPRT
           END-IF
      *
           .
       900-QUERY-RECE08-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブルＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-SPATMP-DBSELECT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブルＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       900-SPATMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブルクローズ処理
      *****************************************************************
       900-SPATMP-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル削除処理
      *****************************************************************
       900-SPATMP-DBDELETE-SEC         SECTION.
      *
           MOVE    1                   TO  FLG-END2
      *
           INITIALIZE                      SPATMP-REC
           MOVE    "R02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
      *
           INITIALIZE                      SPATMP-REC
           MOVE    "R01"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 900-SPATMP-CALL-SEC
      *
           .
       900-SPATMP-DBDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       900-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       900-SPATMP-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
