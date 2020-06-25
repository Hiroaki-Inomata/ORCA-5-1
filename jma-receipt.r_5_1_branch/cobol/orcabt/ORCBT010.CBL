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
       PROGRAM-ID.             ORCBT010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 月次統計データ用基本情報作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/11/12    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/02/01  病名コード２１に変更
      *  01.00.02    NACL-藤原    04/02/06  包括の対象に保険組合せ９９９９
      *                                     を追加
      *  01.00.03    NACL-藤原    04/02/09  患者自己負担額が公費の上限額
      *                                     に到達しない場合のレセ種別
      *  01.00.04    NACL-藤原    04/03/08  シス管による生保受給者番号の編集
      *  01.00.05    NACL-藤原    04/04/05  病名コード９９９９９９９読み飛ばす
      *  01.00.06    NACL-藤原    04/04/06  編集病名のとき疑いフラグからの
      *                                     病名コード追加をやめる
      *  01.00.07    NACL-藤原    04/04/27  ユーザーコメントの追加
      *  01.00.08    NACL-藤原    04/08/17  特例退職者＋老人のとき本人の
      *                                     み不明分とする
      *                                     補助区分の追加に伴うレセプト
      *                                     種別の編集を変更
      *  01.00.05    NACL-藤原    05/01/30  入院対応
      *  01.00.06    NACL-藤原    05/04/05  県番号によるレセプト種別の設定
      *                                     を中止
      *
      *  02.06.01    NACL-藤原    05/07/26  老人又は前期高齢者のとき、
      *                                     月途中に保険者は変わらずに
      *                                     保険が変更になった場合は１枚
      *                                     にする
      *                                     ただし、給付割合が変更になっ
      *                                     た場合はそれぞれにレセプトを
      *                                     作成する
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    06/01/25  疑いフラグからの病名の編集方
      *                                     法の変更
      *
      *  03.00.01    NACL-藤原    06/06/07  包括の場合のデータ作成の修正
      *
      *  03.03.02    NACL-藤原    06/09/27  ２００６年１０月改正対応
      *  03.03.02    NACL-藤原    06/10/10  主保険がない場合、精神入院は
      *                                     公費単独のレセプトとする
      *  03.03.05    NACL-藤原    06/10/27  レセプトで生活保護の請求がな
      *                                     い場合記載の対象としない
      *  03.03.06    NACL-藤原    06/12/15  入院の包括のデータ作成の修正
      *
      *  03.04.01    NACL-藤原    07/02/06  月途中の受給者番号変更に対応
      *  03.04.02    NACL-藤原    07/03/06  包括診療判定の追加
      *  03.04.03    NACL-藤原    07/04/04  特例退職医療の種別決定条件の修正
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-小豆沢  07/11/16  公務災害、公害の種別判定追加
      *  04.01.02    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      *
      *  04.02.01    NACL-藤原    08/04/10  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/04/23  後期高齢者の船員保険を本人とする
      *  04.02.03    NACL-門脇    08/08/07  精神通院の請求点判定追加
      *
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *  04.03.02    NACL-門脇    08/09/02  請求点判定追加
      *  04.03.03    NACL-門脇    08/12/17  平成２１年１月改正対応
      *
      *  04.04.01    NACL-藤原    09/01/14  月途中に医保と生保の併用から
      *                                     生保単独になったときのまとめ
      *  04.04.02    NACL-藤原    09/04/09  入院基本料・特定入院料・食事
      *                                     療養出力時の連番の初期化
      *  04.04.03    NACL-藤原    09/10/21  月途中の負担者番号変更対応
      *                                     （自立支援の公費単独） 
      *  04.04.04    NACL-藤原    09/12/17  月途中に医保と自立支援の併用
      *                                     から自立支援単独になったとき
      *                                     のまとめ対応
      *  04.04.05    NACL-藤原    10/08/23  後期高齢非該当対応
      *                                     （高齢受給者票がない場合） 
      *  04.04.06    NACL-藤原    10/09/14  医療観察法対応
      *                                     公費単独のレセプトとする
      *
      *  04.05.01    NACL-藤原    10/03/12  平成２２年４月改正対応
      *                                     平成２２年４月分以降の政府管
      *                                     掌をエラーとする
      *  04.05.06    NACL-藤原    11/08/25  公費単独対応
      *                                     公費を最大５０に変更
      *  04.05.07    NACL-藤原    12/07/02  住所欄に番地方書を追加
      *  04.05.08    NACL-藤原    12/11/15  剤点数等のマイナス表記対応
      *
      *  04.06.01    NACL-藤原    10/11/05  補足コメント対応
      *  04.06.02    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.03    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.06.04    NACL-藤原    11/02/24  月途中に医保と児童保護(53)の
      *                                     併用から単独になったときのまとめ
      *  04.06.05    NACL-藤原    12/10/04  同一レセプトの公費が４つを超
      *                                     えた場合の種別不明対応
      *  04.06.06    NACL-藤原    12/09/27  生活保護期間重複対応
      *  04.06.07    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.02    NACL-藤原    14/03/30  平成２６年４月改正対応
      *  04.07.03    NACL-藤原    14/06/03  同日再入院対応
      *  04.07.04    NACL-藤原    14/06/05  月途中のまとめのとき第二公費
      *                                     マル長のときも対象とする
      *  04.07.05    NACL-藤原    14/08/18  ７５歳以上の海外居住者２割対応
      *  04.07.06    NACL-門脇    14/09/01  平成２６年１０月改正対応
      *  04.07.07    NACL-門脇    15/03/16  平成２７年４月改正対応
      *  04.07.08    NACL-藤原    15/05/12  存在しない保険組合せの情報設定
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-藤原    15/10/08  複数枚レセプト分割対応
      *                                     公費単独の２回以上の分割対応
      *  04.08.03    NACL-藤原    15/11/13  診療データがない場合は対象の
      *                                     病名があっても病名データも作成しない
      *  04.08.04    NACL-門脇    16/03/28  平成２８年４月改正対応
      *  04.08.05    NACL-小豆沢  16/09/16  持参薬は出力しないよう対応
      *  04.08.06    NACL-門脇    17/12/13  包括点数の公害補償法減点分のマイナス
      *                                     集計対応
      *  04.08.07    NACL-藤原    18/03/28  平成３０年４月改正対応
      *
      *  05.00.01    NACL-太田    17/11/21  入院料と異なる食事算定保険組み合わせ対応
      *  05.00.02    NACL-藤原    19/07/09  公費単独＋沖縄県の精神障害者
      *                                     特別措置(121)の対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    月次統計データ（基本情報）
           SELECT  TOUKEI01-FILE   ASSIGN  TOUKEI01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI01-KEY
                                   FILE    STATUS  IS  STS-TOUKEI01.
      *
      *    月次統計データ（統計情報）
           SELECT  TOUKEI02-FILE   ASSIGN  TOUKEI02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI02-KEY
                                   FILE    STATUS  IS  STS-TOUKEI02.
      *
      *    月次統計データ（病名情報）
           SELECT  TOUKEI03-FILE   ASSIGN  TOUKEI03PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOUKEI03-KEY
                                   FILE    STATUS  IS  STS-TOUKEI03.
      *
      *    エラーファイル
           SELECT  TOUKEIERR-FILE  ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    月次統計データ
       FD  TOUKEI01-FILE.
       01  TOUKEI01-REC. 
           COPY    "CPTCF001.INC".
           03  TOUKEI01-SEIHO-OCC.
               05  TOUKEI01-SEIHO-KEY
                                   PIC X(50)   OCCURS  10.
      *
      *    月次統計データ
       FD  TOUKEI02-FILE.
       01  TOUKEI02-REC. 
           COPY    "CPTCF002.INC".
      *
      *    月次統計データ
       FD  TOUKEI03-FILE.
       01  TOUKEI03-REC. 
           COPY    "CPTCF003.INC".
      *
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI01PARA//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI02PARA//.
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI03PARA//.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //TOUKEIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TOUKEI01                    PIC X(02).
           03  STS-TOUKEI02                    PIC X(02).
           03  STS-TOUKEI03                    PIC X(02).
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-HKNCOMBI                    PIC 9(01). 
           03  FLG-HKNNUM                      PIC 9(01). 
           03  FLG-TOUKEI01                    PIC 9(01).
           03  FLG-PTHKNINF                    PIC 9(01).
           03  FLG-PTKOHINF                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-KYUSEIRRK                   PIC 9(01).
           03  FLG-MONTHLYNUM                  PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-PTCOM                       PIC 9(01).
           03  FLG-PTRSIINF                    PIC 9(01).
           03  FLG-ADRS                        PIC 9(01).
           03  FLG-PTBYOMEI                    PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-SRYACCT                     PIC 9(01).
           03  FLG-PT-HDIST-DAY                PIC 9(01).
      * 
           03  FLG-KOH                         PIC 9(01).
           03  FLG-CONTKBN                     PIC 9(01).
           03  FLG-SET                         PIC 9(01).
           03  FLG-LASTHKNID                   PIC 9(01). 
           03  FLG-SEIHO                       PIC 9(01).
           03  FLG-WRITE                       PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-TOUKEI01                    PIC 9(06).
           03  CNT-TOUKEI02                    PIC 9(06).
           03  CNT-TOUKEI03                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDA                             PIC 9(03). 
           03  IDB                             PIC 9(03). 
           03  IDX                             PIC 9(03). 
           03  IDX1                            PIC 9(03). 
           03  IDX2                            PIC 9(03). 
           03  IDX3                            PIC 9(03). 
           03  IDX4                            PIC 9(03). 
           03  IDX5                            PIC 9(03). 
           03  IDX6                            PIC 9(03). 
           03  IDX7                            PIC 9(03). 
           03  IDY                             PIC 9(03). 
           03  IDZ                             PIC 9(03).
           03  IDXX                            PIC 9(03).
           03  IDYY                            PIC 9(03).
           03  IDZZ                            PIC 9(03).
           03  IDXD                            PIC 9(03).
      **** 03  SYSKAN-IDX                      PIC 9(03).
      *
           03  IDZ4                            PIC 9(03).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
               05  KEY-N-HKNCOMBI              PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
               05  KEY-O-HKNCOMBI              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-HOSPID             PIC X(24).
               05  WRK-PARA-PREFNUM            PIC X(02). 
               05  WRK-PARA-SRYYM              PIC X(06).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-DATAKBN            PIC X(01).
               05  WRK-PARA-HOUKATUKBN         PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
      *
           03  WRK-TOUKEIERR               PIC X(200). 
           03  WRK-ERRMSG                  PIC X(300).
      *
           03  WRK-MCP-TABLE               PIC X(64).
           03  WRK-MCP-PATHNAME            PIC X(64).
      *
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN              PIC X(01).
               05  WRK-TEISEIKBN           PIC X(01).
      *
           03  WRK-DATAKBN             PIC X(01).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-RENNUM              PIC 9(03).
      * 
           03  WRK-PREFKBN             PIC X(01).
           03  WRK-PREFNUM             PIC X(02).
      *
           03  WRK-SEIHO-FLG           PIC 9(01).
      *
           03  WRK-PTID                PIC 9(10).
      *
           03  ST-IDX                  PIC 9(03).
      *
           03  WRK-RECEIPT-KBN         PIC X(10).
      *
      *    後期高齢者非該当フラグ
           03  WRK-KOUKI-FLG           PIC 9(01).
      *
           03  WRK-TEKSTYMD.
               05  WRK-TEKSTYYMM       PIC 9(06).
               05  WRK-TEKSTDD         PIC 9(02).
      *
      *    傷病名編集
           03  WRK-BYOMEI          PIC X(150).
           03  WRK-BYOMEIW         PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1         PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2         PIC X(01). 
      *    病名文字数         
           03  WRK-MOJISU          PIC 9(03).
      *
           03  WRK-YM              PIC 9(06).
      *
           03  WRK-HKNCOMBINUM-G.
               05  WRK-HKNCOMBINUM PIC 9(04).
      *
      *    レセプト種別判定用領域
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO                               PIC X(01).
           03  WK-IHO                                  PIC X(01).
           03  WK-RJN                                  PIC X(01).
           03  WK-KOHI-CNT                             PIC 9(02).
           03  WK-HONKZKKBN                            PIC X(01).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08).
      *              
      *    診療回数テーブル（入院会計用　１：基本　２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *     
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).   
      *
       01  WRK-TOUKEI01-TABLE.
           02  WRK-TOUKEI01-OCC    OCCURS  100.
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //WRK-TOUKEI01//.
               03  WRK-TOUKEI01-SYOKBN
                                   PIC X(01).
           02  WRK-TOUKEI01-SEIHO-OCC
                                   OCCURS  100.
               03  WRK-TOUKEI01-SEIHO-KEY
                                   PIC X(50)   OCCURS  10.
       01  WRK-TOUKEI01-TABLE-MAX  PIC 9(03)   VALUE   100.
      *     
       01  WRK-SEIHO-TOUKEI01-REC. 
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //WRK-SEIHO-TOUKEI01//.
      *
      *    ワーク生保まとめ
       01  TBL-SEIHO-TOUKEI01-AREA.
           02  TBL-SEIHO-TOUKEI01-REC  OCCURS  50. 
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //TBL-SEIHO-TOUKEI01//.
           02  TBL-SEIHO-TOUKEI01-OCC  OCCURS  50.
               03  TBL-SEIHO-TOUKEI01-PRTFLG
                                   PIC 9(01).
           02  TBL-SEIHO-TOUKEI01-MAX
                                   PIC 9(03).
      *
       01  WRK-KOH-TOUKEI01-REC. 
           COPY    "CPTCF001.INC"  REPLACING  //TOUKEI01//
                                   BY         //WRK-KOH-TOUKEI01//.
      *
      *    ワーク患者病名
       01  WRK-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO//
                                   BY         //WRK-PTBYO//.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"
                   REPLACING   //SPA-//    BY //WRK-SPA-//.
      *
           COPY    "MCPAREA".
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      * 
      *    保険番号マスタ退避ワーク
       01  WRK-HKNNUM-AREA.
           03  WRK-HKNNUM-IDX                  PIC 9(05).
           03  WRK-HKNNUM-MAX                  PIC 9(05).
           03  WRK-CHK-HKNNUM                  PIC X(03).
           03  WRK-CHK-PAYKBN                  PIC X(02).
           03  WRK-CHK-TEKSTYMD                PIC X(08).
           03  WRK-CHK-TEKEDYMD                PIC X(08).
       01  TABLE-HKNNUM-AREA.
         02  TBL-HKNNUM-REC                    OCCURS  2000.
           COPY    "CPHKNNUM.INC"  REPLACING   //HKN-//
                                   BY          //TBL-HKN-//.        
       01  TBL-HKNNUM-CONST-MAX                PIC 9(05)
                                               VALUE   2000.
      *
      *    施設入所情報（９５０、９５１）退避用領域
       01  TABLE-PTKOHINF-AREA.
           02  TBL-PTKOHINF-REC        OCCURS  100.
           COPY    "CPPTKOHINF.INC"
                                   REPLACING   //PTKOH//
                                   BY          //TBL-PTKOH//.
           02  TBL-PTKOHINF-MAX    PIC 9(03).
      *
      *    患者レセプト分割情報退避用領域
       01  TBL-PT-HDIST-DAY-T.
         02  TBL-PT-HDIST-DAY-REC      OCCURS  10.
           COPY    "CPPT-HDIST-DAY.INC"
                                   REPLACING   //PT-HDIST-DAY//
                                   BY          //TBL-PT-HDIST-DAY//.
         02  TBL-PT-HDIST-DAY-MAX  PIC 9(02).
      *
       01  WRK-CONS-AREA.
      *    
      *    生活保護保険番号 
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012". 
      *    中国残留邦人等保険番号 
           03  WRK-CONS-ZANRYUU-HKNNUM PIC X(03)   VALUE   "025". 
      *    自費保険番号 
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980". 
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971". 
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".       
      *    原爆保険番号 
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".       
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM  PIC X(03)   VALUE   "975".
      *
      *    マイナス表記を行う診療行為コード
      *    薬剤料減点（合算薬剤料上限超）
           03  WRK-CONS-SRYCD1         PIC X(09)   VALUE   "630010001".
      *    薬剤料逓減（９０／１００）（内服薬）
           03  WRK-CONS-SRYCD2         PIC X(09)   VALUE   "630010002".
      *    包括点数の治験減点分
           03  WRK-CONS-SRYCD3         PIC X(09)   VALUE   "199000210".
      *    薬剤料逓減（８０／１００）（向精神薬多剤投与）
           03  WRK-CONS-SRYCD4         PIC X(09)   VALUE   "630010005".
      *    薬剤料逓減（６０／１００）（紹介率が低い大病院３０日以上投薬）
           03  WRK-CONS-SRYCD5         PIC X(09)   VALUE   "630010006".
      *    薬剤料減点（湿布薬薬剤料上限超）
           03  WRK-CONS-SRYCD6         PIC X(09)   VALUE   "630010007".
      *    包括点数の公害補償法減点分
           03  WRK-CONS-SRYCD7         PIC X(09)   VALUE   "199000410".
      *
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
           03  WRK-CONS-BYOMEICD-UTAGAI
                                   PIC X(07)   VALUE   "ZZZ8002".   
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4. 
           03  WRK-CONS-BYOMEICD-KYUSEI
                                   PIC X(07)   VALUE   "ZZZ4012".   
      *
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
           03  WRK-CONS-SRYYM-201004   PIC 9(06)   VALUE   201004.      
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
      *
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *     
           COPY    "CPSK2005.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
       COPY    "CPPTCOM.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    患者レセプト分割
       01  PT-HDIST-DAY-REC.
           COPY    "CPPT-HDIST-DAY.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    入院会計編集サブ
           COPY    "CPORCHCM34S01.INC".
      *
      *    公費付加情報取得サブ
           COPY    "CPORCSKOHPLUS.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *   保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *    患者自己負担金レセプト記載判定
           COPY    "CPORCSFTNJGNCHK.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    レセプト種別返却サブ
           COPY    "CPORCSSYUBETUGET.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    入院料と異なる食事算定保険組み合わせ取得サブ
           COPY    "CPORCSSKJHKN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
      ****************************************************************
       PROCEDURE           DIVISION
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
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
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
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       WRK-TOUKEI01-TABLE
                                       TTNS-AREA
                                       WRK-HKNNUM-AREA
                                       TABLE-HKNNUM-AREA  
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPID
                                               WRK-PARA-PREFNUM 
                                               WRK-PARA-SRYYM
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-HOUKATUKBN 
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               TOUKEIERR
           END-UNSTRING 
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "TOKEI010"          TO  TOUKEI01PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI01PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI01PARA-HOSPNUM
      *
           MOVE    "TOKEI020"          TO  TOUKEI02PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI02PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI02PARA-HOSPNUM
      *
           MOVE    "TOKEI030"          TO  TOUKEI03PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI03PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI03PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOUKEIERR           TO  SGETTEMP-BASENAMES  (1)
           MOVE    TOUKEI01PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    TOUKEI02PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    TOUKEI03PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  TOUKEI01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  TOUKEI02PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  TOUKEI03PARA-FULLNAME
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBT010"      TO  JOB-PGID
           MOVE    "月次統計データ作成"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
      *
           MOVE    WRK-SYMD        TO  WRK-STYMD
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM  TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    患者番号構成管理情報編集
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1009-STYUKYMD
                                           SYS-1009-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-HJNKSIKBN  
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN 
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
           ELSE
               MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *     
      *    レセプト編集区分情報
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    WRK-SYMD            TO  SYS-2005-STYUKYMD
                                           SYS-2005-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE               TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-RJNGNBKKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
           END-IF
      * 
      *    県内扱い保険者情報
           MOVE    SPACE           TO  WRK-1033-TABLE
      *     
           MOVE    SPACE           TO  SYS-1033-REC
           INITIALIZE                  SYS-1033-REC
           MOVE    "1033"          TO  SYS-1033-KANRICD
           MOVE    "*"             TO  SYS-1033-KBNCD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1033-HOSPNUM
           MOVE    SYS-1033-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               ADD     1                   TO  IDX
               MOVE    SYS-1033-STYUKYMD   TO  WRK-1033-STYUKYMD   (IDX)
               MOVE    SYS-1033-EDYUKYMD   TO  WRK-1033-EDYUKYMD   (IDX)
               MOVE    SYS-1033-TBL        TO  WRK-1033-HKNJANUM-G (IDX)
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           OPEN    OUTPUT              TOUKEI01-FILE
           CLOSE                       TOUKEI01-FILE
           OPEN    OUTPUT              TOUKEI02-FILE
           CLOSE                       TOUKEI02-FILE
           OPEN    OUTPUT              TOUKEI03-FILE
           CLOSE                       TOUKEI03-FILE
      *
           OPEN    I-O                 TOUKEI01-FILE
                                       TOUKEI02-FILE
                                       TOUKEI03-FILE 
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    "1" 
      *            入院会計読み込み（患者ＩＤ、診療科、剤識別区分順）
                   INITIALIZE                  NYUINACCT-REC
grpsys             MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  NACCT-SRYYM
                   MOVE    WRK-PARA-NYUGAIKBN  TO  NACCT-NYUGAIKBN
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                       MOVE    "key13"             TO  MCP-PATHNAME
                       PERFORM 900-NYUINACCT-READ-SEC
                   ELSE
                       INITIALIZE                  NYUINACCT-REC
                       MOVE    1                   TO  FLG-NYUINACCT
                   END-IF
      *
                   PERFORM 2001-NYUIN-HENSYU-SEC
                                       UNTIL   FLG-NYUINACCT   =   1
      *
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               WHEN    "2" 
      *            診療会計読み込み（患者ＩＤ、保険組合せ順）
                   INITIALIZE                  SRYACCT-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  ACCT-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  ACCT-SRYYM
                   MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
grpsys             PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
      *   
                   PERFORM 2001-NYUINGAI-HENSYU-SEC
                                       UNTIL   FLG-SRYACCT     =   1
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院処理
      *****************************************************************
       2001-NYUIN-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  WRK-TOUKEI01-TABLE
      *
           MOVE    ZERO                TO  WRK-RENNUM
      *     
      *    テスト患者のときは読み飛ばす
           MOVE    WRK-PARA-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    NACCT-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "NYUINACCT PTID=" NACCT-PTID
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF    ( FLG-PTINF           =   ZERO )
           AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      NACCT-PTID  NOT =   PTINF-PTID
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
      *
      *        患者レセプト分割情報情報
               MOVE    NACCT-PTID              TO  WRK-PTID
               PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
               INITIALIZE                      WRK-DAY-AREA
                                               WRK-HKNCOMBI-TABLE
      *
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      NACCT-PTID  NOT =   PTINF-PTID
      *            剤識別区分別テーブル編集
                   EVALUATE    NACCT-ZAISKBKBN
                       WHEN    "1"
                           MOVE    1               TO  IDX
                       WHEN    "5"
                           MOVE    2               TO  IDX
                   END-EVALUATE
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >       31
                       IF      NACCT-DAY  (IDX1)       >   ZERO
                           MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
                       END-IF
                   END-PERFORM            
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key13"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
      *
      *        同日再入院のチェック
               PERFORM 500-ORCSNYUINDAY-CHECK-SEC
      *
      *        入院料と異なる食事算定保険組み合わせのチェック
               IF    ( WRK-PARA-SRYYM          >=  "201710" )
                   PERFORM 500-ORCSSKJHKN-CHECK-SEC
               END-IF
      *
      *        入院会計の保険組合せをまとめる
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF  ( ( WRK-DAY (1 IDX)     >   ZERO )  OR
                         ( LNK-NYUINDAY-KBN (IDX)
                                               =   "1"  )    )  AND
                       (   WRK-DAY (2 IDX)     >   ZERO      )
                       MOVE    WRK-DAY (2 IDX) TO  WRK-HKNCOMBINUM
                       MOVE    IDX             TO  IDXD
                       PERFORM 20011-NYUINACCT-HENSYU-SEC
                   END-IF
               END-PERFORM        
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   MOVE    WRK-HKNCOMBI-REC (IDX2)
                                               TO  HKNCOMBI-REC    
      *
                   MOVE    "1"                 TO  WRK-DATAKBN
                   PERFORM 2001-FILE-SYORI-SEC
      *
      *            入院基本料等処理
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  NOT =   "1"
                       PERFORM 20013-NYUIN-KIHON-SEC
                   END-IF
               END-PERFORM
      *
      *        診療会計編集処理
               PERFORM 20012-NYUIN-SRYACCT-HENSYU-SEC
      *
               IF      TBL-PT-HDIST-DAY-MAX
                                           =   ZERO
                   MOVE    1               TO  ST-IDX
                   MOVE    SPACE           TO  WRK-RECEIPT-KBN
      *            基本ファイル出力処理
                   PERFORM   2002-KIHON-WRITE-SEC
               ELSE
                   MOVE    ZERO            TO  ST-IDX
                   PERFORM         UNTIL   ST-IDX  =   999
                       MOVE    SPACE           TO  WRK-RECEIPT-KBN
                       MOVE    ZERO            TO  ST-IDX
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       100
                               OR      WRK-TOUKEI01-HOSPNUM (IDXX)
                                               =   ZERO
                           IF      WRK-TOUKEI01-SYOKBN (IDXX)
                                                   =   SPACE
                               MOVE    WRK-TOUKEI01-RECEIPT-KBN (IDXX)
                                                   TO  WRK-RECEIPT-KBN
                               MOVE    IDXX    TO  ST-IDX
                               MOVE    100     TO  IDXX
                           END-IF 
                       END-PERFORM
      *
                       IF      ST-IDX          >   ZERO
      *                    基本ファイル出力処理
                           PERFORM   2002-KIHON-WRITE-SEC
                       ELSE
                           MOVE    999             TO  ST-IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF    
           .
       2001-NYUIN-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       20011-NYUINACCT-HENSYU-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *
      *        保険組合せ追加
               IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    WRK-PARA-HOSPNUM
                                           TO  COMB-HOSPNUM
                   MOVE    PTINF-PTID      TO  COMB-PTID
                   MOVE    WRK-HKNCOMBINUM TO
                                           COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
                   IF      FLG-HKNCOMBI    =   ZERO
                       MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
                   ELSE
                       MOVE    WRK-PARA-HOSPNUM 
                                               TO
                                           WRK-COMB-HOSPNUM     (IDX1)
                       MOVE    PTINF-PTID      TO
                                           WRK-COMB-PTID        (IDX1)
                       MOVE    WRK-HKNCOMBINUM TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                       MOVE    "1"     TO  WRK-HKNCOMBI-SYOKBN  (IDX1)
                   END-IF
               END-IF
      *
      *        適用範囲かチェック（範囲外のとき２をセット）
               IF      WRK-HKNCOMBINUM     =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)   NOT =   SPACE
                       CONTINUE                    
                   ELSE     
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDXD            TO  WRK-DD
                       IF      WRK-COMB-TEKSTYMD (IDX1)
                                               <=  WRK-YMD
                       AND     WRK-COMB-TEKEDYMD (IDX1)
                                               >=  WRK-YMD
                           CONTINUE
                       ELSE   
                           MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                       END-IF
                   END-IF
                   MOVE    10              TO  IDX1
               END-IF            
           END-PERFORM
           .
       20011-NYUINACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    診療会計編集処理（入院）
      *****************************************************************
       20012-NYUIN-SRYACCT-HENSYU-SEC           SECTION.
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
           MOVE    SPACE               TO  WRK-ERR-AREA
           INITIALIZE                      WRK-ERR-AREA
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    WRK-PARA-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  ACCT-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    PTINF-PTID          TO  ACCT-PTID
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      KEY-N-HKNCOMBI      =   9999
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  COMB-HOSPNUM
                   MOVE    PTINF-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    ZERO                TO  FLG-HKNCOMBI 
               ELSE
      *            保険組合せ読込み
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  COMB-HOSPNUM
                   MOVE    PTINF-PTID          TO  COMB-PTID
                   MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *
                   IF      FLG-HKNCOMBI    =   ZERO
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                        ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   ELSE
                       MOVE    WRK-PARA-HOSPNUM
                                              TO  COMB-HOSPNUM
                       MOVE    PTINF-PTID     TO  COMB-PTID
                       MOVE    KEY-N-HKNCOMBI TO  COMB-HKNCOMBINUM 
                       MOVE    "1"            TO  WRK-TEISEIKBN
                   END-IF
               END-IF
      *             
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      KEY-NEW     NOT =   KEY-OLD
                   MOVE    "2"                 TO  WRK-DATAKBN
                   PERFORM 2001-FILE-SYORI-SEC 
      *
                   IF      WRK-UPDKBN          =   SPACE                   
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT =   1 
                                   OR      KEY-NEW NOT =   KEY-OLD
                           PERFORM 2003-SRYACCT-HENSYU-SEC
      *
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    "key9"              TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-PERFORM
                   ELSE
                       PERFORM 2003-SRYACCT-HENSYU-SEC
      *
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key9"              TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       20012-NYUIN-SRYACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    入院基本料・特定入院料・食事療養編集処理
      *****************************************************************
       20013-NYUIN-KIHON-SEC           SECTION.
      *
      *    統計データ（病名）のみのとき
           IF      WRK-PARA-DATAKBN    =   "2"
               GO  TO  20013-NYUIN-KIHON-EXT
           END-IF
      *
           INITIALIZE                      HCM34S01-AREA
                                           WRK-SPA-AREA
      *
           MOVE    WRK-PARA-HOSPNUM    TO  HCM34S01-HOSPNUM
                                           WRK-SPA-HOSPNUM
           MOVE    PTINF-PTID          TO  HCM34S01-PTID
           MOVE    WRK-PARA-SRYYM      TO  HCM34S01-SANTEIYM
           MOVE    COMB-HKNCOMBINUM    TO  HCM34S01-HKNCOMBI
           MOVE    COMB-HKNNUM         TO  HCM34S01-HKNNUM
           IF    ( COMB-HKNNUM         =   "971" OR "973" )
      *        労災自賠入院会計編集サブ
               CALL    "ORCHCM34S02"       USING HCM34S01-AREA
                                                 WRK-SPA-AREA
           ELSE
      *        医療入院会計編集サブ
               CALL    "ORCHCM34S01"       USING HCM34S01-AREA
                                                 WRK-SPA-AREA
           END-IF
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL   IDX4    >       HCM34S01-HENKYAKU-MAX
      *
             PERFORM VARYING IDX5    FROM    1   BY  1
                     UNTIL ( IDX5                >   20    )
                     OR    ( HCM34S01-SRYCD (IDX4 IDX5)
                                                 =   SPACE )
      *
               MOVE    SPACE               TO  TOUKEI02-REC
               INITIALIZE                      TOUKEI02-REC
      *
      *        診療会計データ編集
               MOVE    TOUKEI01-KEY        TO  TOUKEI02-KEY
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  TOUKEI02-SEQNO
               MOVE    COMB-HKNID          TO  TOUKEI02-HKNID
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       4
                   MOVE    COMB-KOHID (IDX3)   TO  TOUKEI02-TKOHID(IDX3)
               END-PERFORM
               MOVE    HCM34S01-SRYKBN  (IDX4) TO  TOUKEI02-SRYKBN
               MOVE    HCM34S01-ZAITEN  (IDX4) TO  TOUKEI02-ZAITEN
               MOVE    HCM34S01-ZAIKAISU(IDX4) TO  TOUKEI02-ZAIKAISU
               MOVE    HCM34S01-HKNCOMBI       TO  TOUKEI02-HKNCOMBI
               MOVE    HCM34S01-DAY-TBL (IDX4) TO  TOUKEI02-DAY-AREA
      *
      *        診療行為データ編集
               MOVE    HCM34S01-SRYKBN  (IDX4) TO  TOUKEI02-SRYSYUKBN
               MOVE    HCM34S01-SRYCD   (IDX4 IDX5)
                                               TO  TOUKEI02-SRYCD
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    HCM34S01-SRYCD   (IDX4 IDX5)
                                               TO  TNS-SRYCD
grpsys         MOVE    SPA-HOSPNUM             TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TANICD              TO  TOUKEI02-TANICD
               MOVE    TNS-TANINAME            TO  TOUKEI02-TANINAME
               MOVE    TNS-FORMALNAME          TO  TOUKEI02-FORMALNAME
               MOVE    TNS-TENSIKIBETU         TO  TOUKEI02-TENSIKIBETU
               MOVE    TNS-TEN                 TO  TOUKEI02-TEN
               IF      TNS-TENSIKIBETU     =   1   OR   3
                   IF      ACCT-NYUGAIKBN      =   "1"
                       MOVE  TNS-NYUTENTTLKBN  TO  TOUKEI02-TENTTLKBN
                   ELSE
                       MOVE  TNS-GAITENTTLKBN  TO  TOUKEI02-TENTTLKBN
                   END-IF
               END-IF
               MOVE    TNS-NAME                TO  TOUKEI02-NAME
      *
               WRITE   TOUKEI02-REC
      *
               IF      STS-TOUKEI02        =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-TOUKEIERR
                   STRING
                       "月次統計データ（統計情報） 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                           STS-TOUKEI02            DELIMITED  BY  SIZE
                                                   INTO    WRK-TOUKEIERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               ADD     1               TO  CNT-TOUKEI02 
      *
      *        生保単独で別ファイルがあるとき
               IF    ( WRK-SEIHO-TOUKEI01-HOSPNUM )
                                       >   ZERO
               AND   ( WRK-SEIHO-TOUKEI01-SRYYM
                                   NOT =   SPACE  )    
                   MOVE    WRK-SEIHO-TOUKEI01-KEY
                                           TO  TOUKEI02-KEY1
                   WRITE   TOUKEI02-REC
      *
                   IF      STS-TOUKEI02        =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-TOUKEIERR
                       STRING
                       "月次統計データ（統計情報） 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                               STS-TOUKEI02        DELIMITED  BY  SIZE
                                                   INTO    WRK-TOUKEIERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   ADD     1               TO  CNT-TOUKEI02
               END-IF  
             END-PERFORM
           END-PERFORM
           .
       20013-NYUIN-KIHON-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       2001-NYUINGAI-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  WRK-TOUKEI01-TABLE
           INITIALIZE                      WRK-ERR-AREA
      *    テスト患者のときは読み飛ばす
           MOVE    WRK-PARA-HOSPNUM        TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID              TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           DISPLAY "SRYACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *
           IF      FLG-PTINF           =   ZERO
           AND     PTINF-TSTPTNUMKBN   =   "1"
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACCT-READ-SEC
               END-PERFORM
           ELSE
      *        施設入所情報
               PERFORM 500-SISETU-HENSYU-SEC
      *
      *        患者レセプト分割情報情報
               MOVE    KEY-N-PTID              TO  WRK-PTID
               PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
      *            未訂正分があるか
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   IF      KEY-N-HKNCOMBI      =   9999
                       INITIALIZE                  HKNCOMBI-REC
                       MOVE    WRK-PARA-HOSPNUM    TO  COMB-HOSPNUM
                       MOVE    KEY-N-PTID          TO  COMB-PTID
                       MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM
                       MOVE    ZERO                TO  FLG-HKNCOMBI 
                   ELSE
      *                保険組合せ読込み
                       INITIALIZE                  HKNCOMBI-REC
                       MOVE    WRK-PARA-HOSPNUM    TO  COMB-HOSPNUM
                       MOVE    KEY-N-PTID          TO  COMB-PTID
                       MOVE    KEY-N-HKNCOMBI      TO  COMB-HKNCOMBINUM 
                       MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                       PERFORM 910-HKNCOMBI-INV-SEC
      *
                       IF      FLG-HKNCOMBI    =   ZERO
                           IF      COMB-TEKSTYMD   <=  WRK-STYMD
                           AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                               MOVE    SPACE           TO  WRK-UPDKBN
                            ELSE
                               MOVE    "1"             TO  WRK-UPDKBN
                           END-IF
                       ELSE
                           MOVE    WRK-PARA-HOSPNUM
                                                   TO  COMB-HOSPNUM
                           MOVE    KEY-N-PTID      TO  COMB-PTID
                           MOVE    KEY-N-HKNCOMBI  TO  COMB-HKNCOMBINUM 
                           MOVE    "1"             TO  WRK-TEISEIKBN
                       END-IF
                   END-IF    
      *             
                   MOVE    KEY-NEW             TO  KEY-OLD
                   PERFORM         UNTIL   FLG-SRYACCT     =   1 
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       MOVE    "2"                 TO  WRK-DATAKBN
                       PERFORM 2001-FILE-SYORI-SEC 
      *
                       IF      WRK-UPDKBN          =   SPACE                   
      *                    同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                           MOVE    KEY-NEW             TO  KEY-OLD
                           PERFORM     UNTIL   FLG-SRYACCT =   1 
                                       OR      KEY-NEW NOT =   KEY-OLD
                               PERFORM 2003-SRYACCT-HENSYU-SEC
      *
                               MOVE    "tbl_sryacct"   TO  MCP-TABLE
                               MOVE    "key8"          TO  MCP-PATHNAME
                               PERFORM 900-SRYACCT-READ-SEC
                           END-PERFORM
                       ELSE
                           PERFORM 2003-SRYACCT-HENSYU-SEC
      *
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                       END-IF
                   END-PERFORM    
               END-PERFORM         
      *
               IF    ( TBL-PT-HDIST-DAY-MAX
                                           =   ZERO )
               AND   ( TBL-PTKOHINF-MAX    =   ZERO )
                   MOVE    1               TO  ST-IDX
                   MOVE    SPACE           TO  WRK-RECEIPT-KBN
      *            基本ファイル出力処理
                   PERFORM   2002-KIHON-WRITE-SEC
               ELSE
                   MOVE    ZERO            TO  ST-IDX
                   PERFORM         UNTIL   ST-IDX  =   999
                       MOVE    SPACE           TO  WRK-RECEIPT-KBN
                       MOVE    ZERO            TO  ST-IDX
                       PERFORM VARYING IDXX    FROM    1   BY  1
                               UNTIL   IDXX    >       100
                               OR      WRK-TOUKEI01-HOSPNUM (IDXX)
                                               =   ZERO
                           IF      WRK-TOUKEI01-SYOKBN (IDXX)
                                                   =   SPACE
                               MOVE    WRK-TOUKEI01-RECEIPT-KBN (IDXX)
                                                   TO  WRK-RECEIPT-KBN
                               MOVE    IDXX    TO  ST-IDX
                               MOVE    100     TO  IDXX
                           END-IF 
                       END-PERFORM
      *
                       IF      ST-IDX          >   ZERO
      *                    基本ファイル出力処理
                           PERFORM   2002-KIHON-WRITE-SEC
                       ELSE
                           MOVE    999             TO  ST-IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       2001-NYUINGAI-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       2001-FILE-SYORI-SEC              SECTION.
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "1"
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                       MOVE    "1"         TO  WRK-TEISEIKBN
                   END-IF        
               WHEN    "2"
                   IF      WRK-UPDKBN          =   "1"
      *                保険の訂正がされているか
                       IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                           MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX             >=  WRK-DD
                                   OR      WRK-TEISEIKBN   =   "1"
                               IF      ACCT-DAY (1 IDX)    >   ZERO
                                   MOVE    "1"     TO  WRK-TEISEIKBN
                               END-IF
                           END-PERFORM
                       ELSE
                           IF      COMB-TEKSTYMD(1:6)
                                                   >   WRK-STYMD (1:6)
                               MOVE    "1"     TO  WRK-TEISEIKBN
                           END-IF
                       END-IF      
                       IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                           MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                           ADD     1                   TO  WRK-DD
                           PERFORM VARYING IDX FROM    WRK-DD  BY  1
                                   UNTIL   IDX             >   31
                                   OR      WRK-TEISEIKBN   =   "1"
                               IF      ACCT-DAY (1 IDX)    >   ZERO
                                   MOVE    "1"     TO  WRK-TEISEIKBN
                               END-IF
                           END-PERFORM
                       ELSE
                           IF      COMB-TEKEDYMD(1:6)
                                                   <   WRK-EDYMD (1:6)
                               MOVE    "1"         TO  WRK-TEISEIKBN
                           END-IF    
                       END-IF    
                   END-IF
            END-EVALUATE
      *
      *    年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    PTINF-HOSPNUM   TO  AGECHK-HOSPNUM
           MOVE    PTINF-PTID      TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYM  TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *
      *    平成２０年４月以降に老人保険使用のとき
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-KOH1HKNNUM     =   "027"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
           END-IF
      *
      *    平成２０年４月以降に退職国保で６５歳以上のとき
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-HKNNUM         =   "067"                 )
               IF      AGECHK-NENREI       >=  65
                   MOVE    "1"                 TO  WRK-TEISEIKBN
               END-IF
          END-IF
      *
      *    平成２２年４月以降に政府管掌を使用のとき
           IF    ( WRK-TEISEIKBN       =   SPACE                 )
           AND   ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201004 )
           AND   ( COMB-HKNNUM         =   "001"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
           END-IF
      *
      *   主保険情報読み出し
           INITIALIZE                      PTHKNINF-REC
                                           PTRSIINF-REC
           IF      COMB-HKNID          NOT =   ZERO
               EVALUATE    COMB-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                       INITIALIZE                  PTRSIINF-REC
                       MOVE    COMB-HOSPNUM        TO  PTRSI-HOSPNUM
                       MOVE    COMB-HKNID          TO  PTRSI-HKNID
                       MOVE    COMB-PTID           TO  PTRSI-PTID
                       MOVE    PTRSIINF-REC        TO  MCPDATA-REC
                       PERFORM 900-PTRSIINF-INV-SEC
                   WHEN    OTHER
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
                       MOVE    COMB-PTID           TO  PTHKN-PTID
                       MOVE    COMB-HKNID          TO  PTHKN-HKNID
                       MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                                       PTHKN-TEKEDYMD
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       PERFORM 910-PTHKNINF-INV-SEC
               END-EVALUATE 
           END-IF
      *
      *    ファイル編集処理
           INITIALIZE                      TOUKEI01-REC
           MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI01-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  TOUKEI01-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  TOUKEI01-NYUGAIKBN
           MOVE    PTINF-PTID          TO  TOUKEI01-PTID
           MOVE    COMB-HKNNUM         TO  TOUKEI01-HKNNUM
           MOVE    PTHKN-HONKZKKBN     TO  TOUKEI01-HONKZKKBN
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  TOUKEI01-KOHID
           ELSE
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE    COMB-KOH1ID         TO  TOUKEI01-KOHID
                   MOVE    "0"                 TO  TOUKEI01-HONKZKKBN
               ELSE
                   MOVE    ZERO                TO  TOUKEI01-KOHID
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002" 
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                             END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "0" TO  TOUKEI01-HONKZKKBN
                              END-EVALUATE
                      WHEN    "040" 
                      WHEN    "039" 
                          EVALUATE   PTHKN-HOJOKBN
                              WHEN   "1"
                              WHEN   "3"
                                 MOVE    "0"   TO  TOUKEI01-HONKZKKBN
                          END-EVALUATE             
                      WHEN    OTHER 
                          EVALUATE   PTHKN-HOJOKBN
                             WHEN   "9"
                                 MOVE    "0" TO  TOUKEI01-HONKZKKBN
                             WHEN   "8"
                                 MOVE    "0" TO  TOUKEI01-HONKZKKBN
                             WHEN   "7"
                                 MOVE    "0" TO  TOUKEI01-HONKZKKBN
                         END-EVALUATE
                   END-EVALUATE
               END-IF 
           END-IF
      *
           MOVE    PTHKN-HKNJANUM      TO  TOUKEI01-HKNJANUM
           IF    ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOH1HKNNUM     =   "020"
                                       OR  "030"
                                       OR  "053" )
      *********MOVE    COMB-KOH1HKNNUM     TO  TOUKEI01-HKNJANUM
               MOVE    COMB-TEKSTYMD       TO  TOUKEI01-HKNJANUM
               MOVE    COMB-KOH1ID         TO  TOUKEI01-KOH-KOHID
           END-IF
      *
           IF      WRK-PARA-SRYYM      <=   "200609"
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO
                                               TOUKEI01-HOJOKBN
                   END-IF
               WHEN    "060"
               WHEN    "068"
                   EVALUATE    WRK-PARA-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0"             TO
                                                   TOUKEI01-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1"             TO
                                                   TOUKEI01-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2"             TO
                                                   TOUKEI01-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1"             TO
                                                   TOUKEI01-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2"             TO
                                                   TOUKEI01-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3"             TO
                                                   TOUKEI01-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                      WHEN   "9"
                      WHEN   "8"
                          MOVE    PTHKN-HOJOKBN   TO
                                               TOUKEI01-HOJOKBN
                   END-EVALUATE
             END-EVALUATE             
           ELSE
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                       OR    ( AGECHK-NENREI <=  74                    )  
                           MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-HOJOKBN
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                               WHEN    "D"
                                   MOVE    "1" TO  TOUKEI01-HOJOKBN
                               WHEN    "B"
                               WHEN    "H"
                               WHEN    "E"
                                   MOVE    "2" TO  TOUKEI01-HOJOKBN
                               WHEN    "C"
                               WHEN    "I"
                               WHEN    "F"
                                   MOVE    "3" TO  TOUKEI01-HOJOKBN
                               WHEN    OTHER
                                   MOVE    PTHKN-HOJOKBN
                                               TO  TOUKEI01-HOJOKBN
                           END-EVALUATE
                       END-IF
                   END-IF
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-HOJOKBN
                   END-IF
               WHEN    "060" 
               WHEN    "068" 
                   EVALUATE    WRK-PARA-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0" TO  TOUKEI01-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1" TO  TOUKEI01-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2" TO  TOUKEI01-HOJOKBN
                               WHEN   "3"
                                   MOVE    "3" TO  TOUKEI01-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1" TO  TOUKEI01-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2" TO  TOUKEI01-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3" TO  TOUKEI01-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    "040" 
               WHEN    "039" 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "1"
                       WHEN   "3"
                           MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-HOJOKBN
                   END-EVALUATE             
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                       WHEN   "7"
                           MOVE    PTHKN-HOJOKBN
                                               TO  TOUKEI01-HOJOKBN
                   END-EVALUATE             
             END-EVALUATE
           END-IF             
      *
           IF      COMB-HKNCOMBINUM    =    9999
               MOVE    "9999999"            TO  TOUKEI01-HKNJANUM
           END-IF
      *
      *    施設入所の有無
           IF    ( TOUKEI01-RECEIPT-KBN (1:1)
                                           =   SPACE )
           AND   ( TBL-PTKOHINF-MAX        >   ZERO  )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       TBL-PTKOHINF-MAX
                   IF    ( TBL-PTKOH-TEKSTYMD (IDX1)
                                           <=  COMB-TEKSTYMD           )
                   AND   ( COMB-TEKEDYMD   <=  TBL-PTKOH-TEKEDYMD(IDX1))
                       IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "950"
                           MOVE    "1"     TO  TOUKEI01-RECEIPT-KBN
                                                                   (1:1)
                       END-IF
                       IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "951"
                           MOVE    "2"     TO  TOUKEI01-RECEIPT-KBN
                                                                   (1:1)
                       END-IF
                       IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "948"
                           MOVE    "3"     TO  TOUKEI01-RECEIPT-KBN
                                                                   (1:1)
                       END-IF
                       IF      TBL-PTKOH-KOHNUM (IDX1)
                                               =   "949"
                           MOVE    "4"     TO  TOUKEI01-RECEIPT-KBN
                                                                   (1:1)
                       END-IF
                       MOVE    TBL-PTKOHINF-MAX
                                           TO  IDX1
                   END-IF
               END-PERFORM
           END-IF
      *
      *    患者レセプト分割情報の有無
           IF    ( TOUKEI01-RECEIPT-KBN (2:3)
                                           =   SPACE )
           AND   ( TBL-PT-HDIST-DAY-MAX    >   ZERO  )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1    >       TBL-PT-HDIST-DAY-MAX
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL   IDX4    >       2
                       PERFORM VARYING IDX5    FROM    1   BY  1
                               UNTIL   IDX5    >       10
                               OR      TBL-PT-HDIST-DAY-HKNCOMBI
                                                       (IDX1 IDX4 IDX5)
                                               =   ZERO
                         IF      TBL-PT-HDIST-DAY-DLTFLG
                                                       (IDX1 IDX4 IDX5)
                                                       =   "1"
                           CONTINUE
                         ELSE
                           IF      COMB-HKNCOMBINUM    =
                                   TBL-PT-HDIST-DAY-HKNCOMBI
                                                       (IDX1 IDX4 IDX5)
                               MOVE    TBL-PT-HDIST-DAY-DAY (IDX1)
                                                       TO
                                               TOUKEI01-RECEIPT-KBN(2:2)
                               IF      IDX4            =   1
                                   MOVE    "1"         TO
                                               TOUKEI01-RECEIPT-KBN(4:1)
                               ELSE
                                   MOVE    "2"         TO
                                               TOUKEI01-RECEIPT-KBN(4:1)
                               END-IF
                               IF    ( TBL-PT-HDIST-DAY-SRYYM   (IDX1)
                                           =   COMB-TEKSTYMD (1:6) AND
                                       TBL-PT-HDIST-DAY-DAY (IDX1)
                                           =   COMB-TEKSTYMD (7:2)     )
                               OR    ( TBL-PT-HDIST-DAY-SRYYM   (IDX1)
                                           =   COMB-TEKEDYMD (1:6) AND
                                       TBL-PT-HDIST-DAY-DAY (IDX1)
                                           =   COMB-TEKEDYMD (7:2)     )
                                   MOVE    10              TO  IDX5
                                   MOVE    2               TO  IDX4
                                   MOVE    TBL-PT-HDIST-DAY-MAX
                                                           TO  IDX1   
                               END-IF
                           END-IF
                         END-IF 
                       END-PERFORM
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           PERFORM VARYING IDXX    FROM   1   BY  1
                   UNTIL   IDXX    >      WRK-TOUKEI01-TABLE-MAX
               IF      WRK-TOUKEI01-HOSPNUM (IDXX) =   ZERO
                   PERFORM 20011-TOUKEI01-WRITE-SEC
                   MOVE    WRK-TOUKEI01-TABLE-MAX
                                              TO  IDXX
               ELSE
                   IF      WRK-TOUKEI01-KEY (IDXX) =
                                               TOUKEI01-KEY
                       PERFORM 20011-TOUKEI01-REWRITE-SEC
                       MOVE    WRK-TOUKEI01-TABLE-MAX
                                                   TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
      *
           INITIALIZE                  WRK-SEIHO-TOUKEI01-REC
      *    公費単独の場合（生保、中国残留邦人等、自立支援）
           IF    ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOHHKNNUM (1)  =   "012"
                                       OR  "015"
                                       OR  "016"
                                       OR  "021"
                                       OR  "025" )
               IF      ( COMB-KOHHKNNUM (2)  =   SPACE )
                   PERFORM 20013-SEIHO-TOUKEI01-WRITE-SEC
               ELSE
      *            沖縄県で精神障害者特別措置(121)の場合も対応
                   IF    ( SYS-1001-PREFNUM    =   "47"  )
                   AND   ( COMB-KOHHKNNUM (2)  =   "121" )
                       PERFORM 20013-SEIHO-TOUKEI01-WRITE-SEC
                   END-IF
               END-IF
           END-IF
           .
       2001-FILE-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    追加処理
      *****************************************************************
       20011-TOUKEI01-WRITE-SEC              SECTION.
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
           IF      TOUKEI01-HKNNUM NOT =   SPACE
               EVALUATE    TOUKEI01-HKNNUM
                 WHEN    WRK-CONS-ROUSAI-HKNNUM
                 WHEN    WRK-CONS-JIBAI-HKNNUM
      *            労災情報 
                   MOVE   PTRSI-HKNID      TO  TOUKEI01-HKNID
                   MOVE   PTRSI-HKNKBN     TO  TOUKEI01-HKNKBN
                   MOVE   PTRSI-KOFUNUM    TO  TOUKEI01-KOFUNUM
                   MOVE   PTRSI-JIGYOUNAME TO  TOUKEI01-JIGYOUNAME
                   MOVE   PTRSI-PREFNAME   TO  TOUKEI01-PREFNAME
                   MOVE   PTRSI-PREFKBN    TO  TOUKEI01-PREFKBN  
                   MOVE   PTRSI-CITYNAME   TO  TOUKEI01-CITYNAME
                   MOVE   PTRSI-CITYKBN    TO  TOUKEI01-CITYKBN
      *
                   MOVE   PTRSI-SINKEI     TO  TOUKEI01-SINKEI
                   EVALUATE  PTRSI-HKNKBN 
                       WHEN    "1"  
                           IF   ( PTRSI-SINKEI = "1" OR "3" OR "7" )
                           AND  ( PTRSI-RYOSTYMD(1:6) 
                                               <  WRK-PARA-SRYYM )
                               MOVE   "5"      TO  TOUKEI01-SINKEI
                           END-IF
                       WHEN    "2"  
                           IF   ( PTRSI-SINKEI = "1" OR "7"        )
                           AND  ( PTRSI-RYOSTYMD(1:6) 
                                               <  WRK-PARA-SRYYM   )
                               MOVE   "5"      TO  TOUKEI01-SINKEI
                           END-IF
                   END-EVALUATE
      *
                   MOVE   PTRSI-TENKI      TO  TOUKEI01-TENKI
                   MOVE   PTRSI-SHOBYOYMD  TO  TOUKEI01-SHOBYOYMD
                   MOVE   PTRSI-SISIKBN    TO  TOUKEI01-SISIKBN
                   MOVE   PTRSI-RYOSTYMD   TO  TOUKEI01-RYOSTYMD
                   MOVE   PTRSI-RYOEDYMD   TO  TOUKEI01-RYOEDYMD
                   MOVE   PTRSI-SONSHOUKBN TO  TOUKEI01-SONSHOUKBN
                   MOVE   PTRSI-SONSHOUKBN-EDANUM
                                           TO
                                           TOUKEI01-SONSHOUKBN-EDANUM 
                   MOVE   PTRSI-SAIGAIKBN  TO  TOUKEI01-SAIGAIKBN 
                   MOVE   PTRSI-SINSATUYMD TO  TOUKEI01-SINSATUYMD
                   MOVE   PTRSI-SINSATUEND TO  TOUKEI01-SINSATUEND 
                   MOVE   PTRSI-JIBAISEIKBN
                                           TO  TOUKEI01-JIBAISEIKBN
                   MOVE   PTRSI-HIHKNJANAME
                                           TO  TOUKEI01-HIHKNJANAME
                 WHEN    OTHER
      *            主保険情報 
                   MOVE    PTHKN-HKNID     TO  TOUKEI01-HKNID
                                               TOUKEI01-THKNID     (1)
                   MOVE    PTHKN-TEKSTYMD  TO  TOUKEI01-TTEKSTYMD  (1)
                   MOVE    PTHKN-TEKEDYMD  TO  TOUKEI01-TTEKEDYMD  (1)
                   MOVE    PTHKN-SKKGETYMD TO  TOUKEI01-TSKKGETYMD (1)
      ****         MOVE    PTHKN-HKNJANUM  TO  TOUKEI01-THKNJANUM  (1)
                   MOVE    PTHKN-KIGO      TO  TOUKEI01-TKIGO      (1)
                   MOVE    PTHKN-NUM       TO  TOUKEI01-TNUM       (1)
                   MOVE    PTHKN-CONTKBN   TO  TOUKEI01-TCONTKBN   (1)
                   MOVE    PTHKN-HIHKNJANAME
                                           TO  TOUKEI01-THIHKNJANAME(1)
                   MOVE    PTHKN-HOJOKBN   TO  TOUKEI01-THOJOKBN   (1)
                   IF      PTHKN-HKNNUM    =   "002"
                       IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                       OR    ( AGECHK-NENREI <=  74                    )  
                           CONTINUE
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                               WHEN    "D"
                                   MOVE    "1" TO  TOUKEI01-THOJOKBN (1)
                               WHEN    "B"
                               WHEN    "H"
                               WHEN    "E"
                                   MOVE    "2" TO  TOUKEI01-THOJOKBN (1)
                               WHEN    "C"
                               WHEN    "I"
                               WHEN    "F"
                                   MOVE    "3" TO  TOUKEI01-THOJOKBN (1)
                           END-EVALUATE
                       END-IF
                   END-IF
                   MOVE    PTHKN-HONKZKKBN TO  TOUKEI01-THONKZKKBN (1)
      *            法別番号
                   INITIALIZE                      HKNNUM-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  HKN-HOSPNUM
                   MOVE    PTHKN-HKNNUM        TO  HKN-HKNNUM
                   MOVE    ZERO                TO  HKN-PAYKBN   
                   MOVE    WRK-SYMD            TO  HKN-TEKSTYMD
                                                   HKN-TEKEDYMD
                   MOVE    HKNNUM-REC          TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM          =   ZERO
                       MOVE    HKN-HBTNUM      TO  TOUKEI01-HKNHBTNUM
                       MOVE    HKN-HKNKOHSBTKBN
                                               TO  TOUKEI01-HKNKOHSBTKBN
                   END-IF
               END-EVALUATE
           END-IF    
      *
      *    老人保険情報
           IF      TOUKEI01-KOHID  >   ZERO   
               MOVE    COMB-KOH1HKNNUM TO  TOUKEI01-RJNHKNNUM
      *
               MOVE    WRK-PARA-HOSPNUM
                                       TO  PTKOH-HOSPNUM
               MOVE    PTINF-PTID      TO  PTKOH-PTID
               MOVE    TOUKEI01-KOHID  TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC    TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               IF       FLG-PTKOHINF       =   ZERO
                   MOVE    PTKOH-PAYKBN        TO  TOUKEI01-RJNPAYKBN
                   MOVE    PTKOH-FTNJANUM      TO  TOUKEI01-RJNFTNJANUM
                   MOVE    PTKOH-JKYSNUM       TO  TOUKEI01-RJNJKYSNUM
                   MOVE    PTKOH-TEKSTYMD      TO  TOUKEI01-RJNTEKSTYMD
                   MOVE    PTKOH-TEKEDYMD      TO  TOUKEI01-RJNTEKEDYMD
      *            法別番号のセット
                   INITIALIZE                  HKNNUM-REC
                   MOVE    WRK-PARA-HOSPNUM
                                           TO  HKN-HOSPNUM
                   MOVE    PTKOH-KOHNUM    TO  HKN-HKNNUM
                   MOVE    PTKOH-PAYKBN    TO  HKN-PAYKBN   
                   MOVE    WRK-SYMD        TO  HKN-TEKSTYMD
                                               HKN-TEKEDYMD
                   MOVE    HKNNUM-REC      TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-CHK-SEC
                   IF      FLG-HKNNUM      =   ZERO
                       MOVE   HKN-HBTNUM       TO TOUKEI01-RJNHBTNUM
                   END-IF
               END-IF
           END-IF
      *
      *    患者番号
           INITIALIZE                  ORCSPTIDAREA
           MOVE    TOUKEI01-HOSPNUM
                                   TO  SPTID-HOSPNUM
           MOVE    TOUKEI01-PTID   TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM     TO  TOUKEI01-PTNUM
      *
      *    患者情報
           MOVE    PTINF-KANANAME  TO  TOUKEI01-KANANAME
           MOVE    PTINF-NAME      TO  TOUKEI01-NAME
           MOVE    PTINF-SEX       TO  TOUKEI01-SEX
           MOVE    PTINF-BIRTHDAY  TO  TOUKEI01-BIRTHDAY
           MOVE    PTINF-HOME-POST TO  TOUKEI01-HOME-POST
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                                       INTO        TOUKEI01-HOME-ADRS
           END-STRING
           MOVE    PTINF-HOME-TEL1 TO  TOUKEI01-HOME-TEL1
           INITIALIZE                  ADRS-REC
           MOVE    PTINF-HOME-POST TO  ADRS-POST
grpsys     MOVE    SPA-HOSPNUM     TO  ADRS-HOSPNUM
           MOVE    ADRS-REC        TO  MCPDATA-REC
           PERFORM 900-ADRS-INV-SEC
           MOVE    ADRS-LPUBCD     TO  TOUKEI01-LPUBCD
      *
      *    旧姓履歴の取得
           INITIALIZE                  KYUSEIRRK-REC
           MOVE    TOUKEI01-HOSPNUM
                                   TO  KYUSEI-HOSPNUM
           MOVE    TOUKEI01-PTID   TO  KYUSEI-PTID
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM  TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC   TO  MCPDATA-REC
           PERFORM 900-KYUSEIRRK-READ-SEC
           IF       FLG-KYUSEIRRK  =   ZERO
               MOVE    KYUSEI-NAME     TO  TOUKEI01-NAME
               MOVE    KYUSEI-KANANAME TO  TOUKEI01-KANANAME
           END-IF
      *
      *    年齢
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    TOUKEI01-HOSPNUM
                                   TO  AGECHK-HOSPNUM
           MOVE    TOUKEI01-PTID   TO  AGECHK-PTID
           MOVE    WRK-PARA-SRYYM  TO  AGECHK-SRYYMD(1:6)
           MOVE    "01"            TO  AGECHK-SRYYMD(7:2)
           MOVE    PTINF-BIRTHDAY  TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
grpsys                                 SPA-AREA
           IF      AGECHK-RC       =   ZERO
               MOVE  AGECHK-NENREI     TO  TOUKEI01-AGE
               MOVE  AGECHK-SYUGAKUKBN TO  TOUKEI01-SYUGAKUKBN
           END-IF
      *
           MOVE    COMB-HKNCOMBINUM
                                   TO  TOUKEI01-HKNCOMBI   (1)
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "2"    
                   MOVE    ACCT-SRYKA      TO  TOUKEI01-SRYKA      (1)
           END-EVALUATE
      *
           MOVE    WRK-TEISEIKBN   TO  TOUKEI01-TEISEIKBN
      *
           MOVE    TOUKEI01-REC    TO  WRK-TOUKEI01-OCC (IDXX)
           .
       20011-TOUKEI01-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更処理
      *****************************************************************
       20011-TOUKEI01-REWRITE-SEC            SECTION.
      *
           MOVE    WRK-TOUKEI01-OCC (IDXX) TO  TOUKEI01-REC
      *
           PERFORM 20012-KOHID-CHECK-SEC
      *
      *    主保険情報 
           IF      TOUKEI01-HKNNUM     NOT =   SPACE
             EVALUATE    TOUKEI01-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
               WHEN    WRK-CONS-JIBAI-HKNNUM
                 CONTINUE
               WHEN    OTHER
                 MOVE    1                       TO  FLG-LASTHKNID
                 PERFORM VARYING IDX FROM    1   BY  1
                         UNTIL   IDX >       5
                     IF    ( TOUKEI01-TTEKSTYMD (IDX)
                                       =   PTHKN-TEKSTYMD )
                     AND   ( TOUKEI01-TTEKEDYMD (IDX)
                                       =   PTHKN-TEKEDYMD )
                         MOVE    5              TO  IDX
                     ELSE
                         IF      TOUKEI01-TTEKSTYMD (IDX)  =   SPACE
                             MOVE    PTHKN-HKNID
                                           TO  TOUKEI01-THKNID    (IDX)
                             MOVE    PTHKN-TEKSTYMD
                                           TO  TOUKEI01-TTEKSTYMD (IDX)
                             MOVE    PTHKN-TEKEDYMD
                                           TO  TOUKEI01-TTEKEDYMD (IDX)
                             MOVE    PTHKN-SKKGETYMD
                                           TO  TOUKEI01-TSKKGETYMD(IDX)
                             MOVE    PTHKN-KIGO
                                           TO  TOUKEI01-TKIGO     (IDX)
                             MOVE    PTHKN-NUM
                                           TO  TOUKEI01-TNUM      (IDX)
                             MOVE    PTHKN-CONTKBN
                                           TO  TOUKEI01-TCONTKBN  (IDX)
                             MOVE    PTHKN-HIHKNJANAME
                                           TO
                                           TOUKEI01-THIHKNJANAME  (IDX)
                             MOVE    PTHKN-HOJOKBN
                                           TO  TOUKEI01-THOJOKBN  (IDX)
                             IF      PTHKN-HKNNUM    =   "002"
                                 IF    ( WRK-PARA-SRYYM
                                             <   WRK-CONS-SRYYM-200804 )
                                 OR    ( AGECHK-NENREI <=  74          )  
                                     CONTINUE
                                 ELSE
                                     EVALUATE    PTHKN-HOJOKBN
                                       WHEN    "A"
                                       WHEN    "G"
                                       WHEN    "D"
                                         MOVE    "1" 
                                           TO  TOUKEI01-THOJOKBN (IDX)
                                       WHEN    "B"
                                       WHEN    "H"
                                       WHEN    "E"
                                         MOVE    "2"
                                           TO  TOUKEI01-THOJOKBN (IDX)
                                       WHEN    "C"
                                       WHEN    "I"
                                       WHEN    "F"
                                         MOVE    "3"
                                           TO  TOUKEI01-THOJOKBN (IDX)
                                     END-EVALUATE
                                 END-IF
                             END-IF
                             MOVE    PTHKN-HONKZKKBN
                                           TO  TOUKEI01-THONKZKKBN(IDX)
                             MOVE    5     TO  IDX
                         ELSE
                             IF      TOUKEI01-TTEKSTYMD (IDX)
                                                   >=  PTHKN-TEKSTYMD
                                 MOVE    ZERO   TO  FLG-LASTHKNID
                             END-IF    
                         END-IF
                     END-IF    
                 END-PERFORM
      *    
                 IF      FLG-LASTHKNID       =   1
                     MOVE    PTHKN-HKNID         TO  TOUKEI01-HKNID
                 END-IF
             END-EVALUATE
           END-IF
      *    
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      TOUKEI01-HKNCOMBI    (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       TOUKEI01-HKNCOMBI   (IDX)
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       TOUKEI01-HKNCOMBI   (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           EVALUATE    WRK-DATAKBN
               WHEN    "2"    
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       20
                       IF      TOUKEI01-SRYKA    (IDX) =   SPACE
                           MOVE    ACCT-SRYKA  TO  TOUKEI01-SRYKA (IDX)
                           MOVE    20          TO  IDX
                       ELSE
                           IF      TOUKEI01-SRYKA  (IDX)
                                                       =   ACCT-SRYKA
                               MOVE    20          TO  IDX
                           END-IF
                       END-IF    
                   END-PERFORM
           END-EVALUATE 
      *
           IF      WRK-TEISEIKBN           =   "1"
               MOVE    "1"                 TO  TOUKEI01-TEISEIKBN     
           END-IF          
      *
           MOVE    TOUKEI01-REC    TO  WRK-TOUKEI01-OCC (IDXX)
           .
       20011-TOUKEI01-REWRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費番号チェック処理
      *****************************************************************
       20012-KOHID-CHECK-SEC              SECTION.
      *
      *    老人保険以外の公費情報を編集
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   4
                   OR      COMB-KOHID (IDX)    =   ZERO
               IF      COMB-KOHHKNNUM (IDX)    =   "027"
                   CONTINUE
               ELSE
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY         >   10
                       IF      COMB-KOHID (IDX)
                                           =   TOUKEI01-TKOHID (IDY)
                           MOVE    10              TO  IDY
                       ELSE  
                           IF      TOUKEI01-TKOHID (IDY)   =   ZERO
                               PERFORM 200121-KOHID-HENSYU-SEC
                               MOVE    10              TO  IDY
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20012-KOHID-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費番号編集処理
      *****************************************************************
       200121-KOHID-HENSYU-SEC                SECTION.
      *
           MOVE    COMB-KOHID (IDX)    TO  TOUKEI01-TKOHID    (IDY)
           MOVE    COMB-KOHHKNNUM(IDX) TO  TOUKEI01-TKOHHKNNUM(IDY)
           MOVE    COMB-KOHPAYKBN(IDX) TO  TOUKEI01-TKOHPAYKBN(IDY)
    *
           MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
           MOVE    TOUKEI01-PTID       TO  PTKOH-PTID
           MOVE    COMB-KOHID (IDX)    TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTKOHINF-READ-SEC
           IF       FLG-PTKOHINF       =   ZERO
               MOVE   PTKOH-FTNJANUM   TO  TOUKEI01-TKOHFTNJANUM (IDY)
               MOVE   PTKOH-TEKSTYMD   TO  TOUKEI01-TKOHTEKSTYMD (IDY)
               MOVE   PTKOH-TEKEDYMD   TO  TOUKEI01-TKOHTEKEDYMD (IDY)
               MOVE   PTKOH-JKYSNUM    TO  TOUKEI01-TKOHJKYSNUM  (IDY)
      *             
      *        生保のとき月代わり受給者番号マスタより受給者番号をセット 
               IF    ( PTKOH-KOHNUM    =   WRK-CONS-SEIHO-HKNNUM )
                   MOVE    WRK-PARA-HOSPNUM
                                           TO  MONTHLYNUM-HOSPNUM
                   MOVE    TOUKEI01-PTID   TO  MONTHLYNUM-PTID
                   MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                   MOVE    WRK-PARA-SRYYM  TO  MONTHLYNUM-SRYYM
                   MOVE    WRK-PARA-NYUGAIKBN
                                           TO  MONTHLYNUM-NYUGAIKBN
                   MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                   MOVE    "tbl_monthlynum"
                                           TO  WRK-MCP-TABLE
                   MOVE    "key4"          TO  WRK-MCP-PATHNAME
                   PERFORM 910-MONTHLYNUM-READ-SEC
                   IF      FLG-MONTHLYNUM  =   ZERO
                       MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  TOUKEI01-TKOHJKYSNUM(IDY)
                   ELSE
                       MOVE    WRK-PARA-HOSPNUM
                                               TO  MONTHLYNUM-HOSPNUM
                       MOVE    TOUKEI01-PTID   TO  MONTHLYNUM-PTID
                       MOVE    ZERO            TO  MONTHLYNUM-KOHID
                       MOVE    WRK-PARA-SRYYM  TO  MONTHLYNUM-SRYYM
                       MOVE    WRK-PARA-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       MOVE    "tbl_monthlynum"
                                               TO  WRK-MCP-TABLE
                       MOVE    "key4"          TO  WRK-MCP-PATHNAME
                       PERFORM 910-MONTHLYNUM-READ-SEC
                       IF      FLG-MONTHLYNUM  =   ZERO
                           MOVE    MONTHLYNUM-JKYSNUM 
                                       TO  TOUKEI01-TKOHJKYSNUM  (IDY)
                       END-IF
                   END-IF 
               ELSE                       
                   IF      PTKOH-KOHNUM    =   WRK-CONS-ZANRYUU-HKNNUM
                       MOVE    WRK-PARA-HOSPNUM
                                               TO  MONTHLYNUM-HOSPNUM
                       MOVE    TOUKEI01-PTID   TO  MONTHLYNUM-PTID
                       MOVE    PTKOH-KOHID     TO  MONTHLYNUM-KOHID
                       MOVE    WRK-PARA-SRYYM  TO  MONTHLYNUM-SRYYM
                       MOVE    WRK-PARA-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       MOVE    "tbl_monthlynum"
                                               TO  WRK-MCP-TABLE
                       MOVE    "key4"          TO  WRK-MCP-PATHNAME
                       PERFORM 910-MONTHLYNUM-READ-SEC
                       IF      FLG-MONTHLYNUM  =   ZERO
                           MOVE    MONTHLYNUM-JKYSNUM 
                                           TO  TOUKEI01-TKOHJKYSNUM(IDY)
                       END-IF
                   END-IF
               END-IF
      *    
      *        法別番号のセット
               INITIALIZE                 HKNNUM-REC
               MOVE    WRK-PARA-HOSPNUM
                                       TO HKN-HOSPNUM
               MOVE    PTKOH-KOHNUM    TO HKN-HKNNUM
               MOVE    PTKOH-PAYKBN    TO HKN-PAYKBN   
               MOVE    WRK-SYMD        TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
               MOVE    HKNNUM-REC      TO  MCPDATA-REC
               PERFORM 900-HKNNUM-CHK-SEC
               IF      FLG-HKNNUM      =   ZERO
                    MOVE   HKN-HBTNUM      TO  TOUKEI01-TKOHHBTNUM (IDY)
               END-IF
      *        地方公費のときレセプト請求区分を取得
               IF    ( COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8" )
               OR    ( COMB-KOHHKNNUM (IDX)(1:2)
                                           =  "99"             )
                   EVALUATE    COMB-HKNNUM
                       WHEN    "060"
                       WHEN    "067" 
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                                                               OR  "5"
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       WHEN    "039"                
                           IF      HKN-RECESKYKBN      =   "1" OR  "3"
                                                               OR  "4"
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       WHEN    OTHER                
                           IF      HKN-RECESKYKBN      =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           ELSE
                               MOVE    "1" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                   END-EVALUATE
                   IF     TOUKEI01-TRECESKYKBN (IDY)   =   1
      *                地方公費レセ記載判定
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    WRK-PARA-NYUGAIKBN
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-PARA-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
grpsys                                         SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE       "2"  TO  TOUKEI01-TRECESKYKBN(IDY)
                       END-IF
                   END-IF
               ELSE
      *            全国公費のとき（長期）
                   IF      HKN-HKNKOHSBTKBN    =   "2" 
                       GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            全国公費のとき（生保）
      **>          IF   COMB-KOHHKNNUM (IDX)   =   "012" OR "021"
      *                レセ記載判定
                       INITIALIZE          ORCSFTNJGNCHKAREA
                       MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    WRK-PARA-NYUGAIKBN
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    WRK-PARA-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    SYS-1001-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK" USING
                                               ORCSFTNJGNCHKAREA
grpsys                                         SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE    "2"     TO  TOUKEI01-TRECESKYKBN(IDY)
                       ELSE
                           MOVE    "1"     TO  TOUKEI01-TRECESKYKBN(IDY)
                       END-IF
      **>              GO  TO  200121-KOHID-HENSYU-EXT
      **>          END-IF
      *            全国公費のとき（老人原爆）
                   IF    ( TOUKEI01-RJNHKNNUM
                                       NOT =   SPACE                   )
                   AND   ( COMB-KOHHKNNUM (IDX)
                                           =   WRK-CONS-GENBAKU-HKNNUM )
                   AND   ( TOUKEI01-TRECESKYKBN(IDY)  =  "1"           )
                       IF      TOUKEI01-HKNNUM =   "060"   OR  "067"
                           IF      SYS-2005-RJNGNBKKBN =   "1" OR  ZERO
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       ELSE                
                           IF      SYS-2005-RJNGNBKKBN =   "2" OR  ZERO
                               MOVE    "2" TO  TOUKEI01-TRECESKYKBN(IDY)
                           END-IF
                       END-IF
      **>              GO  TO  200121-KOHID-HENSYU-EXT
                   END-IF
      *            全国公費のとき（長期・老人原爆・生保・精神以外）
      **>          MOVE    "1"         TO  TOUKEI01-TRECESKYKBN(IDY)
               END-IF       
           END-IF
           .
       200121-KOHID-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理（生保単独のときのみ）
      *****************************************************************
       20013-SEIHO-TOUKEI01-WRITE-SEC              SECTION.
      *
           MOVE    TOUKEI01-REC        TO  WRK-SEIHO-TOUKEI01-REC
      *
           INITIALIZE                      TOUKEI01-REC
           MOVE    WRK-SEIHO-TOUKEI01-KEY
                                       TO  TOUKEI01-KEY
           MOVE    COMB-TEKSTYMD       TO  TOUKEI01-HKNJANUM
      *    生活保護期間重複対応のため
           IF      COMB-KOHHKNNUM (1)  =   "012"
               MOVE    COMB-KOH1ID         TO  TOUKEI01-KOH-KOHID
           END-IF
      *
           PERFORM VARYING IDXX    FROM   1   BY  1
                   UNTIL   IDXX    >      WRK-TOUKEI01-TABLE-MAX
               IF      WRK-TOUKEI01-HOSPNUM (IDXX) =   ZERO
                   PERFORM 20011-TOUKEI01-WRITE-SEC
                   MOVE    WRK-TOUKEI01-TABLE-MAX
                                              TO  IDXX
               ELSE
                   IF      WRK-TOUKEI01-KEY (IDXX) =
                                               TOUKEI01-KEY
                       PERFORM 20011-TOUKEI01-REWRITE-SEC
                       MOVE    WRK-TOUKEI01-TABLE-MAX
                                                   TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20013-SEIHO-TOUKEI01-WRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    福岡県公費チェック処理
      *****************************************************************
       20011-PREFKBN-40-CHECK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-PREFKBN
                                           WRK-PREFNUM
      *
      *    県外扱いの保険者はレセプトへ記載しない
           IF      PTHKN-HKNJANUM (1:2)    =   "67"
               MOVE    PTHKN-HKNJANUM (3:2)
                                       TO  WRK-PREFNUM
           ELSE
               MOVE    PTHKN-HKNJANUM (1:2)
                                       TO  WRK-PREFNUM
           END-IF
           IF      WRK-PREFNUM         =   SYS-1001-PREFNUM
               MOVE    "1"                 TO  WRK-PREFKBN
           ELSE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       5
                       OR      WRK-1033-STYUKYMD (IDX2) =   SPACE
                   IF      WRK-1033-STYUKYMD   (IDX2)  <=  WRK-SYMD
                   AND     WRK-1033-EDYUKYMD   (IDX2)  >=  WRK-SYMD      
                       PERFORM VARYING IDX3 FROM    1   BY  1
                               UNTIL   IDX3 >       20
                               OR      WRK-1033-HKNJANUM (IDX2 IDX3)
                                                   =   SPACE
                               OR      WRK-PREFKBN =   "1"     
                           IF      PTHKN-HKNJANUM = 
                                           WRK-1033-HKNJANUM (IDX2 IDX3)
                               MOVE    "1"     TO  WRK-PREFKBN
                           END-IF
                       END-PERFORM
                   END-IF            
               END-PERFORM
           END-IF
      *        
      *    県内扱いの場合、公費付加情報よりレセプト請求区分を取得
           IF      WRK-PREFKBN         =   SPACE
               MOVE    "2"                 TO  TOUKEI01-TRECESKYKBN(IDY)
           ELSE
               INITIALIZE                  ORCSKOHPLUS-AREA
               MOVE    COMB-HOSPNUM        TO  LNK-SKOHPLUS-HOSPNUM
               MOVE    SYS-1001-PREFNUM    TO  LNK-SKOHPLUS-PREFNUM
               MOVE    COMB-PTID           TO  LNK-SKOHPLUS-PTID
               MOVE    COMB-HKNCOMBINUM    TO  LNK-SKOHPLUS-HKNCOMBI
               MOVE    WRK-SYMD            TO  LNK-SKOHPLUS-SRYYMD
               CALL    "ORCSKOHPLUS"       USING
                                           ORCSKOHPLUS-AREA
                                           SPA-AREA
               IF      LNK-SKOHPLUS-RC     =   ZERO
                   IF      LNK-SKOHPLUS-RECESKYKBN =   "1" OR  "3"
                       MOVE    "2"         TO  TOUKEI01-TRECESKYKBN(IDY)
                   ELSE
                       MOVE    "1"         TO  TOUKEI01-TRECESKYKBN(IDY)
                   END-IF
               ELSE
                   MOVE    "1"         TO  TOUKEI01-TRECESKYKBN (IDY)
               END-IF
           END-IF
      *
           .
       20011-PREFKBN-40-CHECK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    基本ファイル出力処理
      *****************************************************************
       2002-KIHON-WRITE-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-SEIHO 
           INITIALIZE                  TBL-SEIHO-TOUKEI01-AREA
                                       WRK-KOH-TOUKEI01-REC
      *
           MOVE    ZERO                TO  WRK-SEIHO-FLG
      *
      *    公費単独の退避（生保、中国残留邦人等、自立支援）と他の
      *    公費単独のチェック（精神入院は除く）    
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-TOUKEI01-TABLE-MAX
                   OR      WRK-TOUKEI01-HOSPNUM (IDXX) =   ZERO
             IF      WRK-RECEIPT-KBN   =
                                       WRK-TOUKEI01-RECEIPT-KBN (IDXX)
               MOVE    WRK-TOUKEI01-OCC (IDXX)     TO  TOUKEI01-REC
      *
      *        公費単独の場合（精神入院・心身喪失・包括以外）
               IF      TOUKEI01-HKNNUM     =   SPACE
                   IF    ( TOUKEI01-TKOHHKNNUM (1) =   "020"   OR
                                                       "030"     )
                   OR    ( TOUKEI01-HKNJANUM       =   "9999999" )
                       CONTINUE
                   ELSE 
                       IF      TOUKEI01-HKNJANUM       =   SPACE
                           IF    ( TOUKEI01-TKOHHKNNUM (1) =
                                                   "012"   OR  "025"
                                               OR  "015"   OR  "016"
                                               OR  "021"            )
                               IF    ( TOUKEI01-TKOHHKNNUM  (2)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (3)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (4)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (5)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (6)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (7)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (8)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (9)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                               AND   ( TOUKEI01-TKOHHKNNUM  (10)
                                           =   TOUKEI01-TKOHHKNNUM (1) 
                                           OR  SPACE                  )
                                   CONTINUE
                               ELSE
      *****************************IF    ( TOUKEI01-TKOHHKNNUM  (2)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************AND   ( TOUKEI01-TKOHHKNNUM  (3)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************AND   ( TOUKEI01-TKOHHKNNUM  (4)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************    生保、中国残留邦人等、自立支援のみ
      *****************************    の公費単独の場合退避
      *****************************    MOVE    TOUKEI01-REC      TO
      *****************************                WRK-KOH-TOUKEI01-REC
      *****************************ELSE
                                       MOVE    1           TO  FLG-SEIHO
      *****************************END-IF
                               END-IF
                           ELSE
                               MOVE    1           TO  FLG-SEIHO
                           END-IF
                       ELSE
      *                    生保単独の場合退避
                           ADD     1        TO  TBL-SEIHO-TOUKEI01-MAX
                           MOVE    TOUKEI01-REC
                                            TO  TBL-SEIHO-TOUKEI01-REC
                                                (TBL-SEIHO-TOUKEI01-MAX)
                       END-IF
                  END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           IF      FLG-SEIHO           =   1
               INITIALIZE                  TBL-SEIHO-TOUKEI01-AREA
           END-IF    
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-TOUKEI01-TABLE-MAX
                   OR      WRK-TOUKEI01-HOSPNUM (IDXX) =   ZERO
             IF      WRK-RECEIPT-KBN   =
                                       WRK-TOUKEI01-RECEIPT-KBN (IDXX)
               MOVE    WRK-TOUKEI01-OCC (IDXX)     TO  TOUKEI01-REC
      *
               MOVE    ZERO            TO  FLG-WRITE 
      *
      *        公費単独の場合（精神入院・心身喪失・包括以外）
               IF      TOUKEI01-HKNNUM     =   SPACE
                   IF    ( TOUKEI01-TKOHHKNNUM (1) =   "020"   OR
                                                       "030"     )
                   OR    ( TOUKEI01-HKNJANUM       =   "9999999" )
                       CONTINUE
                   ELSE 
                       IF      TOUKEI01-HKNJANUM       =   SPACE
      *                    公費単独の場合（生保、中国残留邦人等、自立支援）
                           IF    ( TOUKEI01-TKOHHKNNUM (1) =
                                                   "012"   OR  "025"
                                               OR  "015"   OR  "016"
                                               OR  "021"            )
      *                        同じ公費の公費単独の場合
                               IF    ( TOUKEI01-TKOHHKNNUM  (2)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (3)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (4)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (5)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (6)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (7)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (8)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (9)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                               AND   ( TOUKEI01-TKOHHKNNUM  (10)
                                                    =
                                           TOUKEI01-TKOHHKNNUM (1) 
                                                           OR  SPACE )
                                   MOVE    1           TO  FLG-WRITE
                               ELSE
                                   IF      FLG-SEIHO            =   1
                                       CONTINUE
                                   ELSE 
                                      MOVE    1           TO  FLG-WRITE
                                   END-IF
                               END-IF
                           ELSE
                               IF    ( FLG-SEIHO       =   1     )
                               AND   ( TOUKEI01-TKOHHKNNUM (1)
                                                   NOT =   "053" )
                                   CONTINUE
                               ELSE 
                                  MOVE    1           TO  FLG-WRITE
                               END-IF
                           END-IF
                       ELSE
                           MOVE    1                TO  FLG-WRITE 
                       END-IF
                  END-IF
               END-IF
      *
      *        社保で公費併用の場合（生保、中国残留邦人等、自立支援、児童保護）
               IF    ( TOUKEI01-HKNNUM   NOT =   SPACE )
               AND   ( TOUKEI01-TKOHHKNNUM (1)
                                           =   "012"   OR  "025"   OR 
                                               "015"   OR  "016"   OR
                                               "021"   OR  "053"      )
               AND   ( TOUKEI01-TKOHHKNNUM (2)
                                           =   SPACE   OR  "972"   OR
                                               "974"                  )
                   IF      TOUKEI01-HKNNUM   =
                                               "060"   OR  "067"   OR
                                               "068"   OR  "069"   OR
                                               "039"   OR  "040"
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDA FROM   1   BY  1
                               UNTIL   IDA >      TBL-SEIHO-TOUKEI01-MAX
                           PERFORM VARYING IDYY    FROM    1   BY  1
                                   UNTIL   IDYY    >       10
                                   OR      TOUKEI01-TKOHID (IDYY)
                                                   =       ZERO
                               IF      TBL-SEIHO-TOUKEI01-TKOHID(IDA 1)
                                           =   TOUKEI01-TKOHID (IDYY)
                                   MOVE    1           TO
                                       TBL-SEIHO-TOUKEI01-PRTFLG  (IDA)
                                   MOVE    1           TO  WRK-SEIHO-FLG
                                   PERFORM VARYING IDB FROM    1   BY  1
                                           UNTIL   IDB >       10
                                       IF      WRK-TOUKEI01-SEIHO-KEY
                                                           (IDXX IDB)
                                                           =   SPACE 
                                           MOVE
                                             TBL-SEIHO-TOUKEI01-KEY(IDA)
                                                       TO
                                                 WRK-TOUKEI01-SEIHO-KEY
                                                              (IDXX IDB)
                                           MOVE    10  TO  IDB
                                       END-IF
                                   END-PERFORM
                                   PERFORM VARYING IDB FROM    1   BY  1
                                           UNTIL   IDB >       10
                                       IF      TOUKEI01-HKNCOMBI (IDB)
                                                           =   ZERO
                                           MOVE
                                           TBL-SEIHO-TOUKEI01-HKNCOMBI
                                                              (IDA 1)
                                                           TO
                                               TOUKEI01-HKNCOMBI (IDB)
                                       END-IF
                                       IF      TOUKEI01-HKNCOMBI (IDB)
                                                   =
                                         TBL-SEIHO-TOUKEI01-HKNCOMBI
                                                                 (IDA 1)
                                           MOVE    10          TO  IDB
                                       END-IF
                                   END-PERFORM    
                                   MOVE    10          TO  IDYY
                                   MOVE    TBL-SEIHO-TOUKEI01-MAX
                                                       TO  IDA
                               END-IF
                           END-PERFORM      
                       END-PERFORM
                   END-IF                     
               END-IF
      *
               IF      FLG-WRITE            =   ZERO
      *            レセプト種別
                   PERFORM 20021-RECESYUBETU-SET-SEC
      *
                   MOVE    WRK-TOUKEI01-SEIHO-OCC (IDXX)
                                            TO  TOUKEI01-SEIHO-OCC
                   WRITE   TOUKEI01-REC
      *
                   IF      STS-TOUKEI01        =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-TOUKEIERR
                       STRING
                       "月次統計データ（基本情報） 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                               STS-TOUKEI01        DELIMITED  BY  SIZE
                                                   INTO    WRK-TOUKEIERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   ADD     1                    TO  CNT-TOUKEI01
               END-IF
      *
               MOVE    "1"              TO  WRK-TOUKEI01-SYOKBN (IDXX) 
             END-IF
           END-PERFORM
      *
      *    公費単独レセの出力処理
           IF    ( WRK-SEIHO-FLG           =   ZERO )
           AND   (  WRK-KOH-TOUKEI01-HOSPNUM
                                           >   ZERO )
      *        主保険とのまとめがなく生保、中国残留邦人等、自立支援以外との
      *        公費単独の場合
               MOVE    WRK-KOH-TOUKEI01-REC  TO  TOUKEI01-REC
      *        レセプト種別
               PERFORM 20021-RECESYUBETU-SET-SEC
      *
      *???     MOVE    WRK-TOUKEI01-SEIHO-OCC (IDXX)
      *???                                   TO  TOUKEI01-SEIHO-OCC
               WRITE   TOUKEI01-REC
      *
               IF      STS-TOUKEI01        =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-TOUKEIERR
                   STRING  "月次統計データ（基本情報）"
                                               DELIMITED  BY  SIZE
                           "書き込みエラー STS="
                                               DELIMITED  BY  SIZE
                           STS-TOUKEI01    DELIMITED  BY  SIZE
                                           INTO    WRK-TOUKEIERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               ADD     1                TO  CNT-TOUKEI01
           ELSE
               IF      TBL-SEIHO-TOUKEI01-MAX    >   ZERO
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >   TBL-SEIHO-TOUKEI01-MAX
                       IF      TBL-SEIHO-TOUKEI01-PRTFLG (IDXX)
                                               =   1
                           CONTINUE
                       ELSE
                           MOVE    TBL-SEIHO-TOUKEI01-REC (IDXX)
                                               TO  TOUKEI01-REC
      *                    レセプト種別
                           PERFORM 20021-RECESYUBETU-SET-SEC
      *
                           MOVE    WRK-TOUKEI01-SEIHO-OCC (IDXX)
                                               TO  TOUKEI01-SEIHO-OCC
                           WRITE   TOUKEI01-REC
      *
                           IF      STS-TOUKEI01        =   "00"
                               CONTINUE
                           ELSE
                               MOVE    SPACE           TO  WRK-TOUKEIERR
                               STRING
                                   "月次統計データ（基本情報）"
                                                   DELIMITED  BY  SIZE
                                   "書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                                   STS-TOUKEI01    DELIMITED  BY  SIZE
                                                   INTO    WRK-TOUKEIERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *
                           ADD     1                TO  CNT-TOUKEI01
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *    病名ファイル出力処理
           IF      WRK-TOUKEI01-HOSPNUM (1)    >   ZERO
               PERFORM 20022-BYOMEI-HENSYU-SEC
           END-IF
           .
       2002-KIHON-WRITE-EXT.
           EXIT. 
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       20021-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      TOUKEI01-TEISEIKBN  =   "1"
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    公費が４つを超える時
           IF      TOUKEI01-TKOHHKNNUM (5)
                                   NOT =   SPACE
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
      *    包括のとき
           IF      TOUKEI01-HKNJANUM   =   "9999999"
               GO  TO  20021-RECESYUBETU-SET-EXT
           END-IF
      *
           INITIALIZE                  ORCSSYUBETUGETAREA
           MOVE    1               TO  LNK-SYUBETUGET-SYOFLG
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    WRK-PARA-SRYYM  TO  LNK-SYUBETUGET-SRYYM
           MOVE    TOUKEI01-HKNNUM TO  LNK-SYUBETUGET-HKNNUM
           MOVE    1               TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
                   OR      TOUKEI01-TTEKSTYMD (IDX)  =   SPACE
               IF      TOUKEI01-TTEKSTYMD (IDX)
                                       >   TOUKEI01-TTEKSTYMD (IDY)
                   MOVE    IDX             TO  IDY
               END-IF
           END-PERFORM
           MOVE    TOUKEI01-THONKZKKBN (IDY)
                                   TO  LNK-SYUBETUGET-HKN-HONKZKKBN
           MOVE    TOUKEI01-THOJOKBN (IDY)
                                   TO  LNK-SYUBETUGET-HKN-HOJOKBN
           MOVE    TOUKEI01-HKNKOHSBTKBN
                                   TO  LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
           MOVE    TOUKEI01-AGE    TO  LNK-SYUBETUGET-AGE
           MOVE    TOUKEI01-SYUGAKUKBN
                                   TO  LNK-SYUBETUGET-SYUGAKUKBN
      *****MOVE    TOUKEI01-PREFNUM
      *****                        TO  LNK-SYUBETUGET-PREFNUM
           MOVE    TOUKEI01-RJNHKNNUM
                                   TO  LNK-SYUBETUGET-RJNHKNNUM
           MOVE    TOUKEI01-RJNPAYKBN
                                   TO  LNK-SYUBETUGET-RJNPAYKBN
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
               MOVE    TOUKEI01-TKOHHKNNUM (IDX)   TO
                                   LNK-SYUBETUGET-KOHNUM     (IDX)
               MOVE    TOUKEI01-TKOHHBTNUM (IDX)  TO
                                   LNK-SYUBETUGET-KOHHBTNUM  (IDX)
               IF      TOUKEI01-TRECESKYKBN (IDX) =   "1"
                   MOVE    "1"                 TO
                                       LNK-SYUBETUGET-RECESKYKBN (IDX)
               END-IF 
           END-PERFORM
      *
           IF      LNK-SYUBETUGET-HKNNUM
                                       =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM 
               MOVE    TOUKEI01-HKNKBN     TO  LNK-SYUBETUGET-HKNKBN
               MOVE    TOUKEI01-SAIGAIKBN  TO  LNK-SYUBETUGET-SAIGAIKBN
           END-IF
      *
      *    後期高齢非該当の有無設定
           MOVE    ZERO            TO  WRK-KOUKI-FLG
      *
           IF    ( LNK-SYUBETUGET-AGE    >=  70                    )
           AND   ( LNK-SYUBETUGET-SRYYM  >=  WRK-CONS-SRYYM-200804 )
               IF    ( LNK-SYUBETUGET-HKNNUM
                                           =   SPACE   OR  "040"   
                                           OR  "039"   OR  "067"
                                           OR  "069"             )
               OR    ( LNK-SYUBETUGET-HKNNUM (1:2)
                                           =   "98"    OR  "90"
                                           OR  "91"               )
                   CONTINUE
               ELSE
                   IF   (  LNK-SYUBETUGET-HKNNUM
                                               =   "060"   OR  "068"  )
                   OR   (( LNK-SYUBETUGET-HKNNUM
                                           NOT =   "060"   AND "068" )
                           AND
                         ( LNK-SYUBETUGET-HKN-HOJOKBN
                                               =   SPACE             ))
                       MOVE      LNK-SYUBETUGET-SRYYM TO  WRK-TEKSTYYMM
                       IF   TOUKEI01-TTEKSTYMD (IDY)(1:6)  >=
                                                   LNK-SYUBETUGET-SRYYM
                            MOVE  TOUKEI01-TTEKSTYMD (IDY)(7:2)
                                                       TO  WRK-TEKSTDD
                       ELSE
                            MOVE  01                   TO  WRK-TEKSTDD
                       END-IF
      *
                       INITIALIZE                      PTKOHINF-REC
                       MOVE    WRK-PARA-HOSPNUM TO  PTKOH-HOSPNUM
                       MOVE    TOUKEI01-PTID    TO  PTKOH-PTID
                       MOVE    "976"               TO  PTKOH-KOHNUM
                       MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key4"              TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key4"              TO  MCP-PATHNAME
                           PERFORM 900-PTKOHINF-READ-N-SEC
                           PERFORM UNTIL   FLG-PTKOHINF    =   1
                               IF    (PTKOH-TEKSTYMD   <=  WRK-TEKSTYMD)
                               AND   (WRK-TEKSTYMD     <=
                                                       PTKOH-TEKEDYMD  )
                                   MOVE    1           TO  WRK-KOUKI-FLG
                                   MOVE    1           TO  FLG-PTKOHINF
                               ELSE
                                   MOVE    "tbl_ptkohinf"
                                                    TO  MCP-TABLE
                                   MOVE    "key4"   TO  MCP-PATHNAME
                                   PERFORM  900-PTKOHINF-READ-N-SEC
                               END-IF
                           END-PERFORM
                       END-IF
                       MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
                       MOVE    "key4"               TO  MCP-PATHNAME
                       PERFORM  900-CLOSE-SEC
                   END-IF
               END-IF
      *
               IF      WRK-KOUKI-FLG   =   1
                   MOVE    SPACE       TO  LNK-SYUBETUGET-HKN-HOJOKBN
               END-IF
           END-IF
           MOVE    TOUKEI01-BIRTHDAY   TO  LNK-SYUBETUGET-BIRTHDAY
      *
      *    国保のとき９７７の公費の有無
           IF    ( LNK-SYUBETUGET-BIRTHDAY
                                       <   19440402 )
           AND   ( LNK-SYUBETUGET-HKNNUM
                                       =   "060"    )
           AND   ( LNK-SYUBETUGET-SRYYM
                                       >   "201404" )
               MOVE    "99"                TO  LNK-SYUBETUGET-RJNPAYKBN
      *
               MOVE      LNK-SYUBETUGET-SRYYM
                                           TO  WRK-TEKSTYYMM
               IF   TOUKEI01-TTEKSTYMD (IDY)(1:6)
                                           >=  LNK-SYUBETUGET-SRYYM
                    MOVE  TOUKEI01-TTEKSTYMD (IDY)(7:2)
                                               TO  WRK-TEKSTDD
               ELSE
                   MOVE    01                  TO  WRK-TEKSTDD
               END-IF
      *
               INITIALIZE                      PTKOHINF-REC
               MOVE    WRK-PARA-HOSPNUM    TO  PTKOH-HOSPNUM
               MOVE    TOUKEI01-PTID       TO  PTKOH-PTID
               MOVE    "977"               TO  PTKOH-KOHNUM
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PTKOHINF-READ-N-SEC
                   PERFORM UNTIL   FLG-PTKOHINF    =   1
                       IF    ( PTKOH-TEKSTYMD  <=  WRK-TEKSTYMD   )
                       AND   ( WRK-TEKSTYMD    <=  PTKOH-TEKEDYMD )
                           MOVE    SPACE           TO
                                               LNK-SYUBETUGET-RJNPAYKBN
                           MOVE    1               TO  FLG-PTKOHINF
                       ELSE
                           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
                           MOVE    "key4"          TO  MCP-PATHNAME
                           PERFORM  900-PTKOHINF-READ-N-SEC
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_ptkohinf"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM  900-CLOSE-SEC
           END-IF
      *
           CALL    "ORCSSYUBETUGET"    USING   ORCSSYUBETUGETAREA
      *
           MOVE    LNK-SYUBETUGET-RECESYUBETU
                                       TO  TOUKEI01-RECESYUBETU
      *
      *    DISPLAY "TOUKEI01-RECESYUBETU: " TOUKEI01-RECESYUBETU
           .
        20021-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名情報出力処理
      *****************************************************************
       20022-BYOMEI-HENSYU-SEC              SECTION.
      *
           IF      WRK-PARA-DATAKBN    =   "1"
               GO  TO  20022-BYOMEI-HENSYU-EXT
           END-IF
      *    
      *    患者病名読み込み
           INITIALIZE                  PTBYOMEI-REC
           MOVE    WRK-PARA-HOSPNUM    TO  PTBYO-HOSPNUM
           MOVE    PTINF-PTID          TO  PTBYO-PTID
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM         UNTIL       FLG-PTBYOMEI     =   1
               PERFORM 200221-BYOMEI-WRITE-SEC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       20022-BYOMEI-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    患者病名出力処理
      *****************************************************************
       200221-BYOMEI-WRITE-SEC         SECTION.
      *
      *    診療開始日
           IF      PTBYO-SRYYMD(1:6)    =   WRK-PARA-SRYYM
               CONTINUE
           ELSE
               IF      PTBYO-SRYYMD(1:6)    <   WRK-PARA-SRYYM 
                   IF      PTBYO-TENKIKBN       =   SPACE
                       CONTINUE
                   ELSE
                       IF      PTBYO-TENKIYMD(1:6)  >=  WRK-PARA-SRYYM
                           CONTINUE
                       ELSE
                           GO  TO  200221-BYOMEI-WRITE-EXT
                       END-IF
                   END-IF       
               ELSE
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    入外区分
           IF      PTBYO-NYUGAIKBN      =   SPACE
               CONTINUE
           ELSE
               IF      PTBYO-NYUGAIKBN  NOT =   WRK-PARA-NYUGAIKBN
                   GO  TO  200221-BYOMEI-WRITE-EXT
               END-IF
           END-IF
      *
      *    "999999"のコードを読み飛ばす
           INITIALIZE                  WRK-PTBYOMEI-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
                   OR      PTBYO-BYOMEICD (IDX)    =   SPACE
               IF      PTBYO-BYOMEICD (IDX)    =   ALL "9"
                   CONTINUE
               ELSE
                   ADD     1               TO  WRK-PTBYO-BYOMEICDSU
                                               IDY
                   MOVE    PTBYO-BYOMEICD (IDX)
                                           TO  WRK-PTBYO-BYOMEICD (IDY)
               END-IF
           END-PERFORM
           MOVE    WRK-PTBYO-BYOMEICD-G
                                       TO  PTBYO-BYOMEICD-G
           MOVE    WRK-PTBYO-BYOMEICDSU
                                       TO  PTBYO-BYOMEICDSU    
      *
      *    疑いフラグより病名を編集
           IF    ( PTBYO-UTAGAIFLG NOT =   SPACE )
           OR    ( PTBYO-HOSOKU-COMMENT
                                   NOT =   SPACE )
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
               MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
               MOVE    PTBYO-HOSOKU-COMMENT
                                           TO  SMKBYO-HOSOKU-COMMENT
               MOVE    PTBYO-BYOMEIHENFLG  TO  SMKBYO-BYOMEIHENFLG
               MOVE    PTBYO-BYOMEICD-G    TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
      *
               COMPUTE PTBYO-BYOMEIMOJI    =   SMKBYO-LENGTH   /   2
               MOVE    SMKBYO-BYOMEI       TO  PTBYO-BYOMEI
               MOVE    SMKBYO-BYOMEIHENFLG TO  PTBYO-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  PTBYO-BYOMEICD-G
           END-IF
      *
           INITIALIZE                      TOUKEI03-REC
           MOVE    TOUKEI01-HOSPNUM    TO  TOUKEI03-HOSPNUM
           MOVE    TOUKEI01-SRYYM      TO  TOUKEI03-SRYYM
           MOVE    TOUKEI01-NYUGAIKBN  TO  TOUKEI03-NYUGAIKBN
           MOVE    TOUKEI01-PTID       TO  TOUKEI03-PTID
           MOVE    TOUKEI01-RECEIPT-KBN
                                       TO  TOUKEI03-RECEIPT-KBN
           MOVE    TOUKEI01-RECEKA     TO  TOUKEI03-RECEKA
           MOVE    PTBYO-SRYKA         TO  TOUKEI03-SRYKA
           MOVE    PTBYO-SRYYMD        TO  TOUKEI03-SRYYMD
           MOVE    PTBYO-RENNUM        TO  TOUKEI03-RENNUM
           MOVE    PTBYO-BYOMEI        TO  TOUKEI03-BYOMEI
           MOVE    PTBYO-BYOMEIMOJI    TO  TOUKEI03-BYOMEIMOJI
           MOVE    PTBYO-UTAGAIFLG     TO  TOUKEI03-UTAGAIFLG
           MOVE    PTBYO-SYUBYOFLG     TO  TOUKEI03-SYUBYOFLG   
           MOVE    PTBYO-MANSEIKBN     TO  TOUKEI03-MANSEIKBN
           MOVE    PTBYO-HKNCOMBI      TO  TOUKEI03-HKNCOMBI
           MOVE    PTBYO-REZEFLG        TO  TOUKEI03-REZEFLG
           IF      WRK-PARA-SRYYM       <   PTBYO-TENKIYMD(1:6)
               CONTINUE
           ELSE    
               MOVE    PTBYO-TENKIKBN      TO  TOUKEI03-TENKIKBN
               MOVE    PTBYO-TENKIYMD      TO  TOUKEI03-TENKIYMD   
           END-IF    
           MOVE    PTBYO-HKNBYOMEIFLG  TO  TOUKEI03-HKNBYOMEIFLG
           MOVE    PTBYO-BYOMEIHENFLG  TO  TOUKEI03-BYOMEIHENFLG   
           MOVE    PTBYO-BYOMEICD-G    TO  TOUKEI03-BYOMEICD-G
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
                   OR      PTBYO-BYOMEICD (IDX)    =   SPACE
               MOVE    IDX                 TO  TOUKEI03-BYOMEICDSU
           END-PERFORM
      *    レセプト診療科
           IF      PTBYO-SRYKA     NOT =   SPACE    
               INITIALIZE                      SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE     PTBYO-SRYKA        TO  SYS-1005-KBNCD
               MOVE    WRK-SYMD            TO  SYS-1005-STYUKYMD
                                               SYS-1005-EDYUKYMD 
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
               ELSE
                   INITIALIZE                      SYS-1005-REC
               END-IF
               MOVE    SYS-1005-RECESRYKA  TO  TOUKEI03-RECESRYKA
           END-IF
      *
           WRITE   TOUKEI03-REC
      *
           IF      STS-TOUKEI03        =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-TOUKEIERR
               STRING "月次統計データ（病名情報） 書き込みエラー STS="
                                           DELIMITED  BY  SIZE
                       STS-TOUKEI03        DELIMITED  BY  SIZE
                                           INTO    WRK-TOUKEIERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-TOUKEI03
           .
       200221-BYOMEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計・診療行為データ編集処理
      *****************************************************************
       2003-SRYACCT-HENSYU-SEC              SECTION.
      *
      *    統計データ（病名）のみのとき
           IF      WRK-PARA-DATAKBN    =   "2"
               GO  TO  2003-SRYACCT-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-RENNUM
      *     
      *    レセプト診療科
           INITIALIZE                      SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    ACCT-SRYKA          TO  SYS-1005-KBNCD
           MOVE    WRK-SYMD            TO  SYS-1005-STYUKYMD
                                           SYS-1005-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
           ELSE
               INITIALIZE                      SYS-1005-REC
           END-IF
      *
      *    診療行為データ読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO      MCP-TABLE
           MOVE    "key2"              TO      MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACT    =    1
               PERFORM 20031-SRYACT-HENSYU-SEC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"         TO  MCP-TABLE
           MOVE    "key2"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2003-SRYACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    診療情報出力処理
      *****************************************************************
       20031-SRYACT-HENSYU-SEC               SECTION.
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL   IDX2    >   5
                   OR      SRY-SRYCD (IDX2)    =   SPACE
      *
               MOVE    SPACE               TO  TOUKEI02-REC
               INITIALIZE                      TOUKEI02-REC
      *
      *        診療会計データ編集
               MOVE    TOUKEI01-KEY        TO  TOUKEI02-KEY
               MOVE    ACCT-ZAINUM         TO  TOUKEI02-ZAINUM
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  TOUKEI02-SEQNO
               MOVE    COMB-HKNID          TO  TOUKEI02-HKNID
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       4
                   MOVE    COMB-KOHID (IDX3)   TO  TOUKEI02-TKOHID(IDX3)
               END-PERFORM
               MOVE    ACCT-SRYKA          TO  TOUKEI02-SRYKA
               MOVE    SYS-1005-RECESRYKA  TO  TOUKEI02-RECESRYKA
               MOVE    ACCT-SRYKBN         TO  TOUKEI02-SRYKBN
               MOVE    ACCT-JIHOKBN        TO  TOUKEI02-JIHOKBN
               IF      ACCT-JIHOKBN        =   "1"
                   MOVE    ZERO            TO  TOUKEI02-ZAITEN
               ELSE
                   MOVE    ACCT-ZAITEN     TO  TOUKEI02-ZAITEN
               END-IF
               MOVE    ACCT-ZAIKAISU       TO  TOUKEI02-ZAIKAISU
               MOVE    ACCT-MEISAISU       TO  TOUKEI02-MEISAISU
               MOVE    ACCT-SYUTEN1        TO  TOUKEI02-SYUTEN1
               MOVE    ACCT-SYUTEN2        TO  TOUKEI02-SYUTEN2
               MOVE    ACCT-YKZTEN         TO  TOUKEI02-YKZTEN
               MOVE    ACCT-KIZAITEN       TO  TOUKEI02-KIZAITEN
               MOVE    ACCT-DRCD           TO  TOUKEI02-DRCD
               MOVE    ACCT-HKNCOMBI       TO  TOUKEI02-HKNCOMBI
               MOVE    ACCT-DAY-TBL (1)    TO  TOUKEI02-DAY-AREA
      *
      *        診療行為データ編集
               MOVE    SRY-RENNUM          TO  TOUKEI02-RENNUM
               IF      SRY-SRYSYUKBN       =   SPACE
                   MOVE    ACCT-SRYKBN     TO  TOUKEI02-SRYSYUKBN (1:2)
                   MOVE    "0"             TO  TOUKEI02-SRYSYUKBN (3:1)
               ELSE
                   MOVE    SRY-SRYSYUKBN   TO  TOUKEI02-SRYSYUKBN
               END-IF
               MOVE    SRY-JIHIMONEY(IDX2) TO  TOUKEI02-JIHIMONEY
               MOVE    SRY-SRYCD    (IDX2) TO  TOUKEI02-SRYCD
               MOVE    SRY-SRYSURYO (IDX2) TO  TOUKEI02-SRYSURYO
               MOVE    SRY-SRYKAISU (IDX2) TO  TOUKEI02-SRYKAISU
               MOVE    SRY-INPUTNUM (IDX2) TO  TOUKEI02-INPUTNUM
      *
               IF      SRY-SRYCD    (IDX2) =   WRK-CONS-SRYCD1
                                           OR  WRK-CONS-SRYCD2
                                           OR  WRK-CONS-SRYCD3
                                           OR  WRK-CONS-SRYCD4
                                           OR  WRK-CONS-SRYCD5
                                           OR  WRK-CONS-SRYCD6
                                           OR  WRK-CONS-SRYCD7
                   COMPUTE TOUKEI02-ZAITEN     =   TOUKEI02-ZAITEN *
                                                   (-1)
                   IF      SRY-SRYCD    (IDX2) =   WRK-CONS-SRYCD1
                       COMPUTE TOUKEI02-YKZTEN     =   ACCT-YKZTEN     *
                                                       (-1)
                   ELSE
                       COMPUTE TOUKEI02-SYUTEN1    =   ACCT-SYUTEN1    *
                                                       (-1)
                   END-IF
               END-IF
      *
               INITIALIZE                       TNS-TENSU-REC
               MOVE    SRY-SRYCD(IDX2)     TO  TNS-SRYCD
grpsys         MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TANICD          TO  TOUKEI02-TANICD
               MOVE    TNS-TANINAME        TO  TOUKEI02-TANINAME
               MOVE    TNS-FORMALNAME      TO  TOUKEI02-FORMALNAME
               MOVE    TNS-TENSIKIBETU     TO  TOUKEI02-TENSIKIBETU
               MOVE    TNS-TEN             TO  TOUKEI02-TEN
               IF      TNS-SRYCD (1:1)     =   "6"
                   MOVE    TNS-YAKKAKJNCD      TO  TOUKEI02-YAKKAKJNCD
               END-IF
               IF      TNS-TENSIKIBETU     =   1   OR   3
                   IF      ACCT-NYUGAIKBN      =   "1"
                       MOVE  TNS-NYUTENTTLKBN  TO  TOUKEI02-TENTTLKBN
                   ELSE
                       MOVE  TNS-GAITENTTLKBN  TO  TOUKEI02-TENTTLKBN
                   END-IF
               END-IF
      *
               IF (   (SRY-SRYCD    (IDX2)(1:1)
                                               =   "8"  )
                   OR (SRY-SRYCD    (IDX2)(1:3)
                                               =   "008") ) 
               AND (   SRY-INPUTNUM (IDX2) NOT =   ZERO   )
                   MOVE    SRY-HOSPNUM             TO  PTCOM-HOSPNUM
                   MOVE    SRY-PTID                TO  PTCOM-PTID
                   MOVE    SRY-ZAINUM              TO  PTCOM-ZAINUM
                   MOVE    SRY-SRYCD   (IDX2)      TO  PTCOM-SRYCD
                   MOVE    SRY-INPUTNUM(IDX2)      TO  PTCOM-RENNUM
                   PERFORM 950-PTCOM-READ-SEC
                   MOVE    PTCOM-INPUTCOMENT       TO  TOUKEI02-NAME
               ELSE
                   MOVE    TNS-NAME                TO  TOUKEI02-NAME
               END-IF
      *
               WRITE   TOUKEI02-REC
      *
               IF      STS-TOUKEI02        =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-TOUKEIERR
                   STRING
                       "月次統計データ（統計情報） 書き込みエラー STS="
                                               DELIMITED  BY  SIZE
                           STS-TOUKEI02        DELIMITED  BY  SIZE
                                               INTO    WRK-TOUKEIERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      * 
               ADD     1               TO  CNT-TOUKEI02 
      *
      *        生保単独で別ファイルがあるとき
               IF    ( WRK-SEIHO-TOUKEI01-HOSPNUM )
                                       >   ZERO
               AND   ( WRK-SEIHO-TOUKEI01-SRYYM
                                   NOT =   SPACE  )    
                   MOVE    WRK-SEIHO-TOUKEI01-KEY
                                           TO  TOUKEI02-KEY1
                   WRITE   TOUKEI02-REC
      *
                   IF      STS-TOUKEI02        =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-TOUKEIERR
                       STRING
                       "月次統計データ（統計情報） 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                               STS-TOUKEI02        DELIMITED  BY  SIZE
                                                   INTO    WRK-TOUKEIERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   ADD     1               TO  CNT-TOUKEI02
               END-IF  
           END-PERFORM
      *
           .
       20031-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR         =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR         TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-TOUKEIERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBT010 "         DELIMITED  BY  SIZE
                   WRK-TOUKEIERR       DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      ************************************ *****************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   TOUKEI01-FILE
                   TOUKEI02-FILE
                   TOUKEI03-FILE
      *
           IF      CNT-TOUKEI01           =   ZERO  
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TOUKEI01  CNT-TOUKEI01:" CNT-TOUKEI01
           DISPLAY "TOUKEI02  CNT-TOUKEI02:" CNT-TOUKEI02
           DISPLAY "TOUKEI03  CNT-TOUKEI03:" CNT-TOUKEI03
           DISPLAY "*** ORCBT010 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設入所情報処理
      *****************************************************************
       500-SISETU-HENSYU-SEC             SECTION.
      *
           INITIALIZE                      TABLE-PTKOHINF-AREA
      *
           PERFORM VARYING IDZ4    FROM    1   BY  1
                   UNTIL   IDZ4    >       4
               INITIALIZE                      PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    KEY-N-PTID          TO  PTKOH-PTID
               EVALUATE    IDZ4
                   WHEN    1
      *                ９５０（老併）
                       MOVE    "950"               TO  PTKOH-KOHNUM
                   WHEN    2
      *                ９５１（老健）
                       MOVE    "951"               TO  PTKOH-KOHNUM
                   WHEN    3
      *                ９４８（医併）
                       MOVE    "948"               TO  PTKOH-KOHNUM
                   WHEN    4
      *                ９４９（医療）
                       MOVE    "949"               TO  PTKOH-KOHNUM
               END-EVALUATE
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PTKOHINF-READ-N-SEC
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF  
               END-IF
               PERFORM     UNTIL   FLG-PTKOHINF    =   1
                           OR      TBL-PTKOHINF-MAX
                                                   >=  100
                   IF      PTKOH-SAKJOKBN      =  SPACE
                       ADD     1               TO  TBL-PTKOHINF-MAX
                       MOVE    PTKOHINF-REC    TO  TBL-PTKOHINF-REC
                                                   (TBL-PTKOHINF-MAX)
      *
                       DISPLAY "   TBL-PTKOHINF-MAX=" TBL-PTKOHINF-MAX
                           " KOHNUM="
                                   TBL-PTKOH-KOHNUM   (TBL-PTKOHINF-MAX)
                           " ST="  TBL-PTKOH-TEKSTYMD (TBL-PTKOHINF-MAX)
                           " ED="  TBL-PTKOH-TEKEDYMD (TBL-PTKOHINF-MAX)
      *
                   END-IF
      *
                   MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                   MOVE    "key4"           TO  MCP-PATHNAME
                   PERFORM  900-PTKOHINF-READ-N-SEC
               END-PERFORM
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-PERFORM
           .
       500-SISETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者レセプト分割情報処理
      *****************************************************************
       500-PT-HDIST-DAY-HENSYU-SEC     SECTION.
      *
           INITIALIZE                      TBL-PT-HDIST-DAY-T
      *
           INITIALIZE                      PT-HDIST-DAY-REC
           MOVE    SPA-HOSPNUM         TO  PT-HDIST-DAY-HOSPNUM
           MOVE    WRK-PTID            TO  PT-HDIST-DAY-PTID
           MOVE    WRK-PARA-SRYYM      TO  PT-HDIST-DAY-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN  TO  PT-HDIST-DAY-NYUGAIKBN
           MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PT-HDIST-DAY-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-PT-HDIST-DAY
           END-IF
           PERFORM     UNTIL   FLG-PT-HDIST-DAY    =   1
                       OR      TBL-PT-HDIST-DAY-MAX
                                                   >=  10
               ADD     1                   TO  TBL-PT-HDIST-DAY-MAX
               MOVE    PT-HDIST-DAY-REC    TO  TBL-PT-HDIST-DAY-REC
                                               (TBL-PT-HDIST-DAY-MAX)
      *
               DISPLAY "   TBL-PT-HDIST-DAY=" TBL-PT-HDIST-DAY-MAX
                       " DAY=" TBL-PT-HDIST-DAY-DAY
                                               (TBL-PT-HDIST-DAY-MAX)
               DISPLAY "      1=" TBL-PT-HDIST-DAY-HKNCOMBI-OCC
                                               (TBL-PT-HDIST-DAY-MAX 1)
               DISPLAY "      2=" TBL-PT-HDIST-DAY-HKNCOMBI-OCC
                                               (TBL-PT-HDIST-DAY-MAX 2)
      *
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PT-HDIST-DAY-READ-N-SEC
           END-PERFORM
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       500-PT-HDIST-DAY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院チェック処理
      *****************************************************************
       500-ORCSNYUINDAY-CHECK-SEC          SECTION.
      *
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SPA-HOSPNUM         TO  LNK-NYUINDAY-HOSPNUM
           MOVE    WRK-PARA-SRYYM      TO  LNK-NYUINDAY-SANTEIYM
           MOVE    PTINF-PTID          TO  LNK-NYUINDAY-PTID
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
      *
           IF      LNK-NYUINDAY-RC     >   0
               DISPLAY "LNK-NYUINDAY-RC=" LNK-NYUINDAY-RC
               GO  TO  500-ORCSNYUINDAY-CHECK-EXT
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       31
               IF      LNK-NYUINDAY-DOUJITSU-KBN (IDXX)
                                               =   "1"
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                       TO  WRK-HKNCOMBINUM
                   MOVE    IDXX        TO  IDXD
                   PERFORM 20011-NYUINACCT-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           .
       500-ORCSNYUINDAY-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院料と異なる食事算定保険組み合わせチェック処理
      *****************************************************************
       500-ORCSSKJHKN-CHECK-SEC            SECTION.
      *
           INITIALIZE                  ORCSSKJHKN-AREA
           MOVE    SPA-HOSPNUM     TO  LNK-SKJHKN-HOSPNUM
           MOVE    PTINF-PTID      TO  LNK-SKJHKN-PTID
           MOVE    WRK-PARA-SRYYM  TO  LNK-SKJHKN-SANTEIYM
           CALL    "ORCSSKJHKN"    USING
                                       ORCSSKJHKN-AREA
                                       ORCSNYUINDAY-AREA
                                       SPA-AREA
           IF    ( LNK-SKJHKN-RC   =   ZERO )
               PERFORM VARYING IDXX   FROM    1   BY  1
                       UNTIL ( IDXX   >       31 )
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL ( IDZZ    >   2 )
                       IF    ( LNK-SKJHKN-HKNCOMBINUM  (IDXX IDZZ)
                                   NOT =   SPACE )
                           MOVE    LNK-SKJHKN-HKNCOMBINUM  (IDXX IDZZ)
                                       TO  WRK-HKNCOMBINUM-G
                           MOVE    IDXX    TO  IDXD
                           PERFORM 20011-NYUINACCT-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
           .
       500-ORCSNYUINDAY-CHECK-EXT.
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
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
      *        包括を含まないとき
               IF    ( WRK-PARA-HOUKATUKBN =   "1"  )
                   IF    ( ACCT-ZAITEN         =   ZERO   )
                   OR    ( ACCT-HKNCOMBI       =   "9999" )
                   OR    ( ACCT-JIHOKBN        =   "1"    )
                       GO  TO  900-SRYACCT-READ-SEC
                   END-IF
               END-IF
      *        包括を含むときでも持参薬入力分は出力しない
               IF    ( WRK-PARA-HOUKATUKBN =   "2"  )
                   IF    ( ACCT-JIHOKBN        =   "1"   )
                   AND   ( ACCT-ZAIKBN         =   3  OR  4 )
                       GO  TO  900-SRYACCT-READ-SEC
                   END-IF
               END-IF
      *
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタ　ＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
      ***     IF      FLG-NYUINACCT       =   ZERO
      ***         MOVE    NACCT-PTID          TO  KEY-N-PTID
      ***         MOVE    ZERO                TO  KEY-N-HKNCOMBI 
      ***     END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           IF      COMB-HKNCOMBINUM =   9999
               MOVE    ZERO                TO  FLG-HKNCOMBI
               GO  TO  910-HKNCOMBI-INV-EXT
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"     TO  MCP-TABLE
               MOVE    "key3"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"     TO  MCP-TABLE
           MOVE    "key3 "            TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   MOVE    ZERO                TO  FLG-PTKOHINF
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
                   INITIALIZE                  PTKOHINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTKOHINF-REC
               IF      PTKOH-SAKJOKBN      =   "1"
                   GO  TO  900-PTKOHINF-READ-N-SEC
               END-IF
               MOVE    ZERO                TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1                   TO  FLG-PTKOHINF
           END-IF
      *
           .
       900-PTKOHINF-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           MOVE    HKN-HKNNUM          TO  WRK-CHK-HKNNUM
           MOVE    HKN-PAYKBN          TO  WRK-CHK-PAYKBN
           MOVE    HKN-TEKSTYMD        TO  WRK-CHK-TEKSTYMD
           MOVE    HKN-TEKEDYMD        TO  WRK-CHK-TEKEDYMD
      *
           INITIALIZE                      HKNNUM-REC
      *
      *    保険番号マスタ退避ワークチェック
      *
           PERFORM VARYING WRK-HKNNUM-IDX    FROM    1   BY  1
                   UNTIL   WRK-HKNNUM-IDX    >   WRK-HKNNUM-MAX
                   OR      HKN-HKNNUM        NOT =   SPACE
               IF    ( WRK-CHK-HKNNUM  =
                                       TBL-HKN-HKNNUM (WRK-HKNNUM-IDX) )
               AND   ( WRK-CHK-PAYKBN  =
                                       TBL-HKN-PAYKBN (WRK-HKNNUM-IDX) )
               AND   ( TBL-HKN-TEKSTYMD (WRK-HKNNUM-IDX)
                                       <=  WRK-CHK-TEKSTYMD            )
               AND   ( TBL-HKN-TEKEDYMD(WRK-HKNNUM-IDX)
                                       >=  WRK-CHK-TEKEDYMD            )
                   MOVE    TBL-HKNNUM-REC (WRK-HKNNUM-IDX)
                                       TO  HKNNUM-REC
               END-IF
           END-PERFORM
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF      HKN-HKNNUM          =   SPACE
               INITIALIZE                      HKNNUM-REC
               MOVE    WRK-PARA-HOSPNUM    TO  HKN-HOSPNUM
               MOVE    WRK-CHK-HKNNUM      TO  HKN-HKNNUM
               MOVE    WRK-CHK-PAYKBN      TO  HKN-PAYKBN
               MOVE    WRK-CHK-TEKSTYMD    TO  HKN-TEKSTYMD
               MOVE    WRK-CHK-TEKEDYMD    TO  HKN-TEKEDYMD
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               MOVE    "tbl_hknnum"          TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC                =   ZERO
                   MOVE    "tbl_hknnum"            TO  MCP-TABLE
                   MOVE    "key"                   TO  MCP-PATHNAME
                   PERFORM 900-HKNNUM-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
               END-IF
      *
               IF      FLG-HKNNUM          =   ZERO
      *            退避ワークに編集
                   IF      TBL-HKNNUM-CONST-MAX
                                               >   WRK-HKNNUM-MAX
                       COMPUTE WRK-HKNNUM-MAX
                                       =   WRK-HKNNUM-MAX  +   1
                       MOVE    HKNNUM-REC      TO
                                   TBL-HKNNUM-REC (WRK-HKNNUM-MAX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_hknnum"          TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
      *
       900-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号（主キー）読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC                =   ZERO
               MOVE    "tbl_hknnum"            TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       900-KYUSEIRRK-READ-SEC          SECTION.
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
               ELSE
                   MOVE    1                   TO  FLG-KYUSEIRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       910-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC          TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       900-TENSU-READ-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    WRK-PARA-SRYYM      TO  TTNS-CHK-YMD (1:6)
           MOVE    "01"                TO  TTNS-CHK-YMD (7:2)
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
grpsys         MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
grpsys             PERFORM 900-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                       MOVE    ZERO            TO  FLG-TENSU
      *
      *                点数マスタ退避ワークに編集する
                       PERFORM 9001-TTNS-HEN-SEC
                   ELSE
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数読込
      *****************************************************************
       900-ADRS-INV-SEC            SECTION.
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRS-REC
                   MOVE    ZERO                TO  FLG-ADRS
               ELSE
                   INITIALIZE                      ADRS-REC
                   MOVE    1                   TO  FLG-ADRS
               END-IF
           ELSE
               INITIALIZE                  ADRS-REC
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報マスタ読込
      *****************************************************************
       900-PTRSIINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTRSIINF
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   IF      PTRSI-SAKUJOKBN     =   "1"
                       MOVE    1                   TO  FLG-PTRSIINF
                   END-IF
               ELSE
                    INITIALIZE                 PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスタＲＥＡＤ
      *****************************************************************
       900-PTBYO-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC 
           ELSE
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者レセプト分割読込
      *****************************************************************
       900-PT-HDIST-DAY-READ-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PT-HDIST-DAY-REC
               MOVE    ZERO                TO  FLG-PT-HDIST-DAY
           ELSE
               MOVE    1                   TO  FLG-PT-HDIST-DAY
               INITIALIZE                      PT-HDIST-DAY-REC
           END-IF
      *
           .
       900-PT-HDIST-DAY-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-TOUKEI01-READ-SEC             SECTION.
      *
           READ    TOUKEI01-FILE
               INVALID
                   MOVE    1           TO  FLG-TOUKEI01
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOUKEI01
           END-READ
           .
       900-TOUKEI01-READ-EXT.
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
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
