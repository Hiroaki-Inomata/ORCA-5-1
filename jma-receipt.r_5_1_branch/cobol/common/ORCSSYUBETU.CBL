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
       PROGRAM-ID.             ORCSSYUBETU.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : レセプト種別取得処理
      *  管理者            : 
      *  06/02/01    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.02.01    NACL-藤原    06/09/05  ２００６年１０月改正対応
      *                                     長期９７４対応
      *                                     福岡地方公費対応
      *
      *  03.03.01    NACL-門脇    06/09/27  ２００６年１０月改正対応
      *                                     レセプト種別 
      *                                     公費の追加
      *  03.03.02    NACL-藤原    06/10/10  主保険がない場合、精神入院は
      *                                     公費単独のレセプトとする
      *  03.03.03    NACL-藤原    06/10/25  ２００６年１０月改正対応
      *                                     福岡地方公費の障害
      *  03.03.04    NACL-藤原    06/10/26  福岡地方公費のレセ記載判定の修正
      *  03.03.05    NACL-藤原    06/10/27  レセプトで生活保護の請求がな
      *                                     い場合記載の対象としない
      *  03.03.06    NACL-藤原    06/10/31  療養介護医療２４の追加
      *  03.03.07    NACL-藤原    06/12/04  福岡地方公費の障害のついて全て
      *                                     公費付加情報より記載判定を行う
      *  03.03.08    NACL-藤原    06/12/14  不明分のとき生保単独の上限
      *                                     チェックはしない
      *  03.03.09    NACL-藤原    06/12/14  地方公費のみのときは対象としない
      *
      *  03.04.01    NACL-藤原    07/04/04  特例退職医療の種別決定条件の修正
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/09/04  宮崎県「公費９０」対応
      *                                     ２００７年９月以降のみ対象 
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *  04.01.02    NACL-藤原    08/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      *  04.01.03    NACL-藤原    08/02/12  宮崎県「公費９０」対応
      *                                     通常の記載方法に戻す 
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *  04.02.02    NACL-藤原    08/04/14  月途中の負担者番号変更対応
      *                                     （中国残留法人等の公費単独） 
      *  04.02.03    NACL-藤原    08/04/18  福岡地方公費の国保のとき
      *                                     平成２０年４月以降は公費付加
      *                                     情報より記載判定はしない
      *  04.02.04    NACL-藤原    08/04/23  後期高齢者の船員保険を本人とする
      *  04.02.05    NACL-門脇    08/08/07  精神通院の請求点判定追加
      *
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *                                     レセプト種別（紙レセプト）の追加
      *  04.03.02    NACL-門脇    08/09/02  請求点判定追加
      *  04.03.03    NACL-門脇    08/12/17  平成２１年１月改正対応
      *
      *  04.04.01    NACL-藤原    08/12/02  自費、不明分のレセプト種別
      *                                     （紙レセプト）への設定追加
      *  04.04.02    NACL-藤原    09/01/14  月途中に医保と生保の併用から
      *                                     生保単独になったときのまとめ
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
      *  04.05.02    NACL-藤原    11/08/25  公費単独対応
      *                                     公費を最大５０に変更
      *  04.05.03    NACL-藤原    11/11/22  同日再入院対応
      *  04.05.04    NACL-藤原    12/04/11  平成２４年４月改正対応
      *                                     「６２Ｂ型肝炎」の追加
      *  04.05.05    NACL-藤原    12/11/12  同日再入院のチェック条件の修正
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *  04.06.02    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.06.03    NACL-藤原    11/02/24  月途中に医保と児童保護(53)の
      *                                     併用から単独になったときのまとめ
      *  04.06.04    NACL-藤原    12/10/04  同一レセプトの公費が４つを超
      *                                     えた場合の種別不明対応
      *  04.06.02    NACL-藤原    12/09/27  生活保護期間重複対応
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.02    NACL-藤原    14/03/30  平成２６年４月改正対応
      *  04.07.03    NACL-藤原    14/06/03  短期滞在手術基本料２（又は３）
      *                                     で入院料未算定日の対応
      *  04.04.04    NACL-藤原    14/06/05  月途中のまとめのとき第二公費
      *                                     マル長のときも対象とする
      *
      *  04.07.01    NACL-藤原    14/08/18  ７５歳以上の海外居住者２割対応
      *  04.07.02    NACL-藤原    14/12/05  平成２７年１月改正対応
      *  04.07.03    NACL-藤原    14/12/08  月途中に公費の負担者番号等の
      *                                     変更があった場合の記載対応
      *                                     法別番号、適用開始日の順とする
      *
      *  04.08.01    NACL-藤原    15/10/08  複数枚レセプト分割対応
      *                                     公費単独の２回以上の分割対応
      *  04.08.02    NACL-藤原    15/12/22  施設入所時の分割対応
      *  04.08.03    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.04    NACL-藤原    18/03/28  平成３０年４月改正対応
      *
      *  05.00.01    NACL-藤原    18/11/29  肝がん治療対応
      *  05.00.02    NACL-藤原    19/02/15  Warning 対応
      *  05.00.03    NACL-藤原    19/07/09  公費単独＋沖縄県の精神障害者
      *                                     特別措置(121)の対応
      *  05.00.04    NACL-藤原    20/04/01  令和２年４月改正対応
      *                                     感染症（新型コロナ）の法別番
      *                                     号による記載順対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01). 
           03  FLG-HKNNUM              PIC 9(01). 
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-PT-HDIST-DAY         PIC 9(01).
      * 
           03  FLG-LASTHKNID           PIC 9(01). 
           03  FLG-KOHRECEDSP          PIC 9(01).
           03  FLG-ADDKBN              PIC 9(01).
           03  FLG-SEIHO               PIC 9(01).
           03  FLG-WRITE               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDA                     PIC 9(04).
           03  IDB                     PIC 9(04).
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDW                     PIC 9(04).
           03  IDX1                    PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX3                    PIC 9(04).
           03  IDX4                    PIC 9(04).
           03  IDX5                    PIC 9(04).
           03  IDXX                    PIC 9(04).
           03  IDYY                    PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
           03  IDZ4                    PIC 9(04).
      *
       01  KEY-AREA                        VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID          PIC 9(10).
               05  KEY-N-HKNCOMBI      PIC 9(04).
           03  KEY-OLD.
               05  KEY-O-PTID          PIC 9(10).
               05  KEY-O-HKNCOMBI      PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
      *
      *    最終受診日
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC 9(06).
               05  WRK-SRYDD           PIC 9(02).  
      *    １：入院 ２：入院外
           03  WRK-DBKBN               PIC X(01).
      *
      *    診療年月
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      * 
           03  WRK-PREFKBN             PIC X(01).
           03  WRK-PREFNUM             PIC X(02).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN          PIC X(01).
               05  WRK-TEISEIKBN       PIC X(01).
      *
           03  WRK-SEIHO-FLG           PIC 9(01).
      *
           03  WRK-RECEIPT-KBN         PIC X(10).
      *
      *    年齢
           03  WRK-NENREI              PIC 9(03).
      *    後期高齢者非該当フラグ
           03  WRK-KOUKI-FLG           PIC 9(01).
      *
           03  WRK-TEKSTYMD.
               05  WRK-TEKSTYYMM       PIC 9(06).
               05  WRK-TEKSTDD         PIC 9(02).
      *
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL        OCCURS  5.
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
      *    公費並替え用ワーク
           03  WRK-KOHI-AREA.
               05  WRK-KOHI-TBL        OCCURS  10.
                   07  WRK-KOHID-O     PIC  9(10).
                   07  WRK-KOHHKNNUM-O PIC  X(03).
                   07  WRK-KOHPAYKBN-O PIC  X(02).
                   07  WRK-RECESKYKBN-O
                                       PIC  X(01).
                   07  WRK-KOHTEKSTYMD-O
                                       PIC  X(08).
                   07  WRK-KOHHBTNUM-O PIC  X(02).
                   07  WRK-KOHFTNJANUM-O
                                       PIC  X(08).
                   07  WRK-KOHJKYSNUM-O
                                       PIC  X(20).
           03  WRK-KOHI.
               05  WRK-KOHID           PIC  9(10).
               05  WRK-KOHHKNNUM       PIC  X(03).
               05  WRK-KOHPAYKBN       PIC  X(02).
               05  WRK-RECESKYKBN      PIC  X(01).
               05  WRK-KOHTEKSTYMD     PIC  X(08).
               05  WRK-KOHHBTNUM       PIC  X(02).
               05  WRK-KOHFTNJANUM     PIC  X(08).
               05  WRK-KOHJKYSNUM      PIC  X(20).
      *
           03  TBL-MAX                 PIC 9(03).
      *
           03  ST-IDX                  PIC 9(03).
      ***  03  ED-IDX                  PIC 9(03).
      *
      *    請求区分別公費情報（１：レセプト　２：請求書）
           03  WRK-SEIKYUKBN-KOHI-AREA.
               05  WRK-SEIKYU-TBL   OCCURS  2.
                   07  WRK-SEIKYU-OCC    OCCURS  10.
                       09  WRK-SEIKYU-KOHID      PIC  9(10).
                       09  WRK-SEIKYU-KOHHKNNUM  PIC  X(03).
                       09  WRK-SEIKYU-KOHPAYKBN  PIC  X(02).
                       09  WRK-SEIKYU-RECESKYKBN PIC  X(01).
                       09  WRK-SEIKYU-TEKSTYMD   PIC  X(08).
                       09  WRK-SEIKYU-KOHHBTNUM  PIC  X(02).
                       09  WRK-SEIKYU-KOHFTNJANUM
                                                 PIC  X(08).
                       09  WRK-SEIKYU-KOHJKYSNUM PIC  X(20).
      *
       01  WK-RECESYUBETU-AREA.
           03  WK-KOKUHO               PIC X(01).
           03  WK-IHO                  PIC X(01).
           03  WK-RJN                  PIC X(01).
           03  WK-KOHI-CNT             PIC 9(01).
           03  WK-HONKZKKBN            PIC X(01).
      *
       01  WRK-SRT-AREA.
           03  WRK-SRT-TABLE.
               05  FILLER              PIC X(03)   VALUE   "027".  
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
      *
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "038".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "062".  
               05  FILLER              PIC X(03)   VALUE   "025".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
               05  FILLER              PIC X(03)   VALUE   SPACE.
      *
               05  FILLER              PIC X(03)   VALUE   "013".  
               05  FILLER              PIC X(03)   VALUE   "014".  
               05  FILLER              PIC X(03)   VALUE   "018".  
               05  FILLER              PIC X(03)   VALUE   "029".  
               05  FILLER              PIC X(03)   VALUE   "030".  
               05  FILLER              PIC X(03)   VALUE   "010".  
               05  FILLER              PIC X(03)   VALUE   "011".  
               05  FILLER              PIC X(03)   VALUE   "020".  
               05  FILLER              PIC X(03)   VALUE   "021".  
               05  FILLER              PIC X(03)   VALUE   "015".  
               05  FILLER              PIC X(03)   VALUE   "016".  
               05  FILLER              PIC X(03)   VALUE   "024".  
               05  FILLER              PIC X(03)   VALUE   "022".  
               05  FILLER              PIC X(03)   VALUE   "028".  
               05  FILLER              PIC X(03)   VALUE   "093".
               05  FILLER              PIC X(03)   VALUE   "017".  
               05  FILLER              PIC X(03)   VALUE   "079".  
               05  FILLER              PIC X(03)   VALUE   "019".  
               05  FILLER              PIC X(03)   VALUE   "023".  
               05  FILLER              PIC X(03)   VALUE   "052".  
               05  FILLER              PIC X(03)   VALUE   "054".  
               05  FILLER              PIC X(03)   VALUE   "051".  
               05  FILLER              PIC X(03)   VALUE   "091".  
               05  FILLER              PIC X(03)   VALUE   "038".  
               05  FILLER              PIC X(03)   VALUE   "092".  
               05  FILLER              PIC X(03)   VALUE   "053".  
               05  FILLER              PIC X(03)   VALUE   "066".  
               05  FILLER              PIC X(03)   VALUE   "062".  
               05  FILLER              PIC X(03)   VALUE   "025".  
               05  FILLER              PIC X(03)   VALUE   "012".
               05  FILLER              PIC X(03)   VALUE   "972".
               05  FILLER              PIC X(03)   VALUE   "974".
      *
           03  WRK-SRT-TABLE-R         REDEFINES   WRK-SRT-TABLE.
               05  WRK-SRT-OCC         OCCURS  3.
                   07  WRK-SRT-KOHHKNNUM
                                       PIC X(03)   OCCURS  32.
      *
           03  WRK-SRT-KOHHKNNUM-MAX   PIC 9(02)   VALUE   32.
      *     
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).   
      * 
       01  WRK-RECE61-REC. 
           COPY    "CPRCF061.INC"      REPLACING  //RECE61//
                                       BY         //WRK-RECE61//.
      *    老人保険−支払区分
           03  WRK-RECE61-RJNPAYKBN    PIC X(02).
      *    主保険−公費種別区分
           03  WRK-RECE61-HKNKOHSBTKBN PIC X(01).
      *    
           03  WRK-RECE61-KOH-AREA     OCCURS  10. 
      *       公費−法別番号
               05  WRK-RECE61-KOHHBTNUM    PIC X(02).
      *
       01  TABLE-RECE61-AREA.
           02  TBL-RECE61-OCC          OCCURS  100.
           COPY    "CPRCF061.INC"      REPLACING  //RECE61//
                                       BY         //TBL-RECE61//.
      *    老人保険−支払区分
           03  TBL-RECE61-RJNPAYKBN    PIC X(02).
      *    主保険−公費種別区分
           03  TBL-RECE61-HKNKOHSBTKBN PIC X(01).
      *    
           03  TBL-RECE61-KOH-AREA     OCCURS  10. 
      *       公費−法別番号
               05  TBL-RECE61-KOHHBTNUM    PIC X(02).
      *    処理区分
           03  TBL-RECE61-SYOKBN       PIC X(01).
      *     
       01  WRK-SEIHO-RECE61-REC. 
           COPY    "CPRCF061.INC"  REPLACING  //RECE61//
                                   BY         //WK-SH-RECE61//.
      *    老人保険−支払区分
           03  WRK-SH-RE61-RJNPAYKBN         PIC X(02).
      *    主保険−公費種別区分
           03  WRK-SH-RE61-HKNKOHSBTKBN      PIC X(01).
      *    
           03  WRK-SH-RE61-KOH-AREA          OCCURS  10. 
      *       公費−法別番号
               05  WRK-SH-RE61-KOHHBTNUM         PIC X(02).
      *
      *    ワーク生保まとめ
       01  TBL-SEIHO-RECE61-AREA.
           02  TBL-SEIHO-RECE61-REC    OCCURS  50. 
           COPY    "CPRCF061.INC"      REPLACING  //RECE61-//
                                       BY         //TBL-SH-RE61-//.
           02  TBL-SH-RE61-PRTFLG      PIC 9(01)  OCCURS  50.
           02  TBL-SH-RE61-MAX         PIC 9(03).
      *
       01  WRK-KOH-RECE61-REC. 
           COPY    "CPRCF061.INC"      REPLACING  //RECE61-//
                                       BY         //WRK-KO-RE61-//.
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
      *    労災保険番号 
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    自賠責保険番号 
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    長期
           03  WRK-CONS-CHOUKI-HKNNUM  PIC X(03)   VALUE   "972".
      *    長期（上位所得者）
           03  WRK-CONS-CHOUKI-HKNNUM2 PIC X(03)   VALUE   "974".
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM  PIC X(03)   VALUE   "975". 
      *
           03  WRK-CONS-SRYYM-200609   PIC 9(06)   VALUE   200609.      
           03  WRK-CONS-SRYYM-200804   PIC 9(06)   VALUE   200804.      
           03  WRK-CONS-SRYYM-201004   PIC 9(06)   VALUE   201004.      
           03  WRK-CONS-SRYYM-201501   PIC 9(06)   VALUE   201501.     
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *     
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
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
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    保険情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    患者レセプト分割
       01  PT-HDIST-DAY-REC.
           COPY    "CPPT-HDIST-DAY.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    公費付加情報取得サブ
           COPY    "CPORCSKOHPLUS.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者自己負担金レセプト記載判定
           COPY    "CPORCSFTNJGNCHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    レセプト種別返却サブ
           COPY    "CPORCSSYUBETUGET.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSSYUBETU.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *****************************************************************
     *
       PROCEDURE                    DIVISION    USING
                                    ORCSSYUBETUAREA
                                    SPA-AREA
           .
     *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
      *??
      *     DISPLAY "HOSPNUM  =" LNK-SYUBETU-HOSPNUM
      *     DISPLAY "PREFNUM  =" LNK-SYUBETU-PREFNUM
      *     DISPLAY "SRYYM    =" LNK-SYUBETU-SRYYM
      *     DISPLAY "NYUGAIKBN=" LNK-SYUBETU-NYUGAIKBN
      *     DISPLAY "PTID     =" LNK-SYUBETU-PTID
      *     DISPLAY "BIRTHDAY =" LNK-SYUBETU-BIRTHDAY
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-END-SEC
      *??
      *     DISPLAY "LNK-SYUBETU-RC=" LNK-SYUBETU-RC
      *     PERFORM VARYING IDX FROM 1 BY  1
      *             UNTIL   IDX >    100
      *             OR  LNK-SYUBETU-TEISYUTUSAKI (IDX) =   ZERO
      *        DISPLAY "IDX=" IDX " " LNK-SYUBETU-TEISYUTUSAKI (IDX) "#"
      *                               LNK-SYUBETU-RECESYUBETU (IDX) "#"
      *                               LNK-SYUBETU-HKNJANUM (IDX) "#"
      *                               LNK-SYUBETU-HKNCOMBI-AREA (IDX)
      *     END-PERFORM
      *??
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
                                           TABLE-RECE61-AREA
      *
           MOVE    ZERO                TO  LNK-SYUBETU-RC
           INITIALIZE                      LNK-SYUBETU-OT
      *
      *    入力項目チェック
grpsys     IF    ( LNK-SYUBETU-HOSPNUM     =   ZERO )
           OR    ( LNK-SYUBETU-PREFNUM     =   ZERO  )
           OR    ( LNK-SYUBETU-SRYYM       =   ZERO  )
           OR    ( LNK-SYUBETU-NYUGAIKBN   =   SPACE )
           OR    ( LNK-SYUBETU-PTID        =   ZERO  )
           OR    ( LNK-SYUBETU-BIRTHDAY    =   SPACE )
               MOVE    1                   TO  LNK-SYUBETU-RC
           ELSE
               IF     LNK-SYUBETU-SRYYM        <=  "200209"
                   MOVE    2                   TO  LNK-SYUBETU-RC
               ELSE
                   INITIALIZE                  ORCSAGECHKAREA
grpsys             MOVE    LNK-SYUBETU-HOSPNUM TO  AGECHK-HOSPNUM
                   MOVE    LNK-SYUBETU-PTID    TO  AGECHK-PTID
                   MOVE    LNK-SYUBETU-SRYYM   TO  AGECHK-SRYYMD (1:6)
                   MOVE    "01"                TO  AGECHK-SRYYMD (7:2)
                   MOVE    LNK-SYUBETU-BIRTHDAY
                                               TO  AGECHK-BIRTHDAY
                   CALL    "ORCSAGECHK"        USING
                                               ORCSAGECHKAREA
grpsys                                         SPA-AREA
                   IF      AGECHK-RC           =   ZERO
                       MOVE    AGECHK-NENREI       TO  WRK-NENREI
                   ELSE
                       MOVE    3                   TO  LNK-SYUBETU-RC
                   END-IF
               END-IF
           END-IF
      *
           IF      LNK-SYUBETU-RC      =   ZERO
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-END
               GO  TO  100-INIT-EXT
           END-IF
      *??
      *     DISPLAY "AGE       =" WRK-NENREI 
      *     DISPLAY " SYUGAKU  =" AGECHK-SYUGAKUKBN
      *??
      *     
      *    レセプト編集区分情報
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    LNK-SYUBETU-SRYYM   TO  SYS-2005-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD (7:2)
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE               TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-RJNGNBKKBN =   SPACE
               MOVE    ZERO                TO  SYS-2005-RJNGNBKKBN
           END-IF
      * 
           INITIALIZE                  SYS-25-REC
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    LNK-SYUBETU-SRYYM   TO  SYS-25-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-25-STYUKYMD (7:2)
           MOVE    SYS-25-STYUKYMD     TO  SYS-25-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-25-HOSPNUM
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-25-REC
           ELSE    
               MOVE    SPACE           TO  SYS-25-REC
           END-IF
           IF      SYS-25-DISASTER-RECEIPT
                                   =   SPACE
               MOVE    ZERO            TO  SYS-25-DISASTER-RECEIPT
           END-IF
      *??
           display
          "SYS-200501-DISASTER-RECEIPT=" SYS-25-DISASTER-RECEIPT
      *??
      *
      *    県内扱い保険者情報（福岡のみ）
           IF    ( LNK-SYUBETU-PREFNUM =   "40"                  )
           AND   ( LNK-SYUBETU-SRYYM   <=  WRK-CONS-SRYYM-200609 )
               MOVE    SPACE           TO  WRK-1033-TABLE
      *     
               MOVE    SPACE           TO  SYS-1033-REC
               INITIALIZE                  SYS-1033-REC
               MOVE    "1033"          TO  SYS-1033-KANRICD
               MOVE    "*"             TO  SYS-1033-KBNCD
grpsys         MOVE    SPA-HOSPNUM     TO  SYS-1033-HOSPNUM
               MOVE    SYS-1033-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
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
                   MOVE    SYS-1033-STYUKYMD   TO  
                                               WRK-1033-STYUKYMD   (IDX)
                   MOVE    SYS-1033-EDYUKYMD   TO
                                               WRK-1033-EDYUKYMD   (IDX)
                   MOVE    SYS-1033-TBL        TO
                                               WRK-1033-HKNJANUM-G (IDX)
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    診療年月編集
           MOVE    LNK-SYUBETU-SRYYM   TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    LNK-SYUBETU-SRYYM   TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
           .
       100-INIT-EXT.
           EXIT
           .
      *     
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
      *    施設入所情報
           PERFORM 500-SISETU-HENSYU-SEC
      *
      *    患者レセプト分割情報情報
           PERFORM 500-PT-HDIST-DAY-HENSYU-SEC
      *
      *    診療会計読み込み
           MOVE    "1"                 TO  WRK-DBKBN 
      *   
           INITIALIZE                      SRYACCT-REC
grpsys     MOVE    LNK-SYUBETU-HOSPNUM TO  ACCT-HOSPNUM
           MOVE    LNK-SYUBETU-PTID    TO  ACCT-PTID
           MOVE    LNK-SYUBETU-SRYYM   TO  ACCT-SRYYM
           MOVE    LNK-SYUBETU-NYUGAIKBN
                                       TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key37"             TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
                   OR          FLG-END         =   1 
               PERFORM 2001-SRYACCT-HENSYU-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key37"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    入院会計読み込み
           IF      LNK-SYUBETU-NYUGAIKBN   =   "1"
               MOVE    "2"                 TO  WRK-DBKBN 
      *     
               INITIALIZE                  NYUINACCT-REC
grpsys         MOVE    LNK-SYUBETU-HOSPNUM TO  NACCT-HOSPNUM
               MOVE    LNK-SYUBETU-PTID    TO  NACCT-PTID
               MOVE    LNK-SYUBETU-SRYYM   TO  NACCT-SRYYM
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
                   MOVE    "key14"             TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT     =   1
                       OR          FLG-END           =   1
                   PERFORM 2002-NYUINACCT-HENSYU-SEC
               END-PERFORM
      *
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       2001-SRYACCT-HENSYU-SEC                SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
      *
           PERFORM             UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-END         =   1
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
grpsys         MOVE    LNK-SYUBETU-HOSPNUM TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM 910-HKNCOMBI-INV-SEC
      *
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      FLG-HKNCOMBI        =   ZERO
                   IF      COMB-TEKSTYMD   <=  WRK-STYMD
                   AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                       MOVE    SPACE           TO  WRK-UPDKBN
                   ELSE
                       MOVE    "1"             TO  WRK-UPDKBN
                   END-IF
               ELSE
                   MOVE    "1"                 TO  WRK-TEISEIKBN        
               END-IF    
      *
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1      
                               OR      KEY-NEW     NOT =   KEY-OLD
                   PERFORM 20019-FILE-SYORI-SEC 
      *
                   IF      WRK-UPDKBN          =   SPACE                   
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       MOVE    KEY-NEW             TO  KEY-OLD
                       PERFORM     UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1      
                                   OR      KEY-NEW     NOT =   KEY-OLD
                           MOVE    "tbl_sryacct"       TO  MCP-TABLE
                           MOVE    "key37"             TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                        END-PERFORM
                   ELSE
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key37"             TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM         
           .
       2001-SRYACCT-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    入院会計編集処理
      *****************************************************************
       2002-NYUINACCT-HENSYU-SEC              SECTION.
      *     
           INITIALIZE                      WRK-DAY-AREA
                                           WRK-HKNCOMBI-TABLE
      *
      *    最終診療日
           MOVE    NACCT-SRYYM         TO  WRK-SRYYM
      *
           PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                           OR      FLG-END         =   1      
      *        剤識別区分別テーブル編集
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       MOVE    1               TO  IDX
                   WHEN    "5"
                       MOVE    2               TO  IDX
               END-EVALUATE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1  >        31
                   IF      NACCT-DAY  (IDX1)       >   ZERO
                       MOVE    NACCT-DAY (IDX1)    TO
                                                   WRK-DAY (IDX IDX1)
      *                最終診療日
                       MOVE    IDX                 TO  WRK-SRYDD
                   END-IF
               END-PERFORM            
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
      *        同日再入院のチェック
               PERFORM 500-ORCSNYUINDAY-CHECK-SEC
      *
      *        入院会計より保険組合せ読込     
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF  ( ( WRK-DAY (1 IDX)        >   ZERO ) OR
                         ( LNK-NYUINDAY-KBN(IDX)  =   "1"  )    )  AND
                         ( WRK-DAY (2 IDX)        >   ZERO  )
                       PERFORM 20021-NYUINACCT-CHK-SEC
                   END-IF
               END-PERFORM        
      *
      *        入院会計の保険組合せを追加
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
      *
                   MOVE    WRK-HKNCOMBI-REC (IDX2)
                                               TO  HKNCOMBI-REC    
                   PERFORM 20019-FILE-SYORI-SEC
               END-PERFORM
           END-IF    
           .
       2002-NYUINACCT-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       20021-NYUINACCT-CHK-SEC         SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *
      *        保険組合せ追加
               IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                   INITIALIZE                  HKNCOMBI-REC
grpsys             MOVE    LNK-SYUBETU-HOSPNUM TO  COMB-HOSPNUM
                   MOVE    LNK-SYUBETU-PTID    TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX)     TO  COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *            （対象外の組合せは１をセット）
                   IF      FLG-HKNCOMBI    =   ZERO
                       MOVE    HKNCOMBI-REC    TO
                                               WRK-HKNCOMBI-REC  (IDX1)
                   ELSE
                       MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                       MOVE    "1"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                   END-IF
               END-IF
      *
      *        適用範囲かチェック（範囲外のとき２をセット）
               IF      WRK-DAY (2 IDX)     =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)   NOT =   SPACE
                       CONTINUE                    
                   ELSE     
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDX             TO  WRK-DD
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
       20021-NYUINACCT-CHK-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ファイル処理
      *****************************************************************
       20019-FILE-SYORI-SEC              SECTION.
      *
           EVALUATE    WRK-DBKBN
               WHEN    "1"
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
      *
      *            最終診療日
                   MOVE    ACCT-SRYYM          TO  WRK-SRYYM
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       31
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    IDX                 TO  WRK-SRYDD
                       END-IF
                   END-PERFORM
               WHEN    "2"
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                       MOVE    "1"         TO  WRK-TEISEIKBN
                   END-IF        
            END-EVALUATE
      *
      *    平成２０年４月以降に老人保険使用のとき
           IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-KOH1HKNNUM     =   "027"                 )
               MOVE    "1"                 TO  WRK-TEISEIKBN
           END-IF
      *
      *    平成２０年４月以降に退職国保で６５歳以上のとき
           IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
           AND   ( COMB-HKNNUM         =   "067"                 )
           AND   ( WRK-NENREI          >=  65                    )
               MOVE    "1"                 TO  WRK-TEISEIKBN
           END-IF
      *
      *    平成２２年４月以降に政府管掌を使用のとき
           IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-201004 )
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
                       INITIALIZE                      PTRSIINF-REC
                   WHEN    WRK-CONS-KOUGAI-HKNNUM
                       INITIALIZE                      PTHKNINF-REC
                   WHEN    OTHER
                       INITIALIZE                      PTHKNINF-REC
grpsys                 MOVE    LNK-SYUBETU-HOSPNUM TO  PTHKN-HOSPNUM
                       MOVE    COMB-PTID           TO  PTHKN-PTID
                       MOVE    COMB-HKNID          TO  PTHKN-HKNID
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       PERFORM 900-PTHKNINF-READ-SEC
               END-EVALUATE 
           END-IF
      *
      *    公費のレセプト・請求書への振り分け
      *
           INITIALIZE                  WRK-SEIKYUKBN-KOHI-AREA
                                       FLG-KOHRECEDSP
           MOVE    ZERO                TO  IDY
                                           IDZ    
      *
      *    地方公費のときレセプト請求区分を取得して判別
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      COMB-KOHHKNNUM (IDX)    
                                           =   SPACE
      *
      *      平成２０年４月以降の老人保険は対象外とする
             IF    ( WRK-STYMD (1:6)     >=  WRK-CONS-SRYYM-200804 )
             AND   ( COMB-KOHHKNNUM (IDX)  =  "027"                )
                 CONTINUE
             ELSE        
               IF     (COMB-KOHHKNNUM (IDX)(1:1)
                                           >=  "1" AND <=  "8")
               OR     (COMB-KOHHKNNUM (IDX)(1:2)   =  "99"    )
                   INITIALIZE                      HKNNUM-REC
grpsys             MOVE    LNK-SYUBETU-HOSPNUM     TO  HKN-HOSPNUM
                   MOVE    COMB-KOHHKNNUM (IDX)    TO  HKN-HKNNUM
                   MOVE    WRK-SRYYMD              TO  HKN-TEKSTYMD
                                                       HKN-TEKEDYMD
                   MOVE    "00"                    TO  HKN-PAYKBN   
                   MOVE    HKNNUM-REC              TO  MCPDATA-REC
                   PERFORM 900-HKNNUM-INV-SEC
      *            福岡県の国保のとき（４１老人以外）
                   IF    ( COMB-HKNNUM         =   "060"   OR  "067" )
                   AND   ( LNK-SYUBETU-PREFNUM =   40                )
                   AND   ( HKN-HBTNUM      NOT =   "41"              )
                   AND   ( COMB-KOHHKNNUM(IDX)(1:1)
                                              >=  "1" AND <=  "8"   )
                   AND   ( LNK-SYUBETU-SRYYM  <
                                              WRK-CONS-SRYYM-200804  )
                       IF    ( LNK-SYUBETU-SRYYM
                                              >  WRK-CONS-SRYYM-200609 )
                       AND   ( COMB-KOHHKNNUM (IDX)
                                          NOT =   "180"                )
                           IF      HKN-RECESKYKBN  =   "1" OR  "3"
                                                           OR  "5"
                               MOVE    2               TO  IDX1
                           ELSE
                               MOVE    1               TO  IDX1
                           END-IF
                       ELSE
                           PERFORM 200191-PREFKBN-40-CHECK-SEC
                       END-IF
                   ELSE
      *                福岡県の社保のとき（４１老人）
                       IF    ( COMB-HKNNUM     NOT =   "060" AND "067"
                                                             AND "039" )
                       AND   ( LNK-SYUBETU-PREFNUM =    40             )
                       AND   ( HKN-HBTNUM          =   "41"            )    
                           IF      COMB-HONKZKKBN      =   "1" 
                               MOVE    2                   TO  IDX1
                           ELSE
                               MOVE    1                   TO  IDX1
                           END-IF
                       ELSE
                           EVALUATE    COMB-HKNNUM
                               WHEN    "060"
                               WHEN    "067" 
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    "039"                
                                   IF      HKN-RECESKYKBN
                                                       =   "1" OR  "3"
                                                               OR  "4"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                               WHEN    OTHER                
                                   IF      HKN-RECESKYKBN
                                                       =   "2" OR  "3"
                                                       OR  "4" OR  "5"
                                       MOVE    2               TO  IDX1
                                   ELSE
                                       MOVE    1               TO  IDX1
                                   END-IF
                           END-EVALUATE
                       END-IF
                   END-IF
                   IF     IDX1          =        1
      *                地方公費レセ記載判定
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                       INITIALIZE              ORCSFTNJGNCHKAREA
grpsys                 MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    LNK-SYUBETU-NYUGAIKBN 
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    LNK-SYUBETU-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    LNK-SYUBETU-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
grpsys                                             SPA-AREA
                       IF     LNK-FTNJGNCHK-RC   =   1
                           MOVE       1        TO    FLG-KOHRECEDSP
                       END-IF
                   END-IF
               ELSE
                   MOVE    1                      TO  IDX1
      **>          IF   COMB-KOHHKNNUM (IDX)  =  "972" OR "974" OR "012"
      **>                                              OR "021"
                     IF    ( WRK-TEISEIKBN       NOT =   SPACE )
      **>            AND   ( COMB-KOHHKNNUM (IDX)    =   "012"
      **>                                            OR  "021" )
                     AND   ( COMB-KOHHKNNUM (IDX)(1:1)  =  "0" )
                     AND   ( COMB-HKNNUM             =   SPACE )
                         CONTINUE
                     ELSE 
      *                長期高額療養レセ記載判定（生保単独の不明分はしない）
                       MOVE    ZERO        TO  FLG-KOHRECEDSP
                       INITIALIZE              ORCSFTNJGNCHKAREA
grpsys                 MOVE    COMB-HOSPNUM
                                           TO  LNK-FTNJGNCHK-HOSPNUM
                       MOVE    COMB-PTID   TO  LNK-FTNJGNCHK-PTID
                       MOVE    LNK-SYUBETU-NYUGAIKBN 
                                           TO  LNK-FTNJGNCHK-NYUGAIKBN
      *                診療年月
                       MOVE    LNK-SYUBETU-SRYYM
                                           TO  LNK-FTNJGNCHK-SRYYM
      *                公費番号
                       MOVE    COMB-KOHHKNNUM (IDX)
                                           TO  LNK-FTNJGNCHK-KOHNUM
      *                公費ＩＤ
                       MOVE    COMB-KOHID (IDX)
                                           TO  LNK-FTNJGNCHK-KOHID
      *                都道府県番号
                       MOVE    LNK-SYUBETU-PREFNUM
                                           TO  LNK-FTNJGNCHK-PREFNUM
      *                保険組合せ番号
                       MOVE    COMB-HKNCOMBINUM
                                           TO  LNK-FTNJGNCHK-HKNCOMBINUM
      *                主保険−ＩＤ
                       MOVE    COMB-HKNID  TO  LNK-FTNJGNCHK-HKNID
      *
                       CALL    "ORCSFTNJGNCHK"     USING
                                                   ORCSFTNJGNCHKAREA
grpsys                                             SPA-AREA
                       IF     LNK-FTNJGNCHK-RC =   1
                           MOVE       1            TO    FLG-KOHRECEDSP
                       END-IF
                     END-IF
      **>          END-IF
               END-IF       
      *
               IF     IDX1                =   1
                   IF      FLG-KOHRECEDSP  NOT =  1
                       ADD     1                      TO  IDY    
                       MOVE    COMB-KOHHKNNUM (IDX)   TO 
                                          WRK-SEIKYU-KOHHKNNUM(IDX1 IDY)
                       MOVE    COMB-KOHID     (IDX)   TO
                                          WRK-SEIKYU-KOHID    (IDX1 IDY)
                   END-IF    
               END-IF
             END-IF    
          END-PERFORM
      *            
      *    ファイル編集処理
           INITIALIZE                      WRK-RECE61-REC
grpsys     MOVE    LNK-SYUBETU-HOSPNUM     TO  WRK-RECE61-HOSPNUM
           MOVE    LNK-SYUBETU-SRYYM       TO  WRK-RECE61-SRYYM
           MOVE    LNK-SYUBETU-NYUGAIKBN   TO  WRK-RECE61-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE61-PTID
           MOVE    COMB-HKNNUM         TO  WRK-RECE61-HKNNUM
           MOVE    PTHKN-HONKZKKBN     TO  WRK-RECE61-HONKZKKBN
           IF      COMB-HKNNUM         =   SPACE
               MOVE    ZERO                TO  WRK-RECE61-KOHID
           ELSE
               IF      COMB-KOH1HKNNUM     =   "027"
                   MOVE    COMB-KOH1ID         TO  WRK-RECE61-KOHID
                   MOVE    "0"                 TO  WRK-RECE61-HONKZKKBN
               ELSE
                   MOVE    ZERO                TO  WRK-RECE61-KOHID
                   EVALUATE    PTHKN-HKNNUM
                       WHEN    "002" 
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "A"
                               WHEN   "B"
                               WHEN   "C"
                               WHEN   "9"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "D"
                               WHEN   "E"
                               WHEN   "F"
                               WHEN   "8"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "G"
                               WHEN   "H"
                               WHEN   "I"
                               WHEN   "7"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                           END-EVALUATE
                       WHEN    "031"  THRU    "034"  
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "B"
                               WHEN   "9"   
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "E"
                               WHEN   "8"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                               WHEN   "H"
                               WHEN   "7"
                                   MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             END-EVALUATE
                       WHEN    "060" 
                       WHEN    "068" 
                           EVALUATE    LNK-SYUBETU-NYUGAIKBN
                               WHEN    "1"
                                   EVALUATE   PTHKN-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "5"
                                           MOVE    "0" 
                                               TO  WRK-RECE61-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "6"
                                           MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                                       WHEN   "0"
                                       WHEN   "4"
                                           MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                                       WHEN   "3"
                                           MOVE    "0"
                                               TO  WRK-RECE61-HONKZKKBN
                                   END-EVALUATE
                               WHEN    "2"
                                   EVALUATE   PTHKN-HOJOKBN
                                       WHEN   "1"
                                       WHEN   "4"
                                           MOVE    "0" 
                                               TO  WRK-RECE61-HONKZKKBN
                                       WHEN   "2"
                                       WHEN   "5"
                                           MOVE    "0" 
                                               TO  WRK-RECE61-HONKZKKBN
                                       WHEN   "3"
                                       WHEN   "6"
                                           MOVE    "0" 
                                               TO  WRK-RECE61-HONKZKKBN
                                   END-EVALUATE
                           END-EVALUATE
                      WHEN    "040" 
                      WHEN    "039" 
                          EVALUATE   PTHKN-HOJOKBN
                              WHEN   "1"
                              WHEN   "3"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                          END-EVALUATE             
                      WHEN    OTHER 
                          EVALUATE   PTHKN-HOJOKBN
                             WHEN   "9"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             WHEN   "8"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                             WHEN   "7"
                                 MOVE    "0" TO  WRK-RECE61-HONKZKKBN
                         END-EVALUATE
                   END-EVALUATE
               END-IF 
           END-IF
      *
           MOVE    PTHKN-HKNJANUM      TO  WRK-RECE61-HKNJANUM       
           MOVE    ZERO                TO  WRK-RECE61-TEKSTYMD 
           IF      COMB-HKNNUM         =   SPACE
               IF      COMB-KOH1HKNNUM     =   "020"   OR  "030"   OR
                                               "053"
      *********    MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMD
                   INITIALIZE                      PTKOHINF-REC
                   MOVE    WRK-RECE61-HOSPNUM  TO  PTKOH-HOSPNUM
                   MOVE    WRK-RECE61-PTID     TO  PTKOH-PTID
                   MOVE    COMB-KOHID (1)      TO  PTKOH-KOHID
                   MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                   PERFORM 900-PTKOHINF-READ-SEC
                   MOVE    PTKOH-TEKSTYMD      TO  WRK-RECE61-TEKSTYMD
                   MOVE    COMB-KOH1ID         TO  WRK-RECE61-KOH-KOHID
               END-IF
           END-IF
           IF      LNK-SYUBETU-SRYYM   <=   "200609"
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  WRK-RECE61-HOJOKBN
                   END-IF
               WHEN    "060" 
               WHEN    "068" 
                   EVALUATE    LNK-SYUBETU-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0" TO  WRK-RECE61-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1" TO  WRK-RECE61-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2" TO  WRK-RECE61-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1" TO  WRK-RECE61-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2" TO  WRK-RECE61-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3" TO  WRK-RECE61-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                           MOVE    PTHKN-HOJOKBN
                                               TO  WRK-RECE61-HOJOKBN
                   END-EVALUATE             
             END-EVALUATE
           ELSE
             EVALUATE    PTHKN-HKNNUM
               WHEN    "002"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE
                      IF     ( LNK-SYUBETU-SRYYM
                                           <   WRK-CONS-SRYYM-200804 )
                       OR    ( WRK-NENREI  <=  74                    )  
                           MOVE    PTHKN-HOJOKBN   TO
                                                  WRK-RECE61-HOJOKBN
                       ELSE
                           EVALUATE    PTHKN-HOJOKBN
                               WHEN    "A"
                               WHEN    "G"
                                   MOVE    "1" TO  WRK-RECE61-HOJOKBN
                               WHEN    "B"
                               WHEN    "H"
                                   MOVE    "2" TO  WRK-RECE61-HOJOKBN
                               WHEN    "C"
                               WHEN    "I"
                                   MOVE    "3" TO  WRK-RECE61-HOJOKBN
                               WHEN    OTHER
                                   MOVE    PTHKN-HOJOKBN
                                               TO  WRK-RECE61-HOJOKBN
                           END-EVALUATE
                       END-IF
                   END-IF
               WHEN    "031"   THRU    "034"
                   IF      PTHKN-HOJOKBN   =   SPACE
                       CONTINUE
                   ELSE    
                       MOVE    PTHKN-HOJOKBN   TO  WRK-RECE61-HOJOKBN
                   END-IF
               WHEN    "060" 
               WHEN    "068" 
                   EVALUATE    LNK-SYUBETU-NYUGAIKBN
                       WHEN    "1"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "0"
                               WHEN   "4"
                                   MOVE    "0" TO  WRK-RECE61-HOJOKBN
                               WHEN   "1"
                               WHEN   "5"
                                   MOVE    "1" TO  WRK-RECE61-HOJOKBN
                               WHEN   "2"
                               WHEN   "6"
                                   MOVE    "2" TO  WRK-RECE61-HOJOKBN
                               WHEN   "3"
                                   MOVE    "3" TO  WRK-RECE61-HOJOKBN
                           END-EVALUATE
                       WHEN    "2"
                           EVALUATE   PTHKN-HOJOKBN
                               WHEN   "1"
                               WHEN   "4"
                                   MOVE    "1" TO  WRK-RECE61-HOJOKBN
                               WHEN   "2"
                               WHEN   "5"
                                   MOVE    "2" TO  WRK-RECE61-HOJOKBN
                               WHEN   "3"
                               WHEN   "6"
                                   MOVE    "3" TO  WRK-RECE61-HOJOKBN
                           END-EVALUATE
                   END-EVALUATE
               WHEN    "040" 
               WHEN    "039" 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "1"
                       WHEN   "3"
                           MOVE    PTHKN-HOJOKBN   TO
                                               WRK-RECE61-HOJOKBN
                   END-EVALUATE             
               WHEN    OTHER 
                   EVALUATE   PTHKN-HOJOKBN
                       WHEN   "9"
                       WHEN   "8"
                       WHEN   "7"
                           MOVE    PTHKN-HOJOKBN
                                               TO  WRK-RECE61-HOJOKBN
                   END-EVALUATE             
             END-EVALUATE
           END-IF             
           MOVE    SPACE               TO WRK-RECE61-RECEKA
      *
      *    施設入所の有無
           IF    ( WRK-RECE61-RECEIPT-KBN  =   SPACE )
           AND   ( TBL-PTKOHINF-MAX        >   ZERO  )
           AND   ( LNK-SYUBETU-NYUGAIKBN   =   "2"   )
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       TBL-PTKOHINF-MAX
                   IF    ( TBL-PTKOH-TEKSTYMD (IDXX)
                                           <=  COMB-TEKSTYMD           )
                   AND   ( COMB-TEKEDYMD   <=  TBL-PTKOH-TEKEDYMD(IDXX))
                       EVALUATE    TBL-PTKOH-KOHNUM (IDXX)
                           WHEN    "950"
                               MOVE    "1" TO  WRK-RECE61-RECEIPT-KBN
                           WHEN    "951"
                               MOVE    "2" TO  WRK-RECE61-RECEIPT-KBN
                           WHEN    "948"
                               MOVE    "3" TO  WRK-RECE61-RECEIPT-KBN
                           WHEN    "949"
                               MOVE    "4" TO  WRK-RECE61-RECEIPT-KBN
                       END-EVALUATE
                       MOVE    TBL-PTKOHINF-MAX
                                           TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    災害該当の有無（平成２８年１０月以降）
           IF    ( SYS-25-DISASTER-RECEIPT =   "1"     )
           AND   ( WRK-RECE61-SRYYM        >=   201610 )
               IF    ( WRK-RECE61-RECEIPT-KBN (5:1)
                                               =   SPACE )
               AND   ( TBL-PTKOHINF-MAX        >   ZERO  )
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       TBL-PTKOHINF-MAX
                       IF    ( TBL-PTKOH-TEKSTYMD (IDXX)
                                           <=  COMB-TEKSTYMD           )
                       AND   ( COMB-TEKEDYMD
                                           <=  TBL-PTKOH-TEKEDYMD(IDXX))
                           IF      TBL-PTKOH-KOHNUM (IDXX)
                                               =   "959"
                               IF      WRK-RECE61-HKNNUM   =   SPACE
                                   IF        TBL-PTKOH-FTNJANUM
                                                        (IDXX) (1:1)
                                                               =   "1"
                                       MOVE    "1"         TO
                                           WRK-RECE61-RECEIPT-KBN (5:1)
                                   END-IF
                               ELSE
                                   MOVE    "1"         TO
                                           WRK-RECE61-RECEIPT-KBN (5:1)
                               END-IF
                               MOVE    TBL-PTKOHINF-MAX
                                                   TO  IDXX
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
      *    患者レセプト分割情報の有無
           IF    ( WRK-RECE61-RECEIPT-KBN (2:3)
                                           =   SPACE )
           AND   ( TBL-PT-HDIST-DAY-MAX    >   ZERO  )
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       TBL-PT-HDIST-DAY-MAX
                   PERFORM VARYING IDX4    FROM    1   BY  1
                           UNTIL   IDX4    >       2
                       PERFORM VARYING IDX5    FROM    1   BY  1
                               UNTIL   IDX5    >       10
                               OR      TBL-PT-HDIST-DAY-HKNCOMBI
                                                       (IDXX IDX4 IDX5)
                                               =   ZERO
                         IF      TBL-PT-HDIST-DAY-DLTFLG
                                                       (IDXX IDX4 IDX5)
                                                       =   "1"
                           CONTINUE
                         ELSE
                           IF      COMB-HKNCOMBINUM    =
                                   TBL-PT-HDIST-DAY-HKNCOMBI
                                                       (IDXX IDX4 IDX5)
                               MOVE    TBL-PT-HDIST-DAY-DAY (IDXX)
                                                       TO
                                           WRK-RECE61-RECEIPT-KBN (2:2)
                               IF      IDX4            =   1
                                   MOVE    "1"         TO
                                           WRK-RECE61-RECEIPT-KBN (4:1)
                               ELSE
                                   MOVE    "2"         TO
                                           WRK-RECE61-RECEIPT-KBN (4:1)
                               END-IF
                               IF    ( TBL-PT-HDIST-DAY-SRYYM   (IDXX)
                                           =   COMB-TEKSTYMD (1:6) AND
                                       TBL-PT-HDIST-DAY-DAY (IDXX)
                                           =   COMB-TEKSTYMD (7:2)     )
                               OR    ( TBL-PT-HDIST-DAY-SRYYM   (IDXX)
                                           =   COMB-TEKEDYMD (1:6) AND
                                       TBL-PT-HDIST-DAY-DAY (IDXX)
                                           =   COMB-TEKEDYMD (7:2)     )
                                   MOVE    10              TO  IDX5
                                   MOVE    2               TO  IDX4
                                   MOVE    TBL-PT-HDIST-DAY-MAX
                                                           TO  IDXX
                               END-IF
                           END-IF
                         END-IF
                       END-PERFORM
                   END-PERFORM
               END-PERFORM
           END-IF
      *??
           DISPLAY "WRK-RECE61-RECEIPT-KBN=" WRK-RECE61-RECEIPT-KBN "#"
      *??
      *
      *    地方公費のみの場合は出力しない
           IF    ( WRK-RECE61-HKNNUM      =   SPACE )
           AND   ( WRK-RECE61-KOHID       =   ZERO  )
           AND   ( WRK-SEIKYU-KOHID (1 1) =   ZERO  )
               GO  TO  20019-FILE-SYORI-EXT
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       100
grpsys         IF      TBL-RECE61-HOSPNUM (IDXX)  =   ZERO
                   PERFORM 200192-RECE61-WRITE-SEC
                   MOVE    100             TO  IDXX
               ELSE
                   IF      TBL-RECE61-KEY (IDXX) =
                                           WRK-RECE61-KEY
                       PERFORM 200193-RECE61-REWRITE-SEC
                       MOVE    100               TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    公費単独の場合（生保、中国残留邦人等、自立支援）
           IF    ( COMB-HKNNUM         =   SPACE )
           AND   ( COMB-KOHHKNNUM (1)  =   "012"
                                       OR  "015"
                                       OR  "016"
                                       OR  "021"
                                       OR  "025" )
               IF      ( COMB-KOHHKNNUM (2)  =   SPACE )
                   PERFORM 200194-SEIHO-RECE61-WRITE-SEC
               ELSE
      *            沖縄県で精神障害者特別措置(121)の場合も対応
                   IF    ( LNK-SYUBETU-PREFNUM =   "47"  )
                   AND   ( COMB-KOHHKNNUM (2)  =   "121" )
                      PERFORM 200194-SEIHO-RECE61-WRITE-SEC
                   END-IF
               END-IF
           END-IF
           .
       20019-FILE-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    福岡県公費チェック処理
      *****************************************************************
       200191-PREFKBN-40-CHECK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-PREFKBN
                                           WRK-PREFNUM
      *
      *
      *    ２００６年１０月以降は公費付加情報より設定
           IF    ( LNK-SYUBETU-SRYYM   >  WRK-CONS-SRYYM-200609 )
               MOVE    "1"                 TO  WRK-PREFKBN
           ELSE
      *        県外扱いの保険者はレセプトへ記載しない
               IF      PTHKN-HKNJANUM (1:2)    =   "67"
                   MOVE    PTHKN-HKNJANUM (3:2)
                                           TO  WRK-PREFNUM
               ELSE
                   MOVE    PTHKN-HKNJANUM (1:2)
                                           TO  WRK-PREFNUM
               END-IF
               IF      WRK-PREFNUM         =   LNK-SYUBETU-PREFNUM
                   MOVE    "1"                 TO  WRK-PREFKBN
               ELSE
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       5
                           OR      WRK-1033-STYUKYMD (IDX2) =   SPACE
                       IF      WRK-1033-STYUKYMD   (IDX2)  <=  WRK-STYMD
                       AND     WRK-1033-EDYUKYMD   (IDX2)  >=  WRK-STYMD      
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
           END-IF
      *        
      *    県内扱いの場合、公費付加情報よりレセプト請求区分を取得
           IF      WRK-PREFKBN      =   SPACE
               MOVE    2                   TO  IDX1
           ELSE
               INITIALIZE                  ORCSKOHPLUS-AREA
grpsys         MOVE    COMB-HOSPNUM        TO  LNK-SKOHPLUS-HOSPNUM
               MOVE    LNK-SYUBETU-PREFNUM TO  LNK-SKOHPLUS-PREFNUM
               MOVE    COMB-PTID           TO  LNK-SKOHPLUS-PTID
               MOVE    COMB-HKNCOMBINUM    TO  LNK-SKOHPLUS-HKNCOMBI
               MOVE    LNK-SYUBETU-SRYYM   TO  LNK-SKOHPLUS-SRYYMD (1:6)
               MOVE    "01"                TO  LNK-SKOHPLUS-SRYYMD (7:2)
               CALL    "ORCSKOHPLUS"       USING
                                           ORCSKOHPLUS-AREA
grpsys                                     SPA-AREA
               IF      LNK-SKOHPLUS-RC =   ZERO
                   IF      LNK-SKOHPLUS-RECESKYKBN =   "1" OR  "3"
                                                           OR  "5"
                       MOVE    2                   TO  IDX1
                   ELSE
                       MOVE    1                   TO  IDX1
                   END-IF
               ELSE
                   MOVE    1                   TO  IDX1
               END-IF
           END-IF
      *
           .
       200191-PREFKBN-40-CHECK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       200192-RECE61-WRITE-SEC              SECTION.
      *
           PERFORM 2001921-KOHID-HENSYU-SEC
      *
           MOVE    COMB-HKNCOMBINUM   TO  WRK-RECE61-HKNCOMBI-O (1)
           MOVE    PTHKN-TEKSTYMD     TO  WRK-RECE61-TEKSTYMDX  (1)
           MOVE    PTHKN-TEKEDYMD     TO  WRK-RECE61-TEKEDYMDX  (1)
           IF    ( COMB-HKNNUM        =   SPACE )
           AND   ( COMB-KOH1HKNNUM    =   "020" 
                                      OR  "030"
                                      OR  "053" )
      *********MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMDX  (1)
      *********MOVE    COMB-TEKEDYMD       TO  WRK-RECE61-TEKEDYMDX  (1)
               INITIALIZE                      PTKOHINF-REC
               MOVE    WRK-RECE61-HOSPNUM  TO  PTKOH-HOSPNUM
               MOVE    WRK-RECE61-PTID     TO  PTKOH-PTID
               MOVE    COMB-KOHID (1)      TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               MOVE    PTKOH-TEKSTYMD      TO  WRK-RECE61-TEKSTYMDX  (1)
               MOVE    PTKOH-TEKEDYMD      TO  WRK-RECE61-TEKEDYMDX  (1)
           END-IF
      *    
      *    主保険情報
           IF      WRK-RECE61-HKNNUM  NOT =   SPACE
               EVALUATE    WRK-RECE61-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                   WHEN    WRK-CONS-KOUGAI-HKNNUM
                       CONTINUE
                   WHEN    OTHER
                       MOVE    PTHKN-HKNID TO  WRK-RECE61-HKNID
                       MOVE    PTHKN-HKNJANUM
                                           TO  WRK-RECE61-HKN-HKNJANUM
                       MOVE    PTHKN-HONKZKKBN
                                           TO  WRK-RECE61-HKN-HONKZKKBN
                       MOVE    PTHKN-HOJOKBN
                                           TO  WRK-RECE61-HKN-HOJOKBN
      *
      *                保険公費種別区分（自費保険判定用）
grpsys                 MOVE    LNK-SYUBETU-HOSPNUM
                                           TO  HKN-HOSPNUM
                       MOVE    WRK-RECE61-HKNNUM
                                           TO  HKN-HKNNUM
                       MOVE    WRK-SRYYMD  TO  HKN-TEKSTYMD
                                               HKN-TEKEDYMD
                       MOVE    "00"        TO  HKN-PAYKBN   
                       MOVE    HKNNUM-REC  TO  MCPDATA-REC
                       PERFORM 900-HKNNUM-INV-SEC
                       MOVE    HKN-HKNKOHSBTKBN
                                           TO  WRK-RECE61-HKNKOHSBTKBN
               END-EVALUATE
           END-IF
      *
      *    老人情報
           IF      WRK-RECE61-KOHID     >   ZERO
               INITIALIZE                       PTKOHINF-REC
grpsys         MOVE    WRK-RECE61-HOSPNUM   TO  PTKOH-HOSPNUM
               MOVE    WRK-RECE61-PTID      TO  PTKOH-PTID
               MOVE    WRK-RECE61-KOHID     TO  PTKOH-KOHID
               MOVE    PTKOHINF-REC         TO  MCPDATA-REC
               PERFORM 900-PTKOHINF-READ-SEC
               MOVE    PTKOH-PAYKBN         TO  WRK-RECE61-RJNPAYKBN
           END-IF    
      *
           MOVE    WRK-TEISEIKBN       TO  WRK-RECE61-TEISEIKBN
      *
           MOVE    WRK-RECE61-REC      TO  TBL-RECE61-OCC (IDXX)
      *
           .
       200192-RECE61-WRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理
      *****************************************************************
       200193-RECE61-REWRITE-SEC            SECTION.
      *
           MOVE    TBL-RECE61-OCC (IDXX) 
                                       TO  WRK-RECE61-REC
      *
           PERFORM 2001921-KOHID-HENSYU-SEC
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      WRK-RECE61-HKNCOMBI-O  (IDX)   =   ZERO
                   MOVE    COMB-HKNCOMBINUM    TO
                                       WRK-RECE61-HKNCOMBI-O (IDX)
                   MOVE    10                   TO  IDX
               ELSE
                   IF      COMB-HKNCOMBINUM    =
                                       WRK-RECE61-HKNCOMBI-O (IDX)
                       MOVE    10                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
      *    主保険情報
           IF      WRK-RECE61-HKNNUM  NOT =   SPACE
               EVALUATE    WRK-RECE61-HKNNUM
                   WHEN    WRK-CONS-ROUSAI-HKNNUM
                   WHEN    WRK-CONS-JIBAI-HKNNUM
                   WHEN    WRK-CONS-KOUGAI-HKNNUM
                       CONTINUE
                   WHEN    OTHER
                       MOVE    1                   TO  FLG-LASTHKNID
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       10
                           IF    ( WRK-RECE61-TEKSTYMDX (IDX)
                                                   =   PTHKN-TEKSTYMD )
                           AND   ( WRK-RECE61-TEKEDYMDX (IDX)
                                                   =   PTHKN-TEKEDYMD )
                               MOVE    10              TO  IDX
                           ELSE
                               IF      WRK-RECE61-TEKSTYMDX (IDX)
                                                       =   SPACE
                                   MOVE    PTHKN-TEKSTYMD  TO
                                           WRK-RECE61-TEKSTYMDX (IDX)
                                   MOVE    PTHKN-TEKEDYMD  TO
                                           WRK-RECE61-TEKEDYMDX (IDX)
                                   MOVE    10              TO  IDX
                               ELSE
                                   IF      WRK-RECE61-TEKSTYMDX(IDX)
                                                   >=  PTHKN-TEKSTYMD
                                       MOVE    ZERO    TO  FLG-LASTHKNID
                                   END-IF
                               END-IF
                           END-IF        
                       END-PERFORM
                       IF      FLG-LASTHKNID       =   1
                           MOVE    PTHKN-HKNID
                                           TO  WRK-RECE61-HKNID
                           MOVE    PTHKN-HKNJANUM
                                           TO  WRK-RECE61-HKN-HKNJANUM
                           MOVE    PTHKN-HONKZKKBN
                                           TO  WRK-RECE61-HKN-HONKZKKBN
                           MOVE    PTHKN-HOJOKBN
                                           TO  WRK-RECE61-HKN-HOJOKBN
      *
grpsys                     MOVE    LNK-SYUBETU-HOSPNUM
                                               TO  HKN-HOSPNUM
                           MOVE    WRK-RECE61-HKNNUM
                                               TO  HKN-HKNNUM
                           MOVE    WRK-SRYYMD  TO  HKN-TEKSTYMD
                                                   HKN-TEKEDYMD
                           MOVE    "00"        TO  HKN-PAYKBN   
                           MOVE    HKNNUM-REC  TO  MCPDATA-REC
                           PERFORM 900-HKNNUM-INV-SEC
                           MOVE    HKN-HKNKOHSBTKBN
                                               TO
                                               WRK-RECE61-HKNKOHSBTKBN
                       END-IF
               END-EVALUATE
           END-IF
      *
           IF      WRK-TEISEIKBN           =   "1"
               MOVE    "1"                 TO  WRK-RECE61-TEISEIKBN
           END-IF          
      *
           MOVE    WRK-RECE61-REC    TO  TBL-RECE61-OCC (IDXX)
           .
       200193-RECE61-REWRITE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費個別処理
      *****************************************************************
       2001921-KOHID-HENSYU-SEC              SECTION.
      *
           MOVE    ZERO                    TO  TBL-MAX
                                               FLG-ADDKBN 
      *
           MOVE    WRK-RECE61-KOHI-AREA    TO  WRK-KOHI-AREA
      *
           MOVE    1                       TO  IDX1   
      *
      *    公費の追加 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               MOVE    SPACE               TO  WRK-RECESKYKBN
      *
               IF      WRK-SEIKYU-KOHID (IDX1 IDX)    =   ZERO
                   MOVE    10                  TO  IDX
               ELSE    
      *            老人＋原爆のとき  
                   IF    ( WRK-RECE61-KOHID    NOT =   ZERO  )
                   AND   ( WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)  
                                                   =   "019" )
                       IF      WRK-RECE61-HKNNUM   =   "060"   OR  "067"
                           IF      SYS-2005-RJNGNBKKBN =   "1" OR  ZERO
                               MOVE    "4"         TO  WRK-RECESKYKBN
                           ELSE
                               MOVE    SPACE       TO  WRK-RECESKYKBN
                           END-IF
                       ELSE                
                           IF      SYS-2005-RJNGNBKKBN =   "2" OR  ZERO
                               MOVE    "4"         TO  WRK-RECESKYKBN
                           ELSE
                               MOVE    SPACE       TO  WRK-RECESKYKBN
                           END-IF
                       END-IF
                   ELSE
      *                長期のとき
                       IF      WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)  
                                           =   WRK-CONS-CHOUKI-HKNNUM
                                           OR  WRK-CONS-CHOUKI-HKNNUM2
                           MOVE    "4"         TO  WRK-RECESKYKBN
                       ELSE
                           MOVE    SPACE       TO  WRK-RECESKYKBN
                       END-IF
                   END-IF
                   PERFORM 20019211-KOHID-TBL-HENSYU-SEC
               END-IF      
           END-PERFORM
      *
      *    公費を追加しなかったとき
           IF      FLG-ADDKBN              =   ZERO
               GO  TO  2001921-KOHID-HENSYU-EXT
           END-IF
      *
      *    優先順位に並べ替え
           INITIALIZE                      WRK-RECE61-KOHI-AREA
           MOVE    ZERO                    TO  TBL-MAX
      *
           IF      WRK-RECE61-SRYYM    <   WRK-CONS-SRYYM-200804
               MOVE    1                   TO  IDYY
           ELSE
               IF      WRK-RECE61-SRYYM    <   WRK-CONS-SRYYM-201501
                   MOVE    2                   TO  IDYY
               ELSE
                   MOVE    3                   TO  IDYY
               END-IF
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-SRT-KOHHKNNUM-MAX
                   OR      WRK-SRT-KOHHKNNUM (IDYY IDX)
                                       =   SPACE
                   OR      TBL-MAX     =   10 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                   IF      WRK-SRT-KOHHKNNUM   (IDYY IDX)
                                       =   WRK-KOHHKNNUM-O (IDY)
                       ADD     1               TO  TBL-MAX
                       MOVE    WRK-KOHI-TBL    (IDY)   TO
                                           WRK-RECE61-KOHI-TBL (TBL-MAX)
                       INITIALIZE          WRK-KOHI-TBL    (IDY)
                       MOVE    10              TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    すべて並べ替えた時 
           IF      WRK-KOHHKNNUM-O (1)     =   SPACE
           AND     WRK-KOHHKNNUM-O (2)     =   SPACE
           AND     WRK-KOHHKNNUM-O (3)     =   SPACE
           AND     WRK-KOHHKNNUM-O (4)     =   SPACE
           AND     WRK-KOHHKNNUM-O (5)     =   SPACE
           AND     WRK-KOHHKNNUM-O (6)     =   SPACE
           AND     WRK-KOHHKNNUM-O (7)     =   SPACE
           AND     WRK-KOHHKNNUM-O (8)     =   SPACE
           AND     WRK-KOHHKNNUM-O (9)     =   SPACE
           AND     WRK-KOHHKNNUM-O (10)    =   SPACE
               GO  TO  2001921-KOHID-HENSYU-EXT
           END-IF
      *
      *    地方公費の並び替え
           PERFORM VARYING IDX FROM    1    BY  1
                   UNTIL   IDX >       9
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       10
                   IF      WRK-KOHHKNNUM-O (IDX)
                                           >   WRK-KOHHKNNUM-O (IDY)
                   OR      WRK-KOHHKNNUM-O (IDX) =   SPACE 
                       MOVE    WRK-KOHI-TBL(IDX) TO  WRK-KOHI
                       MOVE    WRK-KOHI-TBL(IDY) TO  WRK-KOHI-TBL(IDX)
                       MOVE    WRK-KOHI          TO  WRK-KOHI-TBL(IDY)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    地方公費の追加
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   10
                   OR      TBL-MAX     =   10 
               IF      WRK-KOHID-O (IDX)   >   ZERO
                   ADD     1                   TO  TBL-MAX
                   MOVE    WRK-KOHI-TBL (IDX)  TO 
                                           WRK-RECE61-KOHI-TBL(TBL-MAX)
      *            法別番号（福岡のみ）
                   IF    ( LNK-SYUBETU-PREFNUM =   "40"             )
                   AND   ( LNK-SYUBETU-SRYYM   <=
                                              WRK-CONS-SRYYM-200609 ) 
                       INITIALIZE                  HKNNUM-REC
grpsys                 MOVE   LNK-SYUBETU-HOSPNUM
                                               TO  HKN-HOSPNUM
                       MOVE   WRK-RECE61-KOHHKNNUM-O (TBL-MAX)
                                               TO  HKN-HKNNUM
                       MOVE    WRK-SRYYMD      TO  HKN-TEKSTYMD
                                                   HKN-TEKEDYMD
                       MOVE    "00"        TO  HKN-PAYKBN   
                       MOVE    HKNNUM-REC  TO  MCPDATA-REC
                       PERFORM 900-HKNNUM-INV-SEC
                       MOVE   HKN-HBTNUM   TO
                                           WRK-RECE61-KOHHBTNUM(TBL-MAX)
                   END-IF 
               END-IF    
           END-PERFORM
           .
       2001921-KOHID-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    公費情報退避処理
      *****************************************************************
       20019211-KOHID-TBL-HENSYU-SEC     SECTION.
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       10
               IF      WRK-KOHID-O (IDY)   =   ZERO
                   MOVE    WRK-SEIKYU-KOHID    (IDX1 IDX)
                                           TO  WRK-KOHID-O     (IDY)
                   MOVE    WRK-SEIKYU-KOHHKNNUM(IDX1 IDX)
                                           TO  WRK-KOHHKNNUM-O (IDY)
                   MOVE    WRK-RECESKYKBN  TO  WRK-RECESKYKBN-O(IDY)
                   MOVE    1               TO  FLG-ADDKBN
                   MOVE    10               TO  IDY
               ELSE 
                   IF      WRK-SEIKYU-KOHID (IDX1 IDX)
                                            =   WRK-KOHID-O  (IDY)
                       MOVE    10               TO  IDY
                   END-IF
               END-IF
           END-PERFORM
           .
       20019211-KOHID-TBL-HENSYU-EXT.
           EXIT.
      *            
      *****************************************************************
      *    出力処理（生保単独のときのみ）
      *****************************************************************
       200194-SEIHO-RECE61-WRITE-SEC              SECTION.
      *
           MOVE    WRK-RECE61-REC      TO  WRK-SEIHO-RECE61-REC
      *
           INITIALIZE                      WRK-RECE61-REC
           MOVE    WK-SH-RECE61-KEY    TO  WRK-RECE61-KEY
      *****MOVE    COMB-TEKSTYMD       TO  WRK-RECE61-TEKSTYMD
           INITIALIZE                      PTKOHINF-REC
           MOVE    WRK-RECE61-HOSPNUM  TO  PTKOH-HOSPNUM
           MOVE    WRK-RECE61-PTID     TO  PTKOH-PTID
           MOVE    COMB-KOHID (1)      TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           PERFORM 900-PTKOHINF-READ-SEC
           MOVE    PTKOH-TEKSTYMD      TO  WRK-RECE61-TEKSTYMD
      *    生活保護期間重複対応のため
           IF      COMB-KOHHKNNUM (1)  =   "012"
               MOVE    COMB-KOH1ID         TO  WRK-RECE61-KOH-KOHID
           END-IF
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       100
grpsys         IF      TBL-RECE61-HOSPNUM (IDXX)  =   ZERO
                   MOVE    PTKOH-TEKSTYMD      TO  PTHKN-TEKSTYMD
                   MOVE    PTKOH-TEKEDYMD      TO  PTHKN-TEKEDYMD
                   PERFORM 200192-RECE61-WRITE-SEC
                   MOVE    100             TO  IDXX
               ELSE
                   IF      TBL-RECE61-KEY (IDXX) =
                                           WRK-RECE61-KEY
                       PERFORM 200193-RECE61-REWRITE-SEC
                       MOVE    100               TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200194-SEIHO-RECE61-WRITE-EXT.
           EXIT.
      * 
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      LNK-SYUBETU-RC      =   ZERO
grpsys         IF      TBL-RECE61-HOSPNUM (1)  =   ZERO
      *********    MOVE    4               TO  LNK-SYUBETU-RC
                   CONTINUE 
               ELSE
                   MOVE    ZERO            TO  IDZZ
      *
                   IF    ( TBL-PT-HDIST-DAY-MAX
                                           =   ZERO )
                   AND   ( TBL-PTKOHINF-MAX
                                           =   ZERO )
                       MOVE    1               TO  ST-IDX
                       MOVE    SPACE           TO  WRK-RECEIPT-KBN
      *                出力処理
                       PERFORM 3001-RECESYUBETU-HENSYU-SEC
                   ELSE
                       MOVE    ZERO            TO  ST-IDX
                       PERFORM         UNTIL   ST-IDX  =   999
                           MOVE    SPACE           TO  WRK-RECEIPT-KBN
                           MOVE    ZERO            TO  ST-IDX
                           PERFORM VARYING IDXX    FROM    1   BY  1
                                   UNTIL   IDXX    >       100
                                   OR      TBL-RECE61-HOSPNUM (IDXX)
                                                   =   ZERO
                               IF      TBL-RECE61-SYOKBN (IDXX)
                                                   =   SPACE
                                   MOVE    TBL-RECE61-RECEIPT-KBN (IDXX)
                                                   TO  WRK-RECEIPT-KBN
                                   MOVE    IDXX    TO  ST-IDX
                                   MOVE    100     TO  IDXX
                               END-IF 
                           END-PERFORM
      *
                           IF      ST-IDX          >   ZERO
      *                        出力処理
                               PERFORM 3001-RECESYUBETU-HENSYU-SEC
                           ELSE
                               MOVE    999             TO  ST-IDX
                           END-IF
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
      *
           .
       300-END-EXT.
           EXIT. 
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       3001-RECESYUBETU-HENSYU-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-SEIHO
           INITIALIZE                      TBL-SEIHO-RECE61-AREA
                                           WRK-KOH-RECE61-REC
      *
           MOVE    ZERO                TO  WRK-SEIHO-FLG
      *    公費単独の退避（生保、中国残留邦人等、自立支援）
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       100
grpsys             OR      TBL-RECE61-HOSPNUM (IDXX)  =   ZERO
                   OR      FLG-SEIHO                  =   1
             IF      WRK-RECEIPT-KBN   =   TBL-RECE61-RECEIPT-KBN (IDXX)
               MOVE    TBL-RECE61-OCC (IDXX)   TO  WRK-RECE61-REC
      *
      *        公費単独の場合（精神入院、心身喪失以外）
               IF      WRK-RECE61-HKNNUM       =   SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (1)  = "020" OR  "030"
      *********                                                OR  "053"
                       CONTINUE
                   ELSE 
                       IF      WRK-RECE61-TEKSTYMD =   ZERO
      *                    公費単独の場合（生保、中国残留邦人等、自立支援）
                           IF    ( WRK-RECE61-KOHHKNNUM-O (1)
                                               =   "012"   OR  "025"
                                               OR  "015"   OR  "016"
                                               OR  "021"            )
      *                        同じ公費の公費単独の場合
                               IF    ( WRK-RECE61-KOHHKNNUM-O  (2)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (3)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (4)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (5)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (6)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (7)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (8)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (9)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (10)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                                   CONTINUE
                               ELSE
      *****************************IF    ( WRK-RECE61-KOHHKNNUM-O  (2)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************AND   ( WRK-RECE61-KOHHKNNUM-O  (3)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************AND   ( WRK-RECE61-KOHHKNNUM-O  (4)
      *****************************            =   "012"   OR  "025"
      *****************************            OR  "015"   OR  "016"
      *****************************            OR  "021"   OR  SPACE )
      *****************************    生保、中国残留邦人等、自立支援のみ
      *****************************    の公費単独の場合退避
      *****************************    MOVE    WRK-RECE61-REC      TO
      *****************************                WRK-KOH-RECE61-REC
      *****************************ELSE
                                       MOVE    1           TO  FLG-SEIHO
      *****************************END-IF
                               END-IF
                           ELSE
                               MOVE    1           TO  FLG-SEIHO
                           END-IF
                       ELSE
      *                    公費単独（生保、中国残留邦人等、自立支援）の場合退避
                           ADD     1        TO  TBL-SH-RE61-MAX
                           MOVE    WRK-RECE61-REC
                                            TO  TBL-SEIHO-RECE61-REC
                                              (TBL-SH-RE61-MAX)
                       END-IF
                   END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           IF      FLG-SEIHO           =   1
               INITIALIZE                  TBL-SEIHO-RECE61-AREA
           END-IF    
      *
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       100
grpsys             OR      TBL-RECE61-HOSPNUM (IDXX)  =   ZERO
             IF      WRK-RECEIPT-KBN   =   TBL-RECE61-RECEIPT-KBN (IDXX)
               MOVE    TBL-RECE61-OCC (IDXX)   TO  WRK-RECE61-REC
      *
               MOVE    ZERO            TO  FLG-WRITE 
      *
      *        公費単独の場合（精神入院、心身喪失以外）
               IF      WRK-RECE61-HKNNUM        =   SPACE
                   IF      WRK-RECE61-KOHHKNNUM-O (1)  = "020" OR  "030"
      *************                                            OR  "053"
                       CONTINUE
                   ELSE 
                       IF      WRK-RECE61-TEKSTYMD  =   ZERO
      *                    公費単独の場合（生保、中国残留邦人等、自立支援）
                           IF    ( WRK-RECE61-KOHHKNNUM-O (1)
                                               =   "012"   OR  "025"
                                               OR  "015"   OR  "016"
                                               OR  "021"            )
      *                        同じ公費の公費単独の場合
                               IF    ( WRK-RECE61-KOHHKNNUM-O  (2)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (3)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (4)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (5)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (6)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (7)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (8)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (9)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
                                                           OR  SPACE )
                               AND   ( WRK-RECE61-KOHHKNNUM-O  (10)
                                                    =
                                           WRK-RECE61-KOHHKNNUM-O (1) 
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
                               AND   ( WRK-RECE61-KOHHKNNUM-O (1)
                                                   NOT =   "053" )
                                   CONTINUE
                               ELSE 
                                  MOVE    1            TO  FLG-WRITE
                               END-IF
                           END-IF
                       ELSE
                           MOVE    1        TO  FLG-WRITE 
                       END-IF
                  END-IF
               END-IF
      *
      *    社保で公費併用の場合（生保、中国残留邦人等、自立支援、児童保護）
               IF    ( WRK-RECE61-HKNNUM
                                       NOT =   SPACE                 )
               AND   ( WRK-RECE61-KOHHKNNUM-O (1)
                                           =   "012"   OR  "025"   OR 
                                               "015"   OR  "016"   OR
                                               "021"   OR  "053"     )
               AND   ( WRK-RECE61-KOHHKNNUM-O (2)
                                           =   SPACE   OR  "972"   OR
                                               "974"                 )
                   IF      WRK-RECE61-HKNNUM   =
                                               "060"   OR  "067"   OR
                                               "068"   OR  "069"   OR
                                               "039"   OR  "040"
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDA FROM    1   BY  1
                               UNTIL   IDA >       TBL-SH-RE61-MAX
      *                  レセプト分割区分毎にまとめる
                         IF      WRK-RECE61-RECEIPT-KBN    =
                                   TBL-SH-RE61-RECEIPT-KBN (IDA)
                           PERFORM VARYING IDYY    FROM    1   BY  1
                                   UNTIL   IDYY    >       10
                                   OR      WRK-RECE61-KOHID-O (IDYY)
                                                   =       ZERO
                               IF      TBL-SH-RE61-KOHID-O (IDA 1)
                                           =   WRK-RECE61-KOHID-O (IDYY)
                                   MOVE    1           TO
                                           TBL-SH-RE61-PRTFLG(IDA)
                                   MOVE    1           TO  WRK-SEIHO-FLG
                                   PERFORM VARYING IDB FROM    1   BY  1
                                           UNTIL   IDB >       10
                                       IF      WRK-RECE61-HKNCOMBI-O
                                                               (IDB)
                                                           =   ZERO
                                           MOVE
                                             TBL-SH-RE61-HKNCOMBI-O
                                                              (IDA 1)
                                                           TO
                                               WRK-RECE61-HKNCOMBI-O
                                                               (IDB)
                                       END-IF
                                       IF      WRK-RECE61-HKNCOMBI-O
                                                               (IDB)
                                                   =
                                             TBL-SH-RE61-HKNCOMBI-O
                                                              (IDA 1)
                                           MOVE    10          TO  IDB
                                       END-IF
                                   END-PERFORM    
                                   MOVE    10              TO  IDYY
                                   MOVE    TBL-SH-RE61-MAX
                                                           TO  IDA
                               END-IF
                           END-PERFORM
                         END-IF
                       END-PERFORM
                   END-IF                     
               END-IF
      *
      *        対象外フラグ
               IF      FLG-WRITE            =   ZERO
      *            レセプト情報設定処理
                   PERFORM 30011-SYUBETUGET-CALL-SEC
               END-IF
      *
               MOVE    "1"                  TO  TBL-RECE61-SYOKBN (IDXX) 
             END-IF
           END-PERFORM
      *
      *    公費単独レセの出力処理
           IF    ( WRK-SEIHO-FLG           =   ZERO )
           AND   (  WRK-KO-RE61-HOSPNUM    >   ZERO )
      *        主保険とのまとめがなく生保、中国残留邦人等、自立支援以外との
      *        公費単独の場合
               MOVE    WRK-KOH-RECE61-REC  TO  WRK-RECE61-REC
      *        レセプト情報設定処理
               PERFORM 30011-SYUBETUGET-CALL-SEC
           ELSE
               IF      TBL-SH-RE61-MAX         >   ZERO
                   PERFORM VARYING IDXX    FROM    1   BY  1
                           UNTIL   IDXX    >       TBL-SH-RE61-MAX
                       IF      TBL-SH-RE61-PRTFLG (IDXX)
                                               =   1
                           CONTINUE
                       ELSE
                           MOVE    TBL-SEIHO-RECE61-REC (IDXX)
                                                   TO  WRK-RECE61-REC
      *                    レセプト情報設定処理
                           PERFORM 30011-SYUBETUGET-CALL-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
       3001-RECESYUBETU-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    レセプト情報設定処理
      *****************************************************************
       30011-SYUBETUGET-CALL-SEC              SECTION.
      *
           ADD     1                    TO  IDZZ 
      *    審査支払機関
           EVALUATE    WRK-RECE61-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
                   MOVE    3   TO  LNK-SYUBETU-TEISYUTUSAKI (IDZZ)
               WHEN    WRK-CONS-JIBAI-HKNNUM
                   MOVE    4   TO  LNK-SYUBETU-TEISYUTUSAKI (IDZZ)
               WHEN    WRK-CONS-KOUGAI-HKNNUM
                   MOVE    5   TO  LNK-SYUBETU-TEISYUTUSAKI (IDZZ)
               WHEN    "060"
               WHEN    "067"
               WHEN    "068"
               WHEN    "069"
                   MOVE    2   TO  LNK-SYUBETU-TEISYUTUSAKI (IDZZ)
               WHEN    "040" 
               WHEN    "039" 
                   MOVE    6   TO  LNK-SYUBETU-TEISYUTUSAKI (IDZZ)
               WHEN    OTHER
                   MOVE    1   TO  LNK-SYUBETU-TEISYUTUSAKI (IDZZ)
           END-EVALUATE
      *    保険者番号
           MOVE    WRK-RECE61-HKNJANUM 
                                   TO  LNK-SYUBETU-HKNJANUM (IDZZ)
      *    補助区分
           MOVE    WRK-RECE61-HOJOKBN
                                   TO  LNK-SYUBETU-HOJOKBN  (IDZZ)
      *    公費ＩＤ
           MOVE    WRK-RECE61-KOH-KOHID
                                   TO  LNK-SYUBETU-KOHID    (IDZZ)
      *    適用開始年月日
           MOVE    WRK-RECE61-TEKSTYMD
                                   TO  LNK-SYUBETU-TEKSTYMD (IDZZ)
      *    分割区分
           MOVE    WRK-RECE61-RECEIPT-KBN 
                                   TO  LNK-SYUBETU-RECEIPT-KBN  (IDZZ)
      *    保険番号（主保険）
           MOVE    WRK-RECE61-HKNNUM
                                   TO  LNK-SYUBETU-HKNNUM   (IDZZ)
      *    保険組合せ
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
               MOVE    WRK-RECE61-HKNCOMBI-O (IDX)
                                   TO  LNK-SYUBETU-HKNCOMBI (IDZZ IDX)
           END-PERFORM
      *        
      *    後期高齢非該当の有無設定
           MOVE    ZERO            TO  WRK-KOUKI-FLG
      *
           IF    WRK-NENREI        >=  70
               IF    ( WRK-RECE61-HKNNUM
                                       =   SPACE   OR  "040"   OR  "039"
                                       OR  "067"   OR  "069"           )
               OR    ( WRK-RECE61-HKNNUM (1:2)
                                       =   "98"    OR  "90"    OR  "91")
                   CONTINUE
               ELSE
                   IF   (  WRK-RECE61-HKNNUM   =   "060"   OR  "068" )
                   OR   (( WRK-RECE61-HKNNUM
                                           NOT =   "060"   AND "068")
                           AND
                         ( WRK-RECE61-HOJOKBN  =   SPACE            ))
                       MOVE      LNK-SYUBETU-SRYYM    TO  WRK-TEKSTYYMM
                       IF   WRK-RECE61-TEKSTYMDX (1) (1:6)  >=
                                                      LNK-SYUBETU-SRYYM
                            MOVE  WRK-RECE61-TEKSTYMDX (1) (7:2)
                                                       TO  WRK-TEKSTDD
                       ELSE
                            MOVE  01                   TO  WRK-TEKSTDD
                       END-IF
      *
                       INITIALIZE                      PTKOHINF-REC
                       MOVE    LNK-SYUBETU-HOSPNUM TO  PTKOH-HOSPNUM
                       MOVE    LNK-SYUBETU-PTID    TO  PTKOH-PTID
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
           END-IF
      *
      *    レセプト種別
           PERFORM 30011-RECESYUBETU-SET-SEC
           .
       30011-SYUBETUGET-CALL-EXT.
           EXIT. 
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       30011-RECESYUBETU-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WK-RECESYUBETU-AREA
           INITIALIZE                      WK-RECESYUBETU-AREA
      *
      *    労災・自賠責のときレセプト種別の設定はなし
           IF      WRK-RECE61-HKNNUM   =   WRK-CONS-ROUSAI-HKNNUM  OR
                                           WRK-CONS-JIBAI-HKNNUM 
               GO  TO  30011-RECESYUBETU-SET-EXT
           END-IF
      *
      *    公害のときレセプト種別の設定はなし
           IF      WRK-RECE61-HKNNUM   =   WRK-CONS-KOUGAI-HKNNUM
               GO  TO  30011-RECESYUBETU-SET-EXT
           END-IF
      *
      *    保険変更後、訂正していないとき
      *    保険組合せがないとき
           IF      WRK-RECE61-TEISEIKBN
                                   =   "1" OR  "9"
               MOVE    ZERO        TO  LNK-SYUBETU-RECESYUBETU  (IDZZ)
                                       LNK-SYUBETU-RECESYUBETU-Y(IDZZ)
               GO  TO  30011-RECESYUBETU-SET-EXT
           END-IF
      *
      *    公費が４つを超える時
           IF      WRK-RECE61-KOHID-O (5)
                                   >   ZERO
               MOVE    ZERO        TO  LNK-SYUBETU-RECESYUBETU  (IDZZ)
                                       LNK-SYUBETU-RECESYUBETU-Y(IDZZ)
               GO  TO  30011-RECESYUBETU-SET-EXT
           END-IF
      *     
      *    自費のとき
           IF     WRK-RECE61-HKNKOHSBTKBN
                                   =   "8"
               MOVE    "8888"      TO  LNK-SYUBETU-RECESYUBETU  (IDZZ)
               MOVE    8888        TO  LNK-SYUBETU-RECESYUBETU-Y(IDZZ)
               GO  TO  30011-RECESYUBETU-SET-EXT
           END-IF
      *
      *    神奈川地方公費対応
           IF    ( LNK-SYUBETU-PREFNUM     =   "14" )
           AND   ( LNK-SYUBETU-TEISYUTUSAKI (IDZZ)
                                       =   "2"      )
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       4
                   IF      WRK-RECE61-KOHHKNNUM-O  (IDX)
                                           =   "180" OR "280"
                       INITIALIZE                       PTKOHINF-REC
grpsys                 MOVE    LNK-SYUBETU-HOSPNUM TO  PTKOH-HOSPNUM
                       MOVE    LNK-SYUBETU-PTID    TO  PTKOH-PTID
                       MOVE    WRK-RECE61-KOHID-O (IDX)
                                                    TO  PTKOH-KOHID
                       MOVE    PTKOHINF-REC         TO  MCPDATA-REC
                       PERFORM 900-PTKOHINF-READ-SEC
                       IF      PTKOH-FTNJANUM  =   SPACE
                           MOVE   "4"              TO
                                           WRK-RECE61-RECESKYKBN-O (IDX)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    種別設定
           INITIALIZE                  ORCSSYUBETUGETAREA
           MOVE    1               TO  LNK-SYUBETUGET-SYOFLG
           MOVE    LNK-SYUBETU-NYUGAIKBN
                                   TO  LNK-SYUBETUGET-NYUGAIKBN
           MOVE    LNK-SYUBETU-SRYYM
                                   TO  LNK-SYUBETUGET-SRYYM
           MOVE    WRK-RECE61-HKNNUM
                                   TO  LNK-SYUBETUGET-HKNNUM
           MOVE    WRK-RECE61-HKN-HONKZKKBN
                                   TO  LNK-SYUBETUGET-HKN-HONKZKKBN
           MOVE    WRK-RECE61-HOJOKBN
                                   TO  LNK-SYUBETUGET-HKN-HOJOKBN
           MOVE    WRK-RECE61-HKNKOHSBTKBN
                                   TO  LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
           MOVE    WRK-NENREI      TO  LNK-SYUBETUGET-AGE
           MOVE    AGECHK-SYUGAKUKBN
                                   TO  LNK-SYUBETUGET-SYUGAKUKBN
           MOVE    LNK-SYUBETU-PREFNUM
                                   TO  LNK-SYUBETUGET-PREFNUM
           IF      WRK-RECE61-KOHID >   ZERO
               MOVE    "027"       TO  LNK-SYUBETUGET-RJNHKNNUM
               MOVE    WRK-RECE61-RJNPAYKBN
                                   TO  LNK-SYUBETUGET-RJNPAYKBN
           END-IF
           MOVE    ZERO            TO  IDYY
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
               IF    ( WRK-RECE61-KOHHKNNUM-O  (IDX)
                                           NOT =   SPACE )
               AND   ( WRK-RECE61-KOHHKNNUM-O  (IDX)
                                           NOT =   "027" )
                   ADD     1                               TO  IDYY
                   MOVE    WRK-RECE61-KOHHKNNUM-O  (IDX)   TO
                                       LNK-SYUBETUGET-KOHNUM     (IDYY)
                   MOVE    WRK-RECE61-KOHHBTNUM (IDX)      TO
                                       LNK-SYUBETUGET-KOHHBTNUM  (IDYY)
                   IF      WRK-RECE61-RECESKYKBN-O (IDX) 
                                       =   SPACE
                       MOVE    "1"         TO
                                       LNK-SYUBETUGET-RECESKYKBN (IDYY)
                   END-IF
               END-IF     
           END-PERFORM
           IF      WRK-KOUKI-FLG   =   1
               MOVE    SPACE           TO  LNK-SYUBETUGET-HKN-HOJOKBN
           END-IF
           MOVE    LNK-SYUBETU-BIRTHDAY
                                   TO  LNK-SYUBETUGET-BIRTHDAY
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
               MOVE    LNK-SYUBETUGET-SRYYM
                                           TO  WRK-TEKSTYYMM
               IF      WRK-RECE61-TEKSTYMDX (1) (1:6)
                                           >=  LNK-SYUBETU-SRYYM
                    MOVE  WRK-RECE61-TEKSTYMDX (1) (7:2)
                                               TO  WRK-TEKSTDD
               ELSE
                    MOVE  01                   TO  WRK-TEKSTDD
               END-IF
      *
               INITIALIZE                      PTKOHINF-REC
               MOVE    LNK-SYUBETU-HOSPNUM TO  PTKOH-HOSPNUM
               MOVE    LNK-SYUBETU-PTID    TO  PTKOH-PTID
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
                                       TO  LNK-SYUBETU-RECESYUBETU(IDZZ)
           MOVE    LNK-SYUBETUGET-RECESYUBETU-Y
                                       TO  LNK-SYUBETU-RECESYUBETU-Y
                                                                  (IDZZ)
      *??
      *         DISPLAY "SYUBETU=" LNK-SYUBETU-RECESYUBETU(IDZZ)
      *??
      *         
           .
        30011-RECESYUBETU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院チェック処理
      *****************************************************************
       500-ORCSNYUINDAY-CHECK-SEC          SECTION.
      *
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    LNK-SYUBETU-HOSPNUM TO  LNK-NYUINDAY-HOSPNUM
           MOVE    LNK-SYUBETU-PTID    TO  LNK-NYUINDAY-PTID
           MOVE    LNK-SYUBETU-SRYYM   TO  LNK-NYUINDAY-SANTEIYM
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC     >   0
               DISPLAY "LNK-NYUINDAY-RC=" LNK-NYUINDAY-RC
               GO  TO  500-ORCSNYUINDAY-CHECK-EXT
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       31
               IF      LNK-NYUINDAY-DOUJITSU-KBN (IDXX)
                                               =   "1"
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL   IDX1    >   10
      *
      *                保険組合せ追加
                       IF      WRK-COMB-HKNCOMBINUM (IDX1)
                                                   =   ZERO
                           INITIALIZE                      HKNCOMBI-REC
                           MOVE    LNK-SYUBETU-HOSPNUM TO  COMB-HOSPNUM
                           MOVE    LNK-SYUBETU-PTID    TO  COMB-PTID
                           MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                                       TO
                                                       COMB-HKNCOMBINUM 
                           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                           PERFORM 910-HKNCOMBI-INV-SEC
      *                    （対象外の組合せは１をセット）
                           IF      FLG-HKNCOMBI    =   ZERO
                               MOVE    HKNCOMBI-REC    TO
                                               WRK-HKNCOMBI-REC(IDX1)
                           ELSE
                               MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                               (IDXX)
                                                       TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                               MOVE    "1"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           END-IF
                       END-IF
      *
      *                適用範囲かチェック（範囲外のとき２をセット）
                       IF      LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                               =
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                           IF      WRK-HKNCOMBI-SYOKBN (IDX1)
                                               NOT =   SPACE
                               CONTINUE                    
                           ELSE     
                               MOVE    WRK-STYMD       TO  WRK-YMD
                               MOVE    IDXX            TO  WRK-DD
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
               END-IF
           END-PERFORM
      *
           .
       500-ORCSNYUINDAY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    施設入所、災害該当情報処理
      *****************************************************************
       500-SISETU-HENSYU-SEC             SECTION.
      *
           INITIALIZE                      TABLE-PTKOHINF-AREA
      *
      *    ９５９（災害該当）
           IF    ( SYS-25-DISASTER-RECEIPT =   "1"     )
           AND   ( LNK-SYUBETU-SRYYM       >=   201610 )
               INITIALIZE                      PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    LNK-SYUBETU-PTID    TO  PTKOH-PTID
               MOVE    "959"               TO  PTKOH-KOHNUM
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
                   IF    ( PTKOH-SAKJOKBN      =   SPACE      )
                   AND   ( PTKOH-TEKSTYMD      >=  "20161001" )
                       ADD     1               TO  TBL-PTKOHINF-MAX
                       MOVE    PTKOHINF-REC    TO  TBL-PTKOHINF-REC
                                                   (TBL-PTKOHINF-MAX)
      *
                       DISPLAY "   TBL-PTKOHINF-MAX=" TBL-PTKOHINF-MAX
                           " KOHNUM="
                                   TBL-PTKOH-KOHNUM   (TBL-PTKOHINF-MAX)
                           " ST="  TBL-PTKOH-TEKSTYMD (TBL-PTKOHINF-MAX)
                           " ED="  TBL-PTKOH-TEKEDYMD (TBL-PTKOHINF-MAX)
                   END-IF
      *
                   MOVE    "tbl_ptkohinf"   TO  MCP-TABLE
                   MOVE    "key4"           TO  MCP-PATHNAME
                   PERFORM  900-PTKOHINF-READ-N-SEC
               END-PERFORM
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *??
           DISPLAY "TBL-PTKOHINF-MAX=" TBL-PTKOHINF-MAX 
      *??
      *
      *    入院外のみ対象
           IF      LNK-SYUBETU-NYUGAIKBN   =   "1"
               GO  TO  500-SISETU-HENSYU-EXT
           END-IF
      *
      *
           PERFORM VARYING IDZ4    FROM    1   BY  1
                   UNTIL   IDZ4    >       4
               INITIALIZE                      PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    LNK-SYUBETU-PTID    TO  PTKOH-PTID
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
                   IF    ( PTKOH-TEKSTYMD(1:6) <=  LNK-SYUBETU-SRYYM   )
                   AND   ( LNK-SYUBETU-SRYYM   <=  PTKOH-TEKEDYMD (1:6))
                   AND   ( PTKOH-SAKJOKBN      =   SPACE               )
                       ADD     1               TO  TBL-PTKOHINF-MAX
                       MOVE    PTKOHINF-REC    TO  TBL-PTKOHINF-REC
                                                   (TBL-PTKOHINF-MAX)
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
           MOVE    LNK-SYUBETU-PTID    TO  PT-HDIST-DAY-PTID
           MOVE    LNK-SYUBETU-SRYYM   TO  PT-HDIST-DAY-SRYYM
           MOVE    "1"                 TO  PT-HDIST-DAY-NYUGAIKBN
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
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *     
      *****************************************************************
      *    システム管理マスタＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys         PERFORM 900-DBFETCH-SEC
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
      *    診療行為マスタＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
grpsys         PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N-HKNCOMBI
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
grpsys         PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
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
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
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
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報マスタ読込
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTKOHINF
                   MOVE    MCPDATA-REC         TO  PTKOHINF-REC
                   IF      PTKOH-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTKOHINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTKOHINF
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
      *    保険番号読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   INITIALIZE                  HKNNUM-REC
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HKNNUM-INV-EXT.
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
