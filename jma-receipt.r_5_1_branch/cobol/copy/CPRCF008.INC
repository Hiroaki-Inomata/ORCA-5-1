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
      ************************************************************
      *    レセプト明細書     COPY (CPRCF008.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  適用開始年月日の追加
      *  01.01.01    MCC-藤原     02.01.29  ファイルレイアウト変更
      *  01.01.02    MCC-藤原     02.05.16  個別の時複数診療月で作成するために
      *                                     診療年月を追加
      *  01.01.03    MCC-小豆沢   02.05.27  社保の出力順変更用に保険区分追加
      *                                     １：政府　　　２：船員　３：日雇
      *                                     ４：特例日雇　５：共済　６：組合
      *                                     ７：自衛官　　８：特定組合
      *  01.01.04    MCC-藤原     02.07.01  帳票データX(10000)に変更
      *                                     RECENUMをＫＥＹ項目に変更      
      *
      *  01.02.01    NACL-藤原    02.09.26  帳票ＩＤ・入外区分の追加
      *
      *  02.06.01    NACL-藤原    05.07.13  検索用項目に連番を追加
      *
      *  02.07.02    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *                                     在総診・在医総
      *                                     長期（長期高額療養費）  
      *                                     県内県外印刷区分
      *                                     保険者印刷区分
      *                                     レセプト種別印刷区分
      *                                     主科コード
      *                                     保険者番号（検索用）
      *
      *  03.00.01    NACL-藤原    06/05/29  印刷時のプリンター指定対応
      *                                     プリンタ名追加
      *                                     連番の長さ変更
      *
      *  03.02.01    NACL-藤原    06/08/31  主科用出力順識別番号追加
      *
      *  03.02.01    NACL-藤原    07/03/01  国保編綴詳細設定対応
      *                                     種別出力順
      *                                     グループ設定１
      *                                     本人家族区分
      *                                     公費出力順 
      *                                     種別内出力順１
      *                                     グループ設定２
      *                                     種別内出力順２
      *                                     出力順 
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.03.01    NACL-藤原    08/06/19  医師別出力対応
      *                                     医師コード、医師別診療科
      *
      *  04.05.01    NACL-藤原    09/06/11  病棟別出力対応
      *
      *  04.07.01    NACL-藤原    12/12/03  ターミナルデジットによる出力対応
      *************************************************************
       01  RECE08-REC.
      *ＫＥＹ
           03  RECE08-KEY.
      *医療機関ＩＤ
               05  RECE08-HOSPNUM          PIC 9(2).
      *帳票ＩＤ（検索用ＫＥＹ）
               05  RECE08-PRTID            PIC  X(04).
      *診療年月（検索用ＫＥＹ　一括：診療年月、個別：ゼロ）
               05  RECE08-RECEYM           PIC  X(06).
      *作成日
               05  RECE08-CREYMD           PIC  X(08).
      *作成時間
               05  RECE08-CREHMS           PIC  X(06).
      *審査支払機関
               05  RECE08-TEISYUTUSAKI     PIC  X(01).
      *主科コード
               05  RECE08-SRYKA            PIC  X(02).
      *県内県外印刷区分（国保のみ使用）
               05  RECE08-PREFKBN-SRT      PIC  X(02).
      *保険者印刷区分（国保のみ使用）
               05  RECE08-HKNJANUM-SRT     PIC  X(01).
      *保険者番号        
               05  RECE08-HKNJANUM         PIC  X(08).
      *適用開始年月日
               05  RECE08-TEKSTYMD         PIC  X(08). 
      *レセプト種別印刷区分
               05  RECE08-RECESYUBETU-SRT  PIC  X(03).
      *レセプト種別
               05  RECE08-RECESYUBETU      PIC  X(04).
      *県内・県外区分（１：県内　２：県外　）
               05  RECE08-PREFKBN          PIC  X(01).
      *県番号
               05  RECE08-PREFNUM          PIC  X(02).
      *患者番号
               05  RECE08-PTNUM            PIC  X(20).
      *診療年月
               05  RECE08-SRYYM            PIC  X(06).
      *連番
               05  RECE08-RENNUM           PIC  9(03).
      *連番号
               05  RECE08-RECENUM          PIC  9(06).
      *保険区分（社保提出時のみ）
           03  RECE08-HKNKBN               PIC  X(01).
      *患者カナ氏名
           03  RECE08-KANANAME             PIC  X(100).
      *出力順識別番号
           03  RECE08-PRTJYUN              PIC  9(02).
      *出力順連番
           03  RECE08-PRTRENNUM            PIC  9(06).
      *入外区分
           03  RECE08-NYUGAIKBN            PIC  X(01).
      *高額点数
           03  RECE08-KOUGAKU              PIC  X(01).
      *在総診・在医総
           03  RECE08-ZAITAKU              PIC  X(01).
      *長期（長期高額療養費）  
           03  RECE08-CHOUKI               PIC  X(01).
      *検索用保険者番号        
           03  RECE08-HKNJANUM-S           PIC  X(08).
      *
           03  RECE08-SYUBETU-SRT1         PIC S9(9)   BINARY.
      *
           03  RECE08-GRP-NO1              PIC 9(3).
      *
           03  RECE08-HONKZKKBN            PIC X(1).
      *
           03  RECE08-KOUHI-NO             PIC S9(3).
      *
           03  RECE08-SYUBETU-UNDER-1.
               05  RECE08-SYUBETU-UNDER-1-1
                                           PIC X(40).
               05  RECE08-SYUBETU-UNDER-1-2
                                           PIC X(40).
      *
           03  RECE08-GRP-NO2              PIC X(3).
      *
           03  RECE08-SYUBETU-UNDER-2      PIC X(40).
      *
           03  RECE08-SYUBETU-SRT2         PIC S9(9)   BINARY.
      *医師別情報
           03  RECE08-DRCD-AREA.
      *医師コード
               05  RECE08-DRCD             PIC  X(05).
      *医師別診療科
               05  RECE08-DRCD-SRYKA       PIC  X(02).
      *ｓｏｒｔ
           03  RECE08-SORT-1               PIC  X(10).
      *ｓｏｒｔ２
           03  RECE08-SORT-2               PIC  X(50).
      *帳票ＩＤ
           03  RECE08-FORMID               PIC  X(20).
      *帳票データ
           03  RECE08-PRTDATA              PIC  X(10000).
      *プリンタ名
           03  RECE08-PRTNM                PIC  X(16).
      *端末ＩＤ
           03  RECE08-TERMID               PIC  X(64).
      *オペレータＩＤ
           03  RECE08-OPID                 PIC  X(16).
      *主科用出力順識別番号
           03  RECE08-PRTJYUN-SYUKA        PIC  9(4).
      *検索用項目
           03  RECE08-STKANANAME           PIC  X(100).
           03  RECE08-EDKANANAME           PIC  X(100).
           03  RECE08-STRENNUM             PIC  9(02).
           03  RECE08-EDRENNUM             PIC  9(02).
