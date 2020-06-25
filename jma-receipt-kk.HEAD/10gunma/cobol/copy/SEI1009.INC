      *******************************************************************
      * Project code name "ORCA"
      * 泣板筛洁レセプトソフト∈JMA standard receipt software∷
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
      *          板闻锐懒滇今 COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  コピ〖剁饯赖旺悟
      * Maj/Min/Rev  饯赖荚  泣烧        柒推
      *  01.00.01    等李    2009/08/24  KEYを恃构
      *  01.00.02    等李    2009/08/28  MF100-YOUSIKI纳裁
      *  01.00.03    等李    2010/07/26  瘦副爬眶纳裁
      *  01.00.04    等李    2010/10/04  MF100-TOTALTEN2纳裁
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *板闻怠簇ＩＤ
      *         05  MF100-HOSPID          PIC  X(24).
               05  MF100-HOSPNUM         PIC  9(02).
      *壳闻彩
      *         05  MF100-SRYKA           PIC  X(02).
      *辉漠录戎规
      *         05  MF100-CITYNUM         PIC  X(02).
               05  MF100-CITYNUM2        PIC  X(06).
      *蘑杉屯及(１♂奠屯及、２♂糠屯及)
               05  MF100-YOUSIKI         PIC  9(01).
      *寥圭せ惰尸
               05  MF100-COMBIKBN        PIC  X(02).
      *ソ〖トキ〖
               05  MF100-SORTKEY         PIC  9(10).
      *戏客瘦副≥瘦副戎规
               05  MF100-RJNHKNNUM-KEY   PIC  X(03).
      *肩瘦副≥惦烧充圭
               05  MF100-KYURATE-KEY     PIC  9(03).
      *给锐≥砷么荚戎规
               05  MF100-FTNJANUM        PIC  X(08).
      *给锐≥减惦荚戎规
               05  MF100-JKYSNUM         PIC  X(20).
      *吹荚戎规
               05  MF100-PTNUM           PIC  X(20).
      *瘦副荚戎规
               05  MF100-HKNJANUM-KEY    PIC  X(08).
      *努脱倡幌钳奉泣
               05  MF100-TEKSTYMD        PIC  X(08). 
      *掐嘲惰尸
               05  MF100-NYUGAIKBN       PIC  X(01).
      *壳闻钳奉
               05  MF100-SRYYM           PIC  X(06).
      *壳闻彩
           03  MF100-SRYKA               PIC  X(02).
      *吹荚ＩＤ
           03  MF100-PTID                PIC  9(10).
      *戳机会叹
           03  MF100-NAME                PIC  X(100).
      *栏钳奉泣
           03  MF100-BIRTHDAY            PIC  9(08).
      *拉侍
           03  MF100-SEX                 PIC  9(01).
      *辉漠录戎规
      *     03  MF100-CITYNUM2            PIC  X(06).
           03  MF100-CITYNUM             PIC  X(02).
      *肩瘦副攫鼠
           03  MF100-HKN.
      *肩瘦副≥瘦副ＩＤ
               05  MF100-HKNID           PIC  9(10).
      *肩瘦副≥瘦副戎规
               05  MF100-HKNNUM          PIC  X(03).
      *肩瘦副≥恕侍戎规
               05  MF100-HKNHBTNUM       PIC  X(02).
      *肩瘦副≥瘦副荚戎规
               05  MF100-HKNJANUM        PIC  X(08).
      *肩瘦副≥塑客踩虏惰尸
               05  MF100-HONKZKKBN       PIC  X(01).
      *肩瘦副≥输锦惰尸
               05  MF100-HOJOKBN         PIC  X(01).
      *肩瘦副≥费鲁惰尸
               05  MF100-CONTKBN         PIC  X(01).
      *肩瘦副≥淡规
               05  MF100-KIGO            PIC  X(80).
      *肩瘦副≥戎规
               05  MF100-NUM             PIC  X(80).
      *肩瘦副≥惦烧充圭
               05  MF100-KYURATE         PIC  9(03).
      *肩瘦副≥沮汤今戎规
               05  MF100-CERTNUM         PIC  X(03).
      *戏客瘦夫攫鼠
           03  MF100-RJNHKN.
      *戏客瘦副≥瘦副ＩＤ
               05  MF100-RJNHKNID        PIC  9(10).
      *戏客瘦副≥瘦副戎规
               05  MF100-RJNHKNNUM       PIC  X(03).
      *戏客瘦副≥恕侍戎规
               05  MF100-RJNHBTNUM       PIC  X(02).
      *戏客瘦副≥辉漠录戎规
               05  MF100-RJNFTNJANUM     PIC  X(08).
      *戏客瘦副≥减惦荚戎规
               05  MF100-RJNJKYSNUM      PIC  X(07).
      *给锐攫鼠
           03  MF100-KOHINF.
      *给锐≥瘦副ＩＤ
               05  MF100-KOHID           PIC  9(10).
      *给锐≥瘦副戎规
               05  MF100-KOHNUM          PIC  X(03).
      *给锐≥恕侍戎规
               05  MF100-KOHHBTNUM       PIC  X(02).
      *给锐≥砷么荚戎规
               05  MF100-KOHFTNJANUM     PIC  X(20).
      *给锐≥减惦荚戎规
               05  MF100-KOHJKYSNUM      PIC  X(20).
      *给锐≥扦罢惦烧惰尸
               05  MF100-KOHNINIKYUKBN   PIC  X(01).
      *给锐≥凤眶
               05  MF100-KOHKENSU        PIC  9(03).
      *给锐≥レセプト懒滇惰尸
               05  MF100-RECESKYKBN      PIC  X(01).
      *悸泣眶攫鼠
           03  MF100-JNISSUINF.
               05  MF100-JNISSU          PIC  9(02).
      *圭纷爬眶攫鼠
           03  MF100-TOTALTENINF.
               05  MF100-TOTALTEN        PIC  9(08).
      *省汇板闻嘲爬眶
           03  MF100-FUKUSHITENINF.
               05  MF100-FUKUSHIGAITEN   PIC  9(08).
      *省汇板闻嘲砷么
           03  MF100-FUKUSHIFTNINF.
               05  MF100-FUKUSHIGAIFTN   PIC  9(08).
      *挑恨办婶砷么垛攫鼠
           03  MF100-YKZFTNINF.
               05  MF100-YKZFTN          PIC  9(05).
      *咯祸闻蛙锐攫鼠
           03  MF100-SHOKUJIINF.
      *咯祸泣眶
               05  MF100-SHOKUJINISSU    PIC  9(02).
      *咯祸闻蛙锐
               05  FM100-SHOKUJIRYOYOHI  PIC  9(09).
      *筛洁砷么驰
               05  MF100-SHOKUJIFTN      PIC  9(09).
      *给锐尸挑恨办婶砷么垛
           03  MF100-KOHYKZFTN           PIC  9(05).
      *戏客办婶砷么垛
           03  MF100-RJNFTN              PIC  9(07).
      *哼吗另圭ˇ哼吗琐袋办婶砷么垛
           03  MF100-ZAITAKU             PIC  9(09).
      *另爬眶２
           03  MF100-TOTALTEN2           PIC  9(09).
      *省汇板闻锐懒滇垛驰
           03  MF100-SEIKYUGK            PIC  9(09).
      *
           03  MF100-SEIKYU-FLG          PIC  9(01).
      *瘦副爬眶
           03  MF100-H-TOTALTEN          PIC  9(08).
