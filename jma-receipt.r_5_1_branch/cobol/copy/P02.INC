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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/04/01  いろいろ追加
      * 01.00.02     MCC-多々納   02/06/19  レイアウト変更
      *****************************************************************
         02  P02.
     *******02  P02-F6.
              03  P02-PTNUM.
                04  P02-VALUE   PIC X(20).
              03  P02-NAMEH-G.
                04  P02-NAMEH-STYLE   PIC X(20).
                04  P02-NAMEH         PIC X(50).
              03  P02-SEXH.
                04  P02-VALUE   PIC X(2).
              03  P02-BIRTHDAYH.
                04  P02-VALUE   PIC X(9).
              03  P02-P02-CONDITIONH1.
                04  P02-VALUE   PIC X(6).
              03  P02-P02-CONDITIONH2.
                04  P02-VALUE   PIC X(6).
              03  P02-P02-CONDITIONH3.
                04  P02-VALUE   PIC X(6).
              03  P02-KYUSEIH.
                04  P02-VALUE   PIC X(50).
              03  P02-LASTYMD.
                04  P02-VALUE   PIC X(16).
              03  P02-B04.
                04  P02-B04-STATE   PIC S9(9)   BINARY.
              03  P02-B10S.
                04  P02-B10S-STATE   PIC S9(9)   BINARY.
              03  P02-B12.
                04  P02-B12-STATE   PIC S9(9)   BINARY.
              03  P02-B05S.
                04  P02-B05S-STATE   PIC S9(9)   BINARY.
              03  P02-B12S.
                04  P02-B12S-STATE   PIC S9(9)   BINARY.
              03  P02-B03S.
                04  P02-B03S-STATE   PIC S9(9)   BINARY.
                04  P02-B03S-LABEL   PIC X(20).
              03  P02-B02S.
                04  P02-B02S-STATE   PIC S9(9)   BINARY.
                04  P02-B02S-LABEL   PIC X(20).
              03  P02-NB2.
                04  P02-PAGENO  PIC S9(9)   BINARY.
                04  P02-FIXPAGE1.
                  05  P02-SC61.
                    06  P02-P020-UKETUKE-LIST.
                      07  P02-P020-UKETUKE-LIST-COUNT
                                                 PIC S9(9)   BINARY.
                      07  P02-P020-UKETUKE-LIST-ROW
                                                 PIC S9(9)   BINARY.
                      07  P02-P020-UKETUKE-LIST-ROWATTR
                                                 PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  10 TIMES.
                        08  P02-P020-ULNUM      PIC X(5).
                        08  P02-P020-ULYYKTIME  PIC X(9).
                        08  P02-P020-ULUKETIME  PIC X(9).
                        08  P02-P020-ULSRYKA    PIC X(10).
                        08  P02-P020-ULDRNAME   PIC X(24).
                        08  P02-P020-ULSRYNAIYO PIC X(20).
                        08  P02-P020-ULKARUTE   PIC X(2).
                        08  P02-P020-ULSYOHO    PIC X(2).
                        08  P02-P020-ULHKNCOMBINUM PIC X(60).
                      07  P02-P020-UKETUKE-LIST-SELECT  PIC X
                                               OCCURS  10.
                  05  P02-P020-SELNUM.
                    06  P02-VALUE   PIC X(4).
                  05  P02-P020-SRYKA-COMBO.
                    06  P02-P020-SRYKA.
                      07  P02-VALUE   PIC X(21).
                    06  P02-P020-SRYKA-COUNT   PIC S9(9)   BINARY.
                    06  P02-P020-SRYKA-ITEM    PIC X(21)   OCCURS  99.
                  05  P02-P020-DRNAME-COMBO.
                    06  P02-P020-DRNAME.
                      07  P02-VALUE   PIC X(29).
                    06  P02-P020-DRNAME-COUNT   PIC S9(9)   BINARY.
                    06  P02-P020-DRNAME-ITEM    PIC X(29)   OCCURS  99.
                  05  P02-P020-SRYNAIYO-COMBO.
                    06  P02-P020-SRYNAIYO.
                      07  P02-VALUE   PIC X(21).
                    06  P02-P020-SRYNAIYO-COUNT   PIC S9(9)   BINARY.
                    06  P02-P020-SRYNAIYO-ITEM    PIC X(21)   OCCURS 99.
                  05  P02-P020-KARUTE-COB.
                    06  P02-P020-KARUTE.
                      07  P02-VALUE   PIC X(30).
                    06  P02-P020-KARUTE-COUNT   PIC S9(9)   BINARY.
                    06  P02-P020-KARUTE-ITEM    PIC X(30)   OCCURS  5.
                  05  P02-P020-SYOHOU-COB.
                    06  P02-P020-SYOHOU.
                      07  P02-VALUE   PIC X(16).
                    06  P02-P020-SYOHOU-COUNT   PIC S9(9)   BINARY.
                    06  P02-P020-SYOHOU-ITEM    PIC X(16)   OCCURS  5.
                  05  P02-P020-HKNKUMI.
                    06  P02-VALUE   PIC X(4).
                  05  P02-SCROLLEDWINDOW9.
                    06  P02-P020-HKNKUMI-LIST.
                      07  P02-P020-HKNCOMBI-COUNT   PIC S9(9)   BINARY.
                      07  P02-P020-HKNCOMBI-ROW     PIC S9(9)   BINARY.
                      07  P02-P020-HKNCOMBI-ROWATTR PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  30 TIMES.
                        08  P02-P020-HKNCOBNUM    PIC X(4).
                        08  P02-P020-HKNCOBMEI    PIC X(60).
                        08  P02-P020-TEKSTYMD     PIC X(10).
                        08  P02-P020-TEKEDYMD     PIC X(10).
                      07  P02-P020-HKNCOMBI-SELECT  PIC X  OCCURS  30.
                04  P02-FIXPAGE2.
                  05  P02-P02-KANANAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P02-SEX-COB.
                    06  P02-P02-SEX.
                      07  P02-VALUE   PIC X(4).
                    06  P02-P02-SEX-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-SEX-ITEM    PIC X(4)    OCCURS  2 TIMES.
                  05  P02-P02-DEATHKBN.
                    06  P02-P02-DEATHKBN-VALUE   PIC X.
                    06  P02-P02-DEATHKBN-LABEL   PIC X(10).
                  05  P02-P02-NICKNAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P02-NAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P02-BIRTHDAY.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-NENREI.
                    06  P02-VALUE   PIC X(8).
                  05  P02-P02-HAKKOCOB.
                    06  P02-P02-HAKKO.
                      07  P02-VALUE   PIC X(30).
                    06  P02-P02-HAKKO-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-HAKKO-ITEM    PIC X(30)   OCCURS  5.
                  05  P02-TSTPTNUMKBN-COB.
                    06  P02-TSTPTNUMKBN.
                      07  P02-VALUE   PIC X(16).
                    06  P02-TSTPTNUMKBN-COUNT   PIC S9(9)   BINARY.
                    06  P02-TSTPTNUMKBN-ITEM    PIC X(16)   OCCURS  2.
                  05  P02-P02-HKNJANUM.
                    06  P02-VALUE   PIC X(8).
                  05  P02-P02-HKNNUMCOB.
                    06  P02-P02-HKNNUM.
                      07  P02-VALUE   PIC X(20).
                    06  P02-HKNNUM-COUNT   PIC S9(9)   BINARY.
                    06  P02-HKNNUM-ITEM    PIC X(20)   OCCURS  50.
                  05  P02-P02-HONKZKKBNCOB.
                    06  P02-P02-HONKZKBN.
                      07  P02-VALUE   PIC X(6).
                    06  P02-HONKZKBN-COUNT   PIC S9(9)   BINARY.
                    06  P02-HONKZKBN-ITEM    PIC X(6)    OCCURS  3.
                  05  P02-P02-HOJOKBNCOB.
                    06  P02-P02-HOJOKBN.
                      07  P02-VALUE   PIC X(16).
                    06  P02-HOJOKBN-COUNT   PIC S9(9)   BINARY.
                    06  P02-HOJOKBN-ITEM    PIC X(16)    OCCURS  15.
                  05  P02-P02-CONTKBNCOB.
                    06  P02-P02-CONTKBN.
                      07  P02-VALUE   PIC X(6).
                    06  P02-CONTKBN-COUNT   PIC S9(9)   BINARY.
                    06  P02-CONTKBN-ITEM    PIC X(6)    OCCURS  3.
                  05  P02-P02-KIGO.
                    06  P02-VALUE   PIC X(40).
                  05  P02-P02-NUM.
                    06  P02-VALUE   PIC X(40).
                  05  P02-P02-EDABAN.
                    06  P02-TEXTDATA    PIC X(2).
                  05  P02-P02-SKKGETYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-HTEKSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-HTEKEDYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-HIHKNJANAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P02-HKAKUNINYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-BH1.
                    06  P02-BH1-VALUE   PIC X.
                    06  P02-BH1-LABEL   PIC X(2).
                  05  P02-P02-FTNJANUM1.
                    06  P02-VALUE   PIC X(8).
                  05  P02-P02-KOHNUMCOB1.
                    06  P02-P02-KOHNUM1.
                      07  P02-VALUE   PIC X(20).
                    06  P02-KOHNUM1-COUNT   PIC S9(9)   BINARY.
                    06  P02-KOHNUM1-ITEM    PIC X(20)   OCCURS  200.
                  05  P02-P02-JKYSNUM1.
                    06  P02-VALUE   PIC X(20).
                  05  P02-P02-KTEKSTYMD1.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-KTEKEDYMD1.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-KKAKUNINYMD1.
                    06  P02-VALUE   PIC X(10).
                  05  P02-BK1.
                    06  P02-BK1-VALUE   PIC X.
                    06  P02-BK1-LABEL   PIC X(2).
                  05  P02-CK1.
                    06  P02-CK1-VALUE   PIC X.
                    06  P02-CK1-LABEL   PIC X(4).
                  05  P02-P02-FTNJANUM2.
                    06  P02-VALUE   PIC X(8).
                  05  P02-P02-KOHNUMCOB2.
                    06  P02-P02-KOHNUM2.
                      07  P02-VALUE   PIC X(20).
                    06  P02-KOHNUM2-COUNT   PIC S9(9)   BINARY.
                    06  P02-KOHNUM2-ITEM    PIC X(20)   OCCURS  200.
                  05  P02-P02-JKYSNUM2.
                    06  P02-VALUE   PIC X(20).
                  05  P02-P02-KTEKSTYMD2.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-KTEKEDYMD2.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-KKAKUNINYMD2.
                    06  P02-VALUE   PIC X(10).
                  05  P02-BK2.
                    06  P02-BK2-VALUE   PIC X.
                    06  P02-BK2-LABEL   PIC X(2).
                  05  P02-CK2.
                    06  P02-CK2-VALUE   PIC X.
                    06  P02-CK2-LABEL   PIC X(4).
                  05  P02-P02-FTNJANUM3.
                    06  P02-VALUE   PIC X(8).
                  05  P02-P02-KOHNUMCOB3.
                    06  P02-P02-KOHNUM3.
                      07  P02-VALUE   PIC X(20).
                    06  P02-KOHNUM3-COUNT   PIC S9(9)   BINARY.
                    06  P02-KOHNUM3-ITEM    PIC X(20)   OCCURS  200.
                  05  P02-P02-JKYSNUM3.
                    06  P02-VALUE   PIC X(20).
                  05  P02-P02-KTEKSTYMD3.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-KTEKEDYMD3.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P02-KKAKUNINYMD3.
                    06  P02-VALUE   PIC X(10).
                  05  P02-BK3.
                    06  P02-BK3-VALUE   PIC X.
                    06  P02-BK3-LABEL   PIC X(2).
                  05  P02-CK3.
                    06  P02-CK3-VALUE   PIC X.
                    06  P02-CK3-LABEL   PIC X(4).
                  05  P02-P02-SELNUM.
                    06  P02-VALUE   PIC S9(4).
                  05  P02-P02-SRYKA-COB.
                    06  P02-P02-SRYKA.
                      07  P02-VALUE   PIC X(21).
                    06  P02-SRYKA-COUNT   PIC S9(9)   BINARY.
                    06  P02-SRYKA-ITEM    PIC X(21)   OCCURS  99.
                  05  P02-P02-POST.
                    06  P02-VALUE   PIC X(7).
                  05  P02-P02-ADRS.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-BANTI.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-TEL1.
                    06  P02-VALUE   PIC X(15).
                  05  P02-P02-TEL2.
                    06  P02-VALUE   PIC X(15).
                  05  P02-P02-DISCOUNTKBN-COB.
                    06  P02-P02-DISCOUNTKBN.
                      07  P02-VALUE   PIC X(23).
                    06  P02-P02-DISCOUNTKBN-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-DISCOUNTKBN-ITEM    PIC X(23)
                                                          OCCURS 100.
                  05  P02-P02-DISCOUNT-COB.
                    06  P02-P02-DISCOUNT.
                      07  P02-VALUE   PIC X(14).
                    06  P02-P02-DISCOUNT-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-DISCOUNT-ITEM    PIC X(14)   OCCURS 100.
                  05  P02-P02-SETAINUSI.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P02-ZOKUGARA.
                    06  P02-VALUE   PIC X(30).
                  05  P02-P02-CONDITION1-COB.
                    06  P02-P02-CONDITION1.
                      07  P02-VALUE   PIC X(23).
                    06  P02-P02-CONDITION1-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-CONDITION1-ITEM    PIC X(23)
                                                   OCCURS 100.
                  05  P02-P02-CONDITION2-COB.
                    06  P02-P02-CONDITION2.
                      07  P02-VALUE   PIC X(23).
                    06  P02-P02-CONDITION2-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-CONDITION2-ITEM    PIC X(23)
                                                   OCCURS 100.
                  05  P02-P02-CONDITION3-COB.
                    06  P02-P02-CONDITION3.
                      07  P02-VALUE   PIC X(23).
                    06  P02-P02-CONDITION3-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-CONDITION3-ITEM    PIC X(23)
                                                   OCCURS 100.
                  05  P02-P02-TABOO1.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-TABOO2.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-ALLERGY1.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-ALLERGY2.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-KANSENSYO1.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-KANSENSYO2.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-COMMENT1.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P02-COMMENT2.
                    06  P02-VALUE   PIC X(100).
                  05  P02-SCROLLEDWINDOW6.
                    06  P02-HKNLIST.
                      07  P02-HKNLIST-COUNT   PIC S9(9)   BINARY.
                      07  P02-HKNLIST-ROW     PIC S9(9)   BINARY.
                      07  P02-HKNLIST-ROWATTR PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  60 TIMES.
                        08  P02-HKNCOBNUM     PIC X(4).
                        08  P02-HKNCOBNAME    PIC X(60).
                      07  P02-HKNLIST-SELECT  PIC X   OCCURS  60 TIMES.
                  05  P02-P02-HKNNAME.
                    06  P02-VALUE   PIC X(30).
                  05  P02-P02-HKNNAME2.
                    06  P02-VALUE   PIC X(30).
                  05  P02-P02-HKNJAADRS.
                    06  P02-VALUE   PIC X(40).
                  05  P02-P02-HKNJAADRS2.
                    06  P02-VALUE   PIC X(40).
                  05  P02-P02-HKNJATEL.
                    06  P02-VALUE   PIC X(16).
                  05  P02-P02-HKNJA-IDOMSG.
                    06  P02-HKNJA-IDOMSG-STYLE   PIC X(20).
                    06  P02-HKNJA-IDOMSG-VALUE   PIC X(36).
                  05  P02-P02-NYUKIN-COB.
                    06  P02-P02-NYUKIN.
                      07  P02-VALUE   PIC X(23).
                    06  P02-P02-NYUKIN-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-NYUKIN-ITEM
                            PIC X(23)   OCCURS  100 TIMES.
                  05  P02-P02-USERPG-COB.
                    06  P02-P02-USERPG-STATE   PIC S9(9)   BINARY.
                    06  P02-P02-USERPG.
                      07  P02-VALUE   PIC X(20).
                    06  P02-P02-USERPG-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-USERPG-ITEM
                            PIC X(20)   OCCURS  2 TIMES.
                  05  P02-P02-SKYPRTFLG-COB.
                    06  P02-P02-SKYPRTFLG.
                      07  P02-VALUE   PIC X(50).
                    06  P02-P02-SKYPRTFLG-COUNT   PIC S9(9)   BINARY.
                    06  P02-P02-SKYPRTFLG-ITEM
                            PIC X(50)   OCCURS  15 TIMES.
                  05  P02-P02-BIRTHWEIGHT.
                    06  P02-VALUE   PIC S9(4).
                  05  P02-TEIMSG.
                    06  P02-TEIMSG-STYLE   PIC X(20).
                    06  P02-TEIMSG-VALUE   PIC X(20).
                  05  P02-TEIMSG2.
                    06  P02-TEIMSG2-STYLE   PIC X(20).
                    06  P02-TEIMSG2-VALUE   PIC X(10).
                  05  P02-TEIMSG3.
                    06  P02-TEIMSG3-STYLE   PIC X(20).
                    06  P02-TEIMSG3-VALUE   PIC X(10).
                  05  P02-BHR.
                    06  P02-BHR-STATE   PIC S9(9)   BINARY.
                    06  P02-BHR-LABEL   PIC X(4).
                04 P02-FP3.
                  05  P02-P027-SELNUM.
                    06  P02-VALUE   PIC X(4).
                  05  P02-P027-TITLE1.
                    06  P02-VALUE   PIC X(8).
                  05  P02-P027-TITLE2.
                    06  P02-VALUE   PIC X(8).
                  05  P02-P027-TITLE3.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-TITLE4.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-TITLE5.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-TITLE6.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-TITLE7.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-TITLE8.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-TITLE9.
                    06  P02-VALUE   PIC X(15).
                  05  P02-P027-FTNKBN PIC X(8) OCCURS 3 TIMES.
                  05  P02-P027-KAIWARI PIC X(8) OCCURS 3 TIMES.
                  05  P02-P027-KAIKOTGAK PIC X(10) OCCURS 3 TIMES.
                  05  P02-P027-KAIJOGGAK PIC X(10) OCCURS 3 TIMES.
                  05  P02-P027-HIJOGGAK  PIC X(10) OCCURS 3 TIMES.
                  05  P02-P027-HIJOGCNT  PIC X(10) OCCURS 3 TIMES.
                  05  P02-P027-TUKJOGGAK  PIC X(10) OCCURS 3 TIMES.
                  05  P02-P027-TUKJOGCNT  PIC X(10) OCCURS 3 TIMES.
                  05  P02-P027-YAKKBN  PIC X(14) OCCURS 3 TIMES.
                  05  P02-P027-TNSTANKA.
                    06  P02-VALUE   PIC X(4).
                  05  P02-P027-TAXKBN.
                    06  P02-VALUE   PIC X(6).
                  05  P02-P027-HKNNUM.
                    06  P02-VALUE   PIC X(3).
                  05  P02-P027-HKNSEIDON.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-KOHNUM1.
                    06  P02-VALUE   PIC X(3).
                  05  P02-P027-KOHSEIDO1N.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-KOHNUM2.
                    06  P02-VALUE   PIC X(3).
                  05  P02-P027-KOHSEIDO2N.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-KOHNUM3.
                    06  P02-VALUE   PIC X(3).
                  05  P02-P027-KOHSEIDO3N.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P027-KOHNUM4.
                    06  P02-VALUE   PIC X(3).
                  05  P02-P027-KOHSEIDO4N.
                    06  P02-VALUE   PIC X(10).
                  05  P02-BHO1.
                    06  P02-BHO1-VALUE   PIC X.
                    06  P02-BHO1-LABEL   PIC X(2).
                  05  P02-P027-KOHLIST.
                    06  P02-P027-KOHLIST-COUNT   PIC S9(9)   BINARY.
                    06  P02-P027-KOHLIST-ROW     PIC S9(9)   BINARY.
                    06  P02-P027-KOHLIST-ROWATTR PIC S9(9)   BINARY.
                    06  P02-ITEM        OCCURS  5 TIMES.
                      07  P02-TKOHNUM   PIC X(10).
                      07  P02-TFTNJANUM PIC X(10).
                      07  P02-TJKYSNUM  PIC X(20).
                      07  P02-TKTEKSTYMD  PIC X(9).
                      07  P02-TKTEKEDYMD  PIC X(9).
                      07  P02-TKKAKUNINYMD  PIC X(9).
                    06  P02-P027-KOHLIST-SELECT  PIC X       OCCURS  5.
                  05  P02-BKH1.
                    06  P02-BKH1-VALUE   PIC X.
                    06  P02-BKH1-LABEL   PIC X(2).
                  05  P02-BKH2.
                    06  P02-BKH2-VALUE   PIC X.
                    06  P02-BKH2-LABEL   PIC X(2).
                  05  P02-BKH3.
                    06  P02-BKH3-VALUE   PIC X.
                    06  P02-BKH3-LABEL   PIC X(2).
                  05  P02-BKH4.
                    06  P02-BKH4-VALUE   PIC X.
                    06  P02-BKH4-LABEL   PIC X(2).
                  05  P02-BKH5.
                    06  P02-BKH5-VALUE   PIC X.
                    06  P02-BKH5-LABEL   PIC X(2).
                  05  P02-BHO2.
                    06  P02-BHO2-VALUE   PIC X.
                    06  P02-BHO2-LABEL   PIC X(2).
                  05  P02-BHO3.
                    06  P02-BHO3-VALUE   PIC X.
                    06  P02-BHO3-LABEL   PIC X(2).
                  05  P02-BHO4.
                    06  P02-BHO4-VALUE   PIC X.
                    06  P02-BHO4-LABEL   PIC X(2).
                  05  P02-BHO5.
                    06  P02-BHO5-VALUE   PIC X.
                    06  P02-BHO5-LABEL   PIC X(2).
                  05  P02-BHR1.
                    06  P02-BHR1-STATE   PIC S9(9)   BINARY.
                    06  P02-BHR1-LABEL   PIC X(2).
                  05  P02-BHR2.
                    06  P02-BHR2-STATE   PIC S9(9)   BINARY.
                    06  P02-BHR2-LABEL   PIC X(2).
                  05  P02-BHR3.
                    06  P02-BHR3-STATE   PIC S9(9)   BINARY.
                    06  P02-BHR3-LABEL   PIC X(2).
                  05  P02-BHR4.
                    06  P02-BHR4-STATE   PIC S9(9)   BINARY.
                    06  P02-BHR4-LABEL   PIC X(2).
                  05  P02-BHR5.
                    06  P02-BHR5-STATE   PIC S9(9)   BINARY.
                    06  P02-BHR5-LABEL   PIC X(2).
                  05  P02-P027-HKNLIST.
                      07  P02-P027-HKNLIST-COUNT   PIC S9(9)   BINARY.
                      07  P02-P027-HKNLIST-ROW     PIC S9(9)   BINARY.
                      07  P02-P027-HKNLIST-ROWATTR PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  5 TIMES.
                        08  P02-THKNNUM   PIC X(10).
                        08  P02-THKNJANUM  PIC X(8).
                        08  P02-THONKAZKBN  PIC X(4).
                        08  P02-THOJOKBN  PIC X(4).
                        08  P02-TCONTKBN  PIC X(4).
                        08  P02-TKIGO     PIC X(30).
                        08  P02-TNUM     PIC X(30).
                        08  P02-TEDABAN  PIC X(02).
                        08  P02-THTEKSTYMD PIC X(09).
                        08  P02-THTEKEDYMD PIC X(09).
                        08  P02-THKAKUNINYMD PIC X(09).
                        08  P02-TSKKGETYMD PIC X(09).
                      07  P02-P027-HKNLIST-SELECT  PIC X    OCCURS  5.
                  05  P02-SCROLLEDWINDOW10.
                    06  P02-P027-HKNKUMI-LIST.
                      07  P02-P027-HKNKUMI-COUNT PIC S9(9) BINARY.
                      07  P02-P027-HKNKUMI-ROW   PIC S9(9) BINARY.
                      07  P02-P027-HKNKUMI-ROWATTR   PIC S9(9) BINARY.
                      07  P02-P027-HKNKUMI-ITEM OCCURS  50 TIMES.
                        08  P02-P027-NUM       PIC X(6).
                        08  P02-P027-HKNSEIDO  PIC X(10).
                        08  P02-P027-KOHSEIDO1 PIC X(10).
                        08  P02-P027-KOHSEIDO2 PIC X(10).
                        08  P02-P027-KOHSEIDO3 PIC X(10).
                        08  P02-P027-KOHSEIDO4 PIC X(10).
                        08  P02-P027-TEKSTYMD  PIC X(10).
                        08  P02-P027-TEKEDYMD  PIC X(10).
                        08  P02-P027-TJKYSNUM  PIC X(20).
                        08  P02-P027-THYOJIKBN PIC X(10).
                      07  P02-P027-HKNKUMI-SELECT PIC X
                                                 OCCURS 50 TIMES.
                  05  P02-P027-NYUGAIKBN.
                    06  P02-P027-NYUGAIKBN-VALUE   PIC X.
                    06  P02-P027-NYUGAIKBN-LABEL   PIC X(20).
                  05  P02-P027-HYOJIKBN-COB.
                    06  P02-P027-HYOJIKBN.
                      07  P02-VALUE   PIC X(20).
                    06  P02-P027-HYOJIKBN-COUNT   PIC S9(9)   BINARY.
                    06  P02-P027-HYOJIKBN-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                  05  P02-P027-COMBKBN.
                    06  P02-P027-COMBKBN-VALUE   PIC X.
                04  P02-FIXPAGE4.
                  05  P02-P021-FAX.
                    06  P02-VALUE   PIC X(15).
                  05  P02-P021-KEITAI.
                    06  P02-VALUE   PIC X(15).
                  05  P02-P021-EMAIL.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P021-JOB.
                    06  P02-VALUE   PIC X(20).
                  05  P02-P021-RENNAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P021-RENZOKUGARA.
                    06  P02-VALUE   PIC X(30).
                  05  P02-P021-RENPOST.
                    06  P02-VALUE   PIC X(7).
                  05  P02-P021-RENADRS.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P021-RENBANTI.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P021-RENTEL1.
                    06  P02-VALUE   PIC X(15).
                  05  P02-P021-RENTEL2.
                    06  P02-VALUE   PIC X(15).
                  05  P02-P021-OFFNAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P021-OFFPOST.
                    06  P02-VALUE   PIC X(7).
                  05  P02-P021-OFFADRS.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P021-OFFBANTI.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P021-OFFTEL.
                    06  P02-VALUE   PIC X(15).
                  05  P02-P021-KISNAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P021-KISPOST.
                    06  P02-VALUE   PIC X(7).
                  05  P02-P021-KISADRS.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P021-KISBANTI.
                    06  P02-VALUE   PIC X(100).
                  05  P02-P021-KISTEL.
                    06  P02-VALUE   PIC X(15).
                04  P02-FIXPAGE7.
                  05  P02-P024-TSRSELNUM.
                    06  P02-VALUE   PIC X(4).
                  05  P02-P024-TSYSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P024-TSYEDYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P024-TSYSKJGNSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P024-TNKSELNUM.
                    06  P02-VALUE   PIC X(4).
                  05  P02-P024-TNKSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P024-TNKEDYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-SC104.
                    06  P02-P024-TSYLIST.
                      07  P02-P024-TSYLIST-COUNT PIC S9(9) BINARY.
                      07  P02-P024-TSYLIST-ROW   PIC S9(9) BINARY.
                      07  P02-P024-TSYLIST-ROWATTR  PIC S9(9) BINARY.
                      07  P02-P024-TSYLIST-ITEM OCCURS  20 TIMES.
                        08  P02-P024-TSYLNUM   PIC X(3).
                        08  P02-P024-TSYLSTYMD PIC X(10).
                        08  P02-P024-TSYLEDYMD PIC X(10).
                        08  P02-P024-TSYLHKNKBN PIC X(12).
                        08  P02-P024-TSYLNINYMD PIC X(10).
                        08  P02-P024-TSYLSKJGNSTYMD PIC X(10).
                        08  P02-P024-TSYLKYOUKAI PIC X(20).
                      07  P02-P024-TSYLIST-SELECT PIC X
                                                OCCURS  20 TIMES.
                  05  P02-SC105.
                    06  P02-P024-TNKLIST.
                      07  P02-P024-TNKLIST-COUNT PIC S9(9) BINARY.
                      07  P02-P024-TNKLIST-ROW   PIC S9(9) BINARY.
                      07  P02-P024-TNKLIST-ROWATTR   PIC S9(9) BINARY.
                      07  P02-P024-TNKLIST-ITEM OCCURS  20 TIMES.
                        08  P02-P024-TNKLNUM   PIC X(3).
                        08  P02-P024-TNKLSTYMD PIC X(10).
                        08  P02-P024-TNKLEDYMD PIC X(10).
                        08  P02-P024-TNKLHKNKBN PIC X(12).
                        08  P02-P024-TNKLNINYMD PIC X(10).
                        08  P02-P024-TNKLFUKUSHI PIC X(10).
                        08  P02-P024-TNKLKYOUKAI  PIC X(20).
                      07  P02-P024-TNKLIST-SELECT PIC X
                                               OCCURS  20 TIMES.
                  05  P02-P024-FUKUSHI-COMB.
                    06  P02-P024-FUKUSHI.
                      07  P02-VALUE   PIC X(10).
                    06  P02-P024-FUKUSHI-COUNT   PIC S9(9)   BINARY.
                    06  P02-P024-FUKUSHI-ITEM
                            PIC X(10)   OCCURS  2 TIMES.
                  05  P02-P024-TSYNINSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P024-TSYTEKK-COB.
                    06  P02-P024-TSYTEKK.
                      07  P02-VALUE   PIC X(30).
                    06  P02-P024-TSYTEKK-COUNT   PIC S9(9)   BINARY.
                    06  P02-P024-TSYTEKK-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  P02-P024-TSYKYOUKAI-COB.
                    06  P02-P024-TSYKYOUKAI.
                      07  P02-TEXTDATA    PIC X(30).
                    06  P02-P024-TSYKYOUKAI-COUNT   PIC S9(9)   BINARY.
                    06  P02-P024-TSYKYOUKAI-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  P02-P024-TNKNINSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P024-TNKTEKK-COB.
                    06  P02-P024-TNKTEKK.
                      07  P02-VALUE   PIC X(30).
                    06  P02-P024-TNKTEKK-COUNT   PIC S9(9)   BINARY.
                    06  P02-P024-TNKTEKK-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  P02-P024-TNKKYOUKAI-COB.
                    06  P02-P024-TNKKYOUKAI.
                      07  P02-TEXTDATA    PIC X(30).
                    06  P02-P024-TNKKYOUKAI-COUNT   PIC S9(9)   BINARY.
                    06  P02-P024-TNKKYOUKAI-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  P02-P024-FTNTEKSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P024-FTNTEKEDYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P024-GAIJGNGAK.
                    06  P02-VALUE   PIC X(9).
                  05  P02-P024-NYUJGNGAK-G.
                    06  P02-P024-NYUJGNGAK-STATE   PIC S9(9)   BINARY.
                    06  P02-P024-NYUJGNGAK   PIC X(9).
                  05  P02-P024-PTKOHFTNLST.
                    06  P02-P024-PTKOHFTN.
                      07  P02-P024-HFTNLST-COUNT    PIC S9(9)   BINARY.
                      07  P02-P024-HFTNLST-ROW      PIC S9(9)   BINARY.
                      07  P02-P024-HFTNLST-ROWATTR  PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  60 TIMES.
                        08  P02-P024-TFTNNUM  PIC X(3).
                        08  P02-P024-TFTNKOH  PIC X(20).
                        08  P02-P024-TKOHSTYMD    PIC X(09).
                        08  P02-P024-TKOHEDYMD    PIC X(9).
                      07  P02-P024-HFTNLST-SELECT  PIC X   OCCURS  60.
                  05  P02-P024-PTKOHFTNLST2.
                    06  P02-P024-PTKOHFTN2.
                      07  P02-P024-LABL1.
                        08  P02-VALUE   PIC X(12).
                      07  P02-P024-LABL2.
                        08  P02-VALUE   PIC X(12).
                      07  P02-P024-LABL3.
                        08  P02-VALUE   PIC X(4).
                      07  P02-P024-HFTNLST2-COUNT   PIC S9(9)   BINARY.
                      07  P02-P024-HFTNLST2-ROW      PIC S9(9)   BINARY.
                      07  P02-P024-HFTNLST2-ROWATTR  PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS 300 TIMES.
                        08  P02-P024-TFTNNUM2  PIC X(3).
                        08  P02-P024-TFTNSTYMD  PIC X(9).
                        08  P02-P024-TFTNEDYMD  PIC X(9).
                        08  P02-P024-TGAIJGNGAK PIC X(9).
                        08  P02-P024-TNYUJGNGAK PIC X(9).
                        08  P02-P024-TFTNETC    PIC X(4).
                      07  P02-P024-HFTNLST2-SELECT  PIC X   OCCURS 300.
                  05  P02-P024-FTNSELNUM.
                    06  P02-VALUE   PIC S9(3).
                  05  P02-P024-FTNSELNUM2.
                    06  P02-VALUE   PIC S9(3).
                  05  P02-P024-JKYSNUM.
                    06  P02-VALUE   PIC X(40).
                  05  P02-FTNMSG1.
                    06  P02-VALUE   PIC X(16).
                  05  P02-FTNMSG2.
                    06  P02-VALUE   PIC X(16).
                04  P02-FIXPAGE8.
                  05  P02-P025-SELNUM1.
                    06  P02-VALUE   PIC X(4).
                  05  P02-P025-THENYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P025-TKANANAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P025-TNAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P025-TNICKNAME.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P025-SELNUM2.
                    06  P02-VALUE   PIC X(4).
                  05  P02-SC108.
                    06  P02-P025-KYUSEILIST.
                      07  P02-P025-KYUSEILIST-COUNT PIC S9(9) BINARY.
                      07  P02-P025-KYUSEILIS-ROW     PIC S9(9)   BINARY.
                      07  P02-P025-KYUSEILIS-ROWATTR PIC S9(9)   BINARY.
                      07  P02-P025-KYUSEILIST-ITEM OCCURS 10 TIMES.
                        08  P02-P025-NUM1     PIC X(4).
                        08  P02-P025-HENKOYMD PIC X(10).
                        08  P02-P025-KANANAME PIC X(50).
                        08  P02-P025-NAME     PIC X(50).
                        08  P02-P025-NICKNAME PIC X(50).
                      07  P02-P025-KYUSEILIST-SELECT PIC X
                                              OCCURS  10 TIMES.
                  05  P02-SC107.
                    06  P02-P025-JYUSINLIST1.
                      07  P02-P025-JYUSINLIST1-COUNT PIC S9(9) BINARY.
                      07  P02-P025-JYUSINLIST1-ROW   PIC S9(9) BINARY.
                      07  P02-P025-JYUSINLIST1-ROWATTR PIC S9(9) BINARY.
                      07  P02-P025-JYUSINLIST1-ITEM OCCURS  50 TIMES.
                        08  P02-P025-NUM2      PIC X(5).
                        08  P02-P025-SRYKA1    PIC X(10).
                        08  P02-P025-SYOSINYMD PIC X(10).
                        08  P02-P025-LASTYMD   PIC X(10).
                      07  P02-P025-JYUSINLIST1-SELECT PIC X
                                               OCCURS  50 TIMES.
                  05  P02-SC106.
                    06  P02-P025-JYUSINLIST2.
                      07  P02-P025-JYUSINLIST2-COUNT PIC S9(9) BINARY.
                      07  P02-P025-JYUSINLIST2-ROW  PIC S9(9) BINARY.
                      07  P02-P025-JYUSINLIST2-ROWATTR PIC S9(9) BINARY.
                      07  P02-P025-JYUSINLIST2-ITEM OCCURS  50 TIMES.
                        08  P02-P025-SRYYMD   PIC X(10).
                        08  P02-P025-SRYKA2   PIC X(10).
                        08  P02-P025-SRYNAIYO PIC X(66).
                      07  P02-P025-JYUSINLIST2-SELECT PIC X
                                               OCCURS  50 TIMES.
                04  P02-FP9.
                  05  P02-P026-SELNUM.
                    06  P02-VALUE   PIC S9(3).
                  05  P02-P026-CD.
                    06  P02-VALUE   PIC X(2).
                  05  P02-P026-NAIYO.
                    06  P02-VALUE   PIC X(12).
                  05  P02-P026-STYM.
                    06  P02-VALUE   PIC X(7).
                  05  P02-P026-EDYM.
                    06  P02-VALUE   PIC X(7).
                  05  P02-SCROLLEDWINDOW11.
                    06  P02-HPOS    PIC S9(9)   BINARY.
                    06  P02-VPOS    PIC S9(9)   BINARY.
                    06  P02-P026-TOKLST.
                      07  P02-P026-TOK-COUNT   PIC S9(9)   BINARY.
                      07  P02-P026-TOK-ROW PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  50 TIMES.
                        08  P02-P026-TNUM     PIC X(3).
                        08  P02-P026-TNYUGAI PIC X(20).
                        08  P02-P026-TCD     PIC X(2).
                        08  P02-P026-TNAIYO  PIC X(12).
                        08  P02-P026-TSTYM   PIC X(10).
                        08  P02-P026-TEDYM   PIC X(10).
                      07  P02-P026-TOK-SELECT
                              PIC X       OCCURS  50 TIMES.
                  05  P02-P026-NYUGAI-COMB.
                    06  P02-P026-NYUGAI.
                      07  P02-VALUE   PIC X(20).
                    06  P02-P026-NYUGAI-COUNT   PIC S9(9)   BINARY.
                    06  P02-P026-NYUGAI-ITEM
                            PIC X(20)   OCCURS  3 TIMES.
                  05  P02-P026-TOKCD-COMB.
                    06  P02-P026-TOKCD.
                      07  P02-VALUE   PIC X(20).
                    06  P02-P026-TOKCD-COUNT   PIC S9(9)   BINARY.
                    06  P02-P026-TOKCD-ITEM
                            PIC X(20)   OCCURS  50 TIMES.
                  05  P02-SCROLLEDWINDOW1.
                    06  P02-HPOS    PIC S9(9)   BINARY.
                    06  P02-VPOS    PIC S9(9)   BINARY.
                    06  P02-P026-HDLST.
                      07  P02-P026-HDLST-COUNT   PIC S9(9)   BINARY.
                      07  P02-P026-HDLST-ROW PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  50 TIMES.
                        08  P02-P026-THDNUM PIC X(3).
                        08  P02-P026-THDSRYYM  PIC X(10).
                        08  P02-P026-THDDAY    PIC X(2).
                        08  P02-P026-TDNYUGAI  PIC X(20).
                        08  P02-P026-TDRESEKBN  PIC X(20).
                      07  P02-P026-HDLST-SELECT
                              PIC X       OCCURS  50 TIMES.
                  05  P02-P026-HDSELNUM.
                    06  P02-VALUE   PIC S9(3).
                  05  P02-P026-HD-NYUGAI-COMB.
                    06  P02-P026-HD-NYUGAI.
                      07  P02-VALUE   PIC X(20).
                    06  P02-P026-HD-NYUGAI-COUNT   PIC S9(9)   BINARY.
                    06  P02-P026-HD-NYUGAI-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                  05  P02-P026-HDSRYYM.
                    06  P02-VALUE   PIC X(8).
                  05  P02-P026-HDDAY.
                    06  P02-VALUE   PIC X(2).
                  05  P02-P026-HD-RESEKBN-COMB.
                    06  P02-P026-HD-RESEKBN.
                      07  P02-VALUE   PIC X(20).
                    06  P02-P026-HD-RESEKBN-COUNT   PIC S9(9)   BINARY.
                    06  P02-P026-HD-RESEKBN-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                  05  P02-SCROLLEDWINDOW2.
                    06  P02-HPOS    PIC S9(9)   BINARY.
                    06  P02-VPOS    PIC S9(9)   BINARY.
                    06  P02-P026-HDHKNLST1.
                      07  P02-P026-HDHKNLST1-COUNT  PIC S9(9)   BINARY.
                      07  P02-P026-HDHKNLST1-ROW PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  20 TIMES.
                        08  P02-P026-THKNNUM1  PIC X(4).
                        08  P02-P026-TDELKBN1  PIC X(2).
                        08  P02-P026-THKNMEI1  PIC X(60).
                      07  P02-P026-HDHKNLST1-SELECT
                              PIC X       OCCURS  20 TIMES.
                  05  P02-SCROLLEDWINDOW3.
                    06  P02-HPOS    PIC S9(9)   BINARY.
                    06  P02-VPOS    PIC S9(9)   BINARY.
                    06  P02-P026-HDHKNLST2.
                      07  P02-P026-HDHKNLST2-COUNT PIC S9(9)   BINARY.
                      07  P02-P026-HDHKNLST2-ROW PIC S9(9)   BINARY.
                      07  P02-ITEM        OCCURS  20 TIMES.
                        08  P02-P026-THKNNUM2  PIC X(4).
                        08  P02-P026-TDELKBN2  PIC X(2).
                        08  P02-P026-THKNMEI2  PIC X(60).
                      07  P02-P026-HDHKNLST2-SELECT
                              PIC X       OCCURS  20 TIMES.
                  05  P02-P026-HDMIDAS1.
                    06  P02-VALUE   PIC X(20).
                  05  P02-P026-HDMIDAS2.
                    06  P02-VALUE   PIC X(20).
                  05  P02-P026-DELSEL1.
                    06  P02-VALUE   PIC S9(4).
                  05  P02-P026-DELSEL2.
                    06  P02-VALUE   PIC S9(4).
                04  P02-FP10.
                  05  P02-P028-MNOTE-QRKBN-COMB.
                    06  P02-P028-MNOTE-QRKBN.
                      07  P02-VALUE   PIC X(30).
                    06  P02-P028-MNOTE-QRKBN-COUNT   PIC S9(9) BINARY.
                    06  P02-P028-MNOTE-QRKBN-ITEM
                            PIC X(30)   OCCURS  9 TIMES.
                  05  P02-P028-MNOTE-CSV-COMB.
                    06  P02-P028-MNOTE-CSV.
                      07  P02-VALUE   PIC X(50).
                    06  P02-P028-MNOTE-CSV-COUNT   PIC S9(9)   BINARY.
                    06  P02-P028-MNOTE-CSV-ITEM
                            PIC X(50)   OCCURS  20 TIMES.
                  05  P02-P028-PATIENTID1.
                    06  P02-VALUE   PIC X(20).
                  05  P02-P028-AGREEMENT-COMB.
                    06  P02-P028-AGREEMENT.
                      07  P02-VALUE   PIC X(20).
                    06  P02-P028-AGREEMENT-COUNT   PIC S9(9)   BINARY.
                    06  P02-P028-AGREEMENT-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                  05  P02-P028-JMAMRVER-COMB.
                    06  P02-P028-JMAMRVER.
                      07  P02-VALUE   PIC X(50).
                    06  P02-P028-JMAMRVER-COUNT   PIC S9(9)   BINARY.
                    06  P02-P028-JMAMRVER-ITEM
                            PIC X(50)   OCCURS  20 TIMES.
                  05  P02-P028-CRHKNJANUM.
                    06  P02-VALUE   PIC X(6).
                  05  P02-P028-CRHIHKNJANUM.
                    06  P02-VALUE   PIC X(10).
                  05  P02-SCROLLEDWINDOW4.
                    06  P02-HPOS    PIC S9(9)   BINARY.
                    06  P02-VPOS    PIC S9(9)   BINARY.
                    06  P02-P028-CARENINLST.
                      07  P02-P028-CARELST-COUNT   PIC S9(9)   BINARY.
                      07  P02-P028-CARELST-ROW PIC S9(9)   BINARY.
                      07  P02-P028-CARLST-ITEM        OCCURS  50 TIMES.
                        08  P02-P028-TCRNUM  PIC X(3).
                        08  P02-P028-TCAREJOTAI  PIC X(30).
                        08  P02-P028-TCRNINYMD   PIC X(9).
                        08  P02-P028-TCRTEKSTYMD  PIC X(9).
                        08  P02-P028-TCRTEKEDYMD  PIC X(9).
                      07  P02-P028-CARELST-SELECT
                              PIC X       OCCURS  50 TIMES.
                      07  P02-P028-CARELST-FGCOLOR
                              PIC X(16)   OCCURS  50 TIMES.
                      07  P02-P028-CARELST-BGCOLOR
                              PIC X(16)   OCCURS  50 TIMES.
                  05  P02-P028-CRNINYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P028-CRTEKSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P028-CRTEKEDYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P028-CAREJOTAI-COMB.
                    06  P02-P028-CAREJOTAI.
                      07  P02-VALUE   PIC X(50).
                    06  P02-P028-CAREJOTAI-COUNT   PIC S9(9)   BINARY.
                    06  P02-P028-CAREJOTAI-ITEM
                            PIC X(50)   OCCURS  99 TIMES.
                  05  P02-P028-CRSELNUM.
                    06  P02-VALUE   PIC S9(3).
                  05  P02-P028-CAREHKNMEI.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P028-CAREHKNJAMEI.
                    06  P02-VALUE   PIC X(50).
                  05  P02-P028-CAREHKNJAADRS.
                    06  P02-VALUE   PIC X(80).
                  05  P02-P028-CAREHKNJAADRS2.
                    06  P02-VALUE   PIC X(80).
                  05  P02-P028-CAREHKNJATEL.
                    06  P02-VALUE   PIC X(16).
                  05  P02-P028-CRHSELNUM.
                    06  P02-VALUE   PIC S9(2).
                  05  P02-P028-CHTEKSTYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-P028-CHTEKEDYMD.
                    06  P02-VALUE   PIC X(10).
                  05  P02-SCROLLEDWINDOW5.
                    06  P02-HPOS    PIC S9(9)   BINARY.
                    06  P02-VPOS    PIC S9(9)   BINARY.
                    06  P02-P028-CAREHKNLST.
                      07  P02-P028-CRHKNLST-COUNT   PIC S9(9)  BINARY.
                      07  P02-P028-CRHKNLST-ROW PIC S9(9)   BINARY.
                      07  P02-P028-ITEM        OCCURS  10 TIMES.
                        08  P02-P028-TCHNUM    PIC X(2).
                        08  P02-P028-TCRHKNJANUM  PIC X(6).
                        08  P02-P028-TCRHIHKNJANUM  PIC X(10).
                        08  P02-P028-TCRHSTRYMD     PIC X(9).
                        08  P02-P028-TCRHENDYMD     PIC X(9).
                      07  P02-P028-CRHKNLST-SELECT
                              PIC X       OCCURS  10 TIMES.
                      07  P02-P028-CRHKNLST-FGCOLOR
                              PIC X(16)   OCCURS  10 TIMES.
                      07  P02-P028-CRHKNLST-BGCOLOR
                              PIC X(16)   OCCURS  10 TIMES.
                  05  P02-P028-CHKHKTBYO1.
                    06  P02-VALUE   PIC X.
                  05  P02-P028-CHKHKTBYO2.
                    06  P02-VALUE   PIC X.
                  05  P02-P028-CHKHKTBYO3.
                    06  P02-VALUE   PIC X.
                  05  P02-P028-CHKHKTBYO4.
                    06  P02-VALUE   PIC X.
                  05  P02-P028-NNCHIKBN.
                    06  P02-VALUE   PIC X.
                  05  P02-P028-SYOUNIKBN.
                    06  P02-VALUE   PIC X.
                  05  P02-SCROLLEDWINDOW7.
                    06  P02-P028-MYLIST.
                      07  P02-P028-MYLIST-COUNT   PIC S9(9)   BINARY.
                      07  P02-P028-MYLIST-ROW PIC S9(9)   BINARY.
                      07  P02-P028-MYLIST-SELECT
                              PIC X       OCCURS  20 TIMES.
                      07  P02-P028-MYLIST-FGCOLOR
                              PIC X(16)   OCCURS  20 TIMES.
                      07  P02-P028-MYLIST-BGCOLOR
                              PIC X(16)   OCCURS  20 TIMES.
                      07  P02-ITEM        OCCURS  20 TIMES.
                        08  P02-P028-TMYNUM PIC X(2).
                        08  P02-P028-TMYIDKEY PIC X(10).
                        08  P02-P028-TMYNUMBER    PIC X(20).
                        08  P02-P028-TMYDESCRIP PIC X(100).
                  05  P02-BCMY1.
                    06  P02-STATE   PIC S9(9)   BINARY.
                  05  P02-BCMY2.
                    06  P02-STATE   PIC S9(9)   BINARY.
                  05  P02-P028-MYSELNUM-G.
                    06  P02-P028-MYSELNUM PIC 9(2).
                  05  P02-P028-MYIDKEY.
                    06  P02-TEXTDATA    PIC X(10).
                  05  P02-P028-MY-NUMBER.
                    06  P02-TEXTDATA    PIC X(20).
                  05  P02-P028-MY-DESCRIP.
                    06  P02-TEXTDATA    PIC X(100).
                  05  P02-P028-AUTOSRYCD1.
                    06  P02-TEXTDATA    PIC X(9).
                  05  P02-P028-AUTOSRYCD2.
                    06  P02-TEXTDATA    PIC X(9).
                  05  P02-P028-AUTOSRYCD3.
                    06  P02-TEXTDATA    PIC X(9).
                  05  P02-P028-AUTONAME1.
                    06  P02-TEXTDATA    PIC X(40).
                  05  P02-P028-AUTONAME2.
                    06  P02-TEXTDATA    PIC X(40).
                  05  P02-P028-AUTONAME3.
                    06  P02-TEXTDATA    PIC X(40).
                  05  P02-P028-NINPUKBN.
                    06  P02-VALUE   PIC X.

