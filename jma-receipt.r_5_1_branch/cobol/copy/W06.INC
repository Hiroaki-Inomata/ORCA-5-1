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
        02  W06.
  ********  02  W06-FIXED1.
              03  W06-NT1.
                04  W06-PAGENO  PIC S9(9)   BINARY.
                04  W06-TG1.
                  05  W06-ISEKINUM.
                    06  W06-VALUE   PIC X(8).
                  05  W06-HKNDRNUMS.
                    06  W06-VALUE   PIC X(8).
                  05  W06-HKNDRNUMK.
                    06  W06-VALUE   PIC X(12).
                  05  W06-MAYAKULICENSE.
                    06  W06-VALUE   PIC X(40).
                  05  W06-SEMONKACD-COMBG     OCCURS   5.
                    06  W06-SEMONKACD-COMB.
                      07  W06-SEMONKACD.
                        09  W06-VALUE   PIC X(17).
                      07  W06-SEMONKACD-COUNT   PIC S9(9)   BINARY.
                      07  W06-SEMONKACD-ITEM
                              PIC X(17)   OCCURS  99 TIMES.
                  05  W06-CHK33.
                    06  W06-CHK33-VALUE   PIC X.
                    06  W06-CHK33-STATE   PIC S9(9)   BINARY.
                    06  W06-CHK33-LABEL   PIC X(20).
                  05  W06-CHK36.
                    06  W06-CHK36-VALUE   PIC X.
                    06  W06-CHK36-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK11.
                    06  W06-CHK11-VALUE   PIC X.
                    06  W06-CHK11-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK01.
                    06  W06-CHK01-VALUE   PIC X.
                    06  W06-CHK01-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK10.
                    06  W06-CHK10-VALUE   PIC X.
                    06  W06-CHK10-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK14.
                    06  W06-CHK14-VALUE   PIC X.
                    06  W06-CHK14-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK24.
                    06  W06-CHK24-VALUE   PIC X.
                    06  W06-CHK24-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK20.
                    06  W06-CHK20-VALUE   PIC X.
                    06  W06-CHK20-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK18.
                    06  W06-CHK18-VALUE   PIC X.
                    06  W06-CHK18-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK19.
                    06  W06-CHK19-VALUE   PIC X.
                    06  W06-CHK19-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK03.
                    06  W06-CHK03-VALUE   PIC X.
                    06  W06-CHK03-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK04.
                    06  W06-CHK04-VALUE   PIC X.
                    06  W06-CHK04-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK05.
                    06  W06-CHK05-VALUE   PIC X.
                    06  W06-CHK05-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK02.
                    06  W06-CHK02-VALUE   PIC X.
                    06  W06-CHK02-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK06.
                    06  W06-CHK06-VALUE   PIC X.
                    06  W06-CHK06-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK07.
                    06  W06-CHK07-VALUE   PIC X.
                    06  W06-CHK07-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK08.
                    06  W06-CHK08-VALUE   PIC X.
                    06  W06-CHK08-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK09.
                    06  W06-CHK09-VALUE   PIC X.
                    06  W06-CHK09-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK27.
                    06  W06-CHK27-VALUE   PIC X.
                    06  W06-CHK27-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK28.
                    06  W06-CHK28-VALUE   PIC X.
                    06  W06-CHK28-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK29.
                    06  W06-CHK29-VALUE   PIC X.
                    06  W06-CHK29-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK30.
                    06  W06-CHK30-VALUE   PIC X.
                    06  W06-CHK30-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK31.
                    06  W06-CHK31-VALUE   PIC X.
                    06  W06-CHK31-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK32.
                    06  W06-CHK32-VALUE   PIC X.
                    06  W06-CHK32-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK12.
                    06  W06-CHK12-VALUE   PIC X.
                    06  W06-CHK12-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK13.
                    06  W06-CHK13-VALUE   PIC X.
                    06  W06-CHK13-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK15.
                    06  W06-CHK15-VALUE   PIC X.
                    06  W06-CHK15-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK16.
                    06  W06-CHK16-VALUE   PIC X.
                    06  W06-CHK16-STATE   PIC S9(9)   BINARY.
                    06  W06-CHK16-LABEL   PIC X(20).
                  05  W06-CHK17.
                    06  W06-CHK17-VALUE   PIC X.
                    06  W06-CHK17-STATE   PIC S9(9)   BINARY.
                    06  W06-CHK17-LABEL   PIC X(20).
                  05  W06-CHK26.
                    06  W06-CHK26-VALUE   PIC X.
                    06  W06-CHK26-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK25.
                    06  W06-CHK25-VALUE   PIC X.
                    06  W06-CHK25-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK21.
                    06  W06-CHK21-VALUE   PIC X.
                    06  W06-CHK21-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK22.
                    06  W06-CHK22-VALUE   PIC X.
                    06  W06-CHK22-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK23.
                    06  W06-CHK23-VALUE   PIC X.
                    06  W06-CHK23-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK34.
                    06  W06-CHK34-VALUE   PIC X.
                    06  W06-CHK34-STATE   PIC S9(9)   BINARY.
                  05  W06-CHK35.
                    06  W06-CHK35-VALUE   PIC X.
                    06  W06-CHK35-STATE   PIC S9(9)   BINARY.
                  05  W06-KKANRIAUTHFLG-COMB.
                    06  W06-KANRIAUTHFLG.
                        07  W06-VALUE   PIC X(12).
                    06  W06-KANRIAUTHFLG-STATE   PIC S9(9)   BINARY.
                    06  W06-KANRIAUTHFLG-COUNT   PIC S9(9)   BINARY.
                    06  W06-KANRIAUTHFLG-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W06-ORCA21FLG-COMB.
                    06  W06-ORCA21FLG.
                      07  W06-VALUE   PIC X(12).
                    06  W06-ORCA21FLG-COUNT   PIC S9(9)   BINARY.
                    06  W06-ORCA21FLG-ITEM
                            PIC X(12)   OCCURS  2 TIMES.
                  05  W06-HYOJIFLG-COMB.
                    06  W06-HYOJIFLG.
                      07  W06-VALUE   PIC X(10).
                    06  W06-HYOJIFLG-COUNT   PIC S9(9)   BINARY.
                    06  W06-HYOJIFLG-ITEM
                            PIC X(10)   OCCURS  2 TIMES.
                04  W06-TG2.
                  05  W06-SKYPRTFLG-COMB.
                    06  W06-SKYPRTFLG.
                      07  W06-VALUE   PIC X(32).
                    06  W06-SKYPRTFLG-COUNT   PIC S9(9)   BINARY.
                    06  W06-SKYPRTFLG-ITEM
                            PIC X(32)   OCCURS  9 TIMES.
                  05  W06-SYOHOPRTFLG-COMB.
                    06  W06-SYOHOPRTFLG.
                      07  W06-VALUE   PIC X(30).
                    06  W06-SYOHOPRTFLG-COUNT   PIC S9(9)   BINARY.
                    06  W06-SYOHOPRTFLG-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W06-YAKUJYOFLG-COMB.
                    06  W06-YAKUJYOFLG.
                      07  W06-VALUE   PIC X(30).
                    06  W06-YAKUJYOFLG-COUNT   PIC S9(9)   BINARY.
                    06  W06-YAKUJYOFLG-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W06-MEIPRTFLG-COMB.
                    06  W06-MEIPRTFLG.
                      07  W06-VALUE   PIC X(32).
                    06  W06-MEIPRTFLG-COUNT   PIC S9(9)   BINARY.
                    06  W06-MEIPRTFLG-ITEM
                            PIC X(32)   OCCURS  9 TIMES.
                  05  W06-FORMALFLG-COMB.
                    06  W06-FORMALFLG.
                      07  W06-VALUE   PIC X(40).
                    06  W06-FORMALFLG-COUNT   PIC S9(9)   BINARY.
                    06  W06-FORMALFLG-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W06-CLIENT-SAVE-COMB.
                    06  W06-CLIENT-SAVE.
                      07  W06-VALUE   PIC X(40).
                    06  W06-CLIENT-SAVE-COUNT   PIC S9(9)   BINARY.
                    06  W06-CLIENT-SAVE-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W06-USER-KANRI-COMB.
                    06  W06-USER-KANRI.
                      07  W06-VALUE   PIC X(40).
                    06  W06-USER-KANRI-STATE   PIC S9(9)   BINARY.
                    06  W06-USER-KANRI-COUNT   PIC S9(9)   BINARY.
                    06  W06-USER-KANRI-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                  05  W06-PRTCONF-COMB.
                    06  W06-PRTCONF.
                      07  W06-VALUE   PIC X(40).
                    06  W06-PRTCONF-STATE   PIC S9(9)   BINARY.
                    06  W06-PRTCONF-COUNT   PIC S9(9)   BINARY.
                    06  W06-PRTCONF-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W06-CLIENT-PRT-COMB.
                    06  W06-CLIENT-PRT.
                      07  W06-VALUE   PIC X(40).
                    06  W06-CLIENT-PRT-STATE   PIC S9(9)   BINARY.
                    06  W06-CLIENT-PRT-COUNT   PIC S9(9)   BINARY.
                    06  W06-CLIENT-PRT-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W06-DIALOGFLG-COMB.
                    06  W06-DIALOGFLG.
                      07  W06-VALUE   PIC X(40).
                    06  W06-DIALOGFLG-COUNT    PIC S9(9)   BINARY.
                    06  W06-DIALOGFLG-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W06-ICCARDRW-COMB.
                    06  W06-ICCARDRW.
                      07  W06-VALUE   PIC X(40).
                    06  W06-ICCARDRW-COUNT    PIC S9(9)   BINARY.
                    06  W06-ICCARDRW-ITEM
                            PIC X(40)   OCCURS  2 TIMES.
                  05  W06-GLINECNT.
                    06  W06-VALUE   PIC S9(2).
                  05  W06-NLINECNT.
                    06  W06-VALUE   PIC S9(2).
                04  W06-TGN3-STATE       PIC S9(9)   BINARY.
                04  W06-TG3.
                  05   W06-GRPHOSPNUM  PIC X(02)   OCCURS  9 TIMES.
                  05   W06-GRPHOSPNAME PIC X(10)   OCCURS  9 TIMES.
                  05   W06-GRPUSERID   PIC X(16)   OCCURS  9 TIMES.
              03  W06-STYUKYMD.
                04  W06-VALUE   PIC X(10).
                04  W06-STYUKYMD-STATE   PIC S9(9)   BINARY.
              03  W06-EDYUKYMD.
                04  W06-VALUE   PIC X(10).
                04  W06-EDYUKYMD-STATE   PIC S9(9)   BINARY.
              03  W06-USERID.
                04  W06-VALUE   PIC X(16).
                04  W06-USERID-STATE   PIC S9(9)   BINARY.
              03  W06-PASSWORD1.
                04  W06-VALUE   PIC X(16).
              03  W06-PASSWORD2.
                04  W06-VALUE   PIC X(16).
              03  W06-KANANAME.
                04  W06-VALUE   PIC X(80).
              03  W06-NAME.
                04  W06-VALUE   PIC X(80).
              03  W06-SELNUM.
                04  W06-VALUE   PIC S9(2).
              03  W06-SYOKUKBN-COMB.
                04  W06-SYOKUKBN.
                  05  W06-VALUE   PIC X(20).
                04  W06-SYOKUKBN-STATE   PIC S9(9)   BINARY.
                04  W06-SYOKUKBN-COUNT   PIC S9(9)   BINARY.
                04  W06-SYOKUKBN-ITEM
                        PIC X(20)   OCCURS  10 TIMES.
              03  W06-SYOKUCOD-COMB.
                04  W06-SYOKUCOD.
                  05  W06-VALUE   PIC X(35).
                04  W06-SYOKUCOD-STATE   PIC S9(9)   BINARY.
                04  W06-SYOKUCOD-COUNT   PIC S9(9)   BINARY.
                04  W06-SYOKUCOD-ITEM
                        PIC X(35)   OCCURS  100 TIMES.
              03  W06-SCROLLEDWINDOW1.
                04  W06-KIKAN-LIST.
                  05  W06-KIKAN-COUNT   PIC S9(9)   BINARY.
                  05  W06-KIKAN-ROW PIC S9(9)   BINARY.
                  05  W06-KIKAN-ROWATTR PIC S9(9)   BINARY.
                  05  W06-ITEM        OCCURS  10 TIMES.
                    06  W06-TNO   PIC X(3).
                    06  W06-TSTYUKYMD  PIC X(10).
                    06  W06-TEDYUKYMD  PIC X(10).
                  05  W06-KIKAN-SELECT
                          PIC X       OCCURS  10 TIMES.
              03  W06-B03.
                04  W06-B03-STATE   PIC S9(9)   BINARY.
              03  W06-B05.
                04  W06-B05-STATE   PIC S9(9)   BINARY.
                04  W06-B05-LABEL   PIC X(10).
