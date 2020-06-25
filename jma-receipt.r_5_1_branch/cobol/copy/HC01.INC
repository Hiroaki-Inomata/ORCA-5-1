      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.04.11  �ݸ���ʬ����̾���ѹ�
      *                                     ��̾������Υơ��֥벽
      *  01.00.04    MCC-ƣ��     01.05.20  �����Խ����ѹ�
      *  01.00.05    MCC-¿��Ǽ   02.01.15  �쥤�������ѹ�
      ************************************************************
       01  HC01.
              03  HC01-WORK    PIC  X(16).
              03  HC01-TUZUKI    PIC  X(8).
              03  HC01-HPTNUM    PIC  X(16).
              03  HC01-KIGONUM    PIC  X(32).
              03  HC01-HIHKNJNAME    PIC  X(22).
              03  HC01-NAME    PIC  X(24).
              03  HC01-PTNUM    PIC  X(20).
              03  HC01-JKOH-TBL        OCCURS  3.
                  05  HC01-JKYSNO      PIC  X(14).
              03  HC01-FKOH-TBL        OCCURS  3.
                  05  HC01-FTNSNO      PIC  X(16).
              03  HC01-OTOKO    PIC  X(2).
              03  HC01-ONNA    PIC  X(2).
              03  HC01-TENKI-TBL    OCCURS  8.
                  05  HC01-TENKIG.
                      07 HC01-TENKI     OCCURS  3  TIMES
                                        PIC  X(02).
              03  HC01-STRYMD-TBL   OCCURS  8.
                  05  HC01-STRYMDG.
                      07 HC01-STRYMD     OCCURS  3  TIMES
                                         PIC  X(02).
              03  HC01-ENDYMD-TBL   OCCURS  8.
                  05  HC01-ENDYMDG.
                      07 HC01-ENDYMD    OCCURS  3  TIMES
                                        PIC  X(02).
              03  HC01-TANJYOG    PIC  X(4).
              03  HC01-TANJYOY    PIC  X(2).
              03  HC01-TANJYOM    PIC  X(2).
              03  HC01-TANJYOD    PIC  X(2).
              03  HC01-YUKOG    PIC  X(4).
              03  HC01-YUKOY    PIC  X(2).
              03  HC01-YUKOM    PIC  X(2).
              03  HC01-YUKOD    PIC  X(2).
              03  HC01-SIKAKUG    PIC  X(4).
              03  HC01-SIKAKUY    PIC  X(2).
              03  HC01-SIKAKUM    PIC  X(2).
              03  HC01-SIKAKUD    PIC  X(2).
              03  HC01-ADRS    PIC  X(64).
              03  HC01-ADRSTEL    PIC  X(20).
              03  HC01-KINADRS    PIC  X(30).
              03  HC01-KINNAME    PIC  X(30).
              03  HC01-KINADRSTEL    PIC  X(20).
              03  HC01-HKNADRS    PIC  X(30).
              03  HC01-HKNNAME    PIC  X(30).
              03  HC01-HKNADRSTEL    PIC  X(20).
              03  HC01-BYOMEI-TBL   OCCURS  8.
                  05  HC01-BYOMEIG.
                      07 HC01-BYOMEI       PIC  X(112).
              03  HC01-JKOHN-TBL       OCCURS  3.
                  05  HC01-JKYSNON     PIC  X(20).
              03  HC01-FKOHN-TBL       OCCURS  3.
                  05  HC01-FTNSNON     PIC  X(20).