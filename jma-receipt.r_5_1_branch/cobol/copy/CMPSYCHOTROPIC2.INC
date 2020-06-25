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
      *-----------------------------------------------------------------*
      *    ��������Ĺ��������ʬ�����ɥơ��֥� COPY (CMPSYCHOTROPIC2.INC)
      *-----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-��ƣ    19/01/31  ��������Ĺ���������о�����
      *****************************************************************
      *
       01  TBL-PSYCHO2-CODE-AREA.
      *
      * ���԰���
      * 01
      *    �����������
           03  FILLER  PIC X(35)   VALUE
               "1124013 00000000 99999999 1124013".
      *    �������������
           03  FILLER  PIC X(35)   VALUE
               "1124014 00000000 99999999 1124014".
      *    �����饼�׻��󥫥ꥦ��
           03  FILLER  PIC X(35)   VALUE
               "1124015 00000000 99999999 1124015".
      *    �������ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124017 00000000 99999999 1124017".
           03  FILLER  PIC X(35)   VALUE
               "1124701 00000000 99999999 1124017".
      *    �ե른�����ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124019 00000000 99999999 1124019".
      *    �֥��ޥ��ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124020 00000000 99999999 1124020".
           03  FILLER  PIC X(35)   VALUE
               "1124700 00000000 99999999 1124020".
      *    ������ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124021 00000000 99999999 1124021".
      *    ���饼�ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124022 00000000 99999999 1124022".
      * 11
      *    ����ץ饾���
           03  FILLER  PIC X(35)   VALUE
               "1124023 00000000 99999999 1124023".
      *    �ե륿�����
           03  FILLER  PIC X(35)   VALUE
               "1124024 00000000 99999999 1124024".
      *    �ᥭ�������
           03  FILLER  PIC X(35)   VALUE
               "1124025 00000000 99999999 1124025".
      *    �ե�ȥץ饼�ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124027 00000000 99999999 1124027".
      *    �����른�����ݥ�����
           03  FILLER  PIC X(35)   VALUE
               "1124028 00000000 99999999 1124028".
      *    ���ե饼�׻�������
           03  FILLER  PIC X(35)   VALUE
               "1124029 00000000 99999999 1124029".
      *    �����������ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1179012 00000000 99999999 1179012".
      *    ���������
           03  FILLER  PIC X(35)   VALUE
               "1179025 00000000 99999999 1179025".
      * ��̲��
      *    �����������
           03  FILLER  PIC X(35)   VALUE
               "1124001 00000000 99999999 1124001".
      *    �ե�饼�ѥ������
           03  FILLER  PIC X(35)   VALUE
               "1124002 00000000 99999999 1124002".
      * 21
      *    �˥ȥ饼�ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124003 00000000 99999999 1124003".
      *    �˥᥿���ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124004 00000000 99999999 1124004".
      *    �ϥ����������
           03  FILLER  PIC X(35)   VALUE
               "1124005 00000000 99999999 1124005".
      *    �ȥꥢ�����
           03  FILLER  PIC X(35)   VALUE
               "1124007 00000000 99999999 1124007".
      *    �ե�˥ȥ饼�ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124008 00000000 99999999 1124008".
      *    �֥��������
           03  FILLER  PIC X(35)   VALUE
               "1124009 00000000 99999999 1124009".
      *    ����᥿���ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124010 00000000 99999999 1124010".
      *    �������ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1124030 00000000 99999999 1124030".
      *    ���ޥ��ۥ����������ʪ
           03  FILLER  PIC X(35)   VALUE
               "1129006 00000000 99999999 1129006".
      *    ���ԥ�����
           03  FILLER  PIC X(35)   VALUE
               "1129007 00000000 99999999 1129007".
      * 31
      *    ����ԥǥ���л���
           03  FILLER  PIC X(35)   VALUE
               "1129009 00000000 99999999 1129009".
      *    �������ԥ�����
           03  FILLER  PIC X(35)   VALUE
               "1129010 00000000 99999999 1129010".
      * ���Ƥ󤫤���
      *    �����ʥ��ѥ�
           03  FILLER  PIC X(35)   VALUE
               "1139003 00000000 99999999 1139003".
      *    �����Х���
           03  FILLER  PIC X(35)   VALUE
               "1139006 00000000 99999999 1139006".
      * ���Ƴ������������
      *    �ߥ����������͡�
      **   03  FILLER  PIC X(35)   VALUE
      **       "1124401 00000000 99999999 1124401".
      **   03  FILLER  PIC X(35)   VALUE
      **       "1139401 00000000 99999999 1124401".
      *
       01  TBL-PSYCHO2-CODE-AREA-R REDEFINES   TBL-PSYCHO2-CODE-AREA.
           03  TBL-PSYCHO2-OCC     OCCURS  34  INDEXED   TBL-PSY2.
               05  TBL-PSYCHO2-CODE        PIC X(07).
               05  TBL-PSYCHO2-YOBI1       PIC X(01).
               05  TBL-PSYCHO2-STYMD       PIC X(08).
               05  TBL-PSYCHO2-YOBI2       PIC X(01).
               05  TBL-PSYCHO2-EDYMD       PIC X(08).
               05  TBL-PSYCHO2-YOBI3       PIC X(01).
               05  TBL-PSYCHO2-GROUP       PIC X(07).
               05  TBL-PSYCHO2-YOBI        PIC X(02).
      *