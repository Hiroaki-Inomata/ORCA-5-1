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
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       01  SYUPARA01-AREA.
           03  SYUPARA01-NYUGAIKBN         PIC X(01).
           03  SYUPARA01-SEIKYU            PIC S9(10).
           03  SYUPARA01-SEIKYU-TAX-SAI    PIC S9(10).
           03  SYUPARA01-NYUKIN            PIC S9(10).
           03  SYUPARA01-ZENMISYU          PIC S9(10).
           03  SYUPARA01-ZENMISYU-2        PIC S9(10).
           03  SYUPARA01-HENKIN            PIC S9(10).
           03  SYUPARA01-KANYUKIN          PIC S9(10).
           03  SYUPARA01-NYU-MISYU         PIC S9(10).
           03  SYUPARA01-GAI-MISYU         PIC S9(10).
           03  SYUPARA01-GAI-MISYU-P       PIC S9(10).
           03  SYUPARA01-GAI-MISYU-M       PIC S9(10).
           03  SYUPARA01-NYU-NYUKIN        PIC S9(10).
           03  SYUPARA01-GAI-NYUKIN        PIC S9(10).
      *    ȯ�ԥե饰�ʣ���������������ȯ�ԡ������������������� �������̡�
           03  SYUPARA01-HAKKOFLG          PIC 9(01).
           03  SYUPARA01-SKYSUMFLG         PIC 9(01).
      *    ����ۥ����߷��������ȯ�Զ�ʬ�ʣ���ȯ�Ԥ��롢����ȯ�Ԥ��ʤ���
           03  SYUPARA01-ZEROSKYKBN        PIC X(01).
           03  SYUPARA01-DENPPRTYMD        PIC X(08).
      *    ������������������ǡ������׶�ʬ
      *    �ʣ������פ˴ޤ�롢�������פ˴ޤ�ʤ���
           03  SYUPARA01-TUKISUMKBN        PIC X(01).
      *    �����������ֶ�ʬ
      *    �ʣ������ֻ��ꤹ���
           03  SYUPARA01-TUKIKIKANKBN      PIC X(01).
      *    �����������ֳ�����
           03  SYUPARA01-TUKISTYMD         PIC X(08).
      *    �����������ֽ�λ��
           03  SYUPARA01-TUKIEDYMD         PIC X(08).
      *    �������
           03  SYUPARA01-CNT               PIC 9(05).
      *    �����ǡ��������ե饰
           03  SYUPARA01-UPDFLG            PIC 9(01).