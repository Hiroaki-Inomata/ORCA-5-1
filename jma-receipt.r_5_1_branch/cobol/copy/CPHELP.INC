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
      *    �إ�ץޥ���                  COPY (CPHELP.INC)       *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  HELP-REC.
           03  HELP-KEY.
               05  HELP-HOSPNUM          PIC 9(2).
      *�إ�׶�ʬ�ʣ� ɸ�ࡢ�� ������
               05  HELP-HELPKBN           PIC  X(01).
      *���ڥ졼���ɣ�
               05  HELP-KEY-OPID          PIC  X(16).
      *�̣�̾
               05  HELP-LDNAME            PIC  X(20).
      *�ܼ���
               05  HELP-MENUNUM1          PIC  9(04).
      *�ܼ���
               05  HELP-MENUNUM2          PIC  9(04).
      *����
               05  HELP-EDANUM            PIC  9(04).
      *�ܼ����ե饰�ʣ����ܼ��������
           03  HELP-MENU2FLG              PIC  9(01).
      *�ܼ�̾��
           03  HELP-MENU                  PIC  X(60).
      *�إ������
           03  HELP-HELP                  PIC  X(100).
      *ü���ɣ�
           03  HELP-TERMID                PIC  X(64).
      *���ڡ��졼���ɣ�
           03  HELP-OPID                  PIC  X(16).
      *����ǯ����
           03  HELP-CREYMD                PIC  X(08).
      *����ǯ����
           03  HELP-UPYMD                 PIC  X(08).
      *��������
           03  HELP-UPHMS                 PIC  X(06).
      *