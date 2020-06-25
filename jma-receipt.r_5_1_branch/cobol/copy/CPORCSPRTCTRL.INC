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
      *    ���饤����Ȱ����������                              *
      *                         COPY  (CPORCSPRTCTRL.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSPRTCTRLAREA.
      *�⡼��
           03  ORCSPRTCTRL-MODE          PIC X(01).
      *�Хå�����Ƚ��
           03  ORCSPRTCTRL-BATCH-FLG     PIC 9(01).
      *������
           03  ORCSPRTCTRL-IN            OCCURS   10.
      *    Ģɼ�����ȥ�
             05  ORCSPRTCTRL-TITLE-I     PIC X(256).
      *�ˣţ�
             05  ORCSPRTCTRL-TBL-KEY     PIC X(08).
      *Ϣ��
             05  ORCSPRTCTRL-RENNUM      PIC 9(04).
      *���롼��
             05  ORCSPRTCTRL-TBL-GROUP   PIC X(14).
      *UUID
             05  ORCSPRTCTRL-TBL-UUID    PIC X(36).
      *������Ϣ��
             05  ORCSPRTCTRL-SHORI-RENNUM
                                         PIC 9(04).
      *������
             05  ORCSPRTCTRL-STPAGE
                                         PIC 9(05).
      *��λ��
             05  ORCSPRTCTRL-EDPAGE
                                         PIC 9(05).
      *
      *������
           03  ORCSPRTCTRL-OUT.
             05  ORCSPRTCTRL-O           OCCURS   10.
      *    Ģɼ�ѥ�
               07  ORCSPRTCTRL-PATH-O    PIC X(256).
      *    Ģɼ�����ȥ�
               07  ORCSPRTCTRL-TITLE-O   PIC X(256).
      *    ɽ��̵ͭ�ե饰
               07  ORCSPRTCTRL-SHOWDIALOG  PIC S9(9)   BINARY.
      *    ��ȥ饤���
               07  ORCSPRTCTRL-NRETRY   PIC S9(9)   BINARY.
      *
      *