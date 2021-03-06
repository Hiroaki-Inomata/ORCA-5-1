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
      *----------------------------------------------------------*
      *    �������������ٽ���ϥѥ�᥿      COPY (CPORCHCN04.INC)
      *                                          (ORCHCN04 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCHCN04AREA.
      *
      *    ���ԣɣ�
           03  ORCHCN04-PTID                   PIC 9(10).
      *    �����ֹ�
           03  ORCHCN04-PTNUM                  PIC X(20).
      *    ���Ի�̾
           03  ORCHCN04-NAME                   PIC X(50).
      *    ȯ�Զ�ʬ (1:��ȯ�� 3:��Ǽ�����ǧ����)
           03  ORCHCN04-HAKKOFLG               PIC 9(01).
      *    �������
           03  ORCHCN04-SKYSTYMD               PIC X(08).
           03  ORCHCN04-SKYEDYMD               PIC X(08).
      *
           03  ORCHCN04-INPUT-OCC        OCCURS   10.
      *    ��ɼ�ֹ��ɬ�ܡ�
               05  ORCHCN04-DENPNUM        PIC 9(07).
      *    ��ɼ�ֹ���֡ʼ�Ǽ����κ�ȯ�ԡ�
               05  ORCHCN04-DENPEDANUM     PIC  9(02).
      *
      *    ��ɼȯ����(�����������ؼ����̤����
           03  ORCHCN04-DENPPRTYMD         PIC  X(08).
      *    ȯ�ԥڡ�����
           03  ORCHCN04-PAGE               PIC  9(05).
      *    ���ڡ������ʰ���ե�����̾�Խ��ѡ�
           03  ORCHCN04-PAGE-CNT           PIC  9(06).
      *
      *    �����ģ½��϶�ʬ(1:�����ģ¤˽��ϡ�
           03  ORCHCN04-PRTKBN             PIC X(01).
      *    ���ѥ�᥿
           03  ORCHCN04-PRTAREA            PIC X(25000).
