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
      *    ���������ٽ���ϥѥ�᥿       COPY (CPORCHC04.INC)
      *                                          (ORCHC04 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCHC04AREA.
      *
      *    ��ɼ�ֹ��ɬ�ܡ�
           03  ORCHC04-DENPNUM          PIC 9(07).
      *
      *    ȯ�Զ�ʬ (2:����3:��Ǽ�����ǧ����)
           03  ORCHC04-HAKKOFLG        PIC 9(01).
      *    ȯ����ˡ��1:�ʡ��ݸ��̡�2:�ݸ��̡�3:���̡�4:����)
      *             (0:��ɼ�ֹ�ʬ�Τ�)
           03  ORCHC04-HAKHOUFLG        PIC 9(01).
      *    ��ɼ�ֹ���֡ʼ�Ǽ����κ�ȯ�ԡ�
           03  ORCHC04-DENPEDANUM      PIC  9(02).
      *    ��ɼȯ����(�����������μ����
           03  ORCHC04-DENPPRTYMD      PIC  X(08).
      *    �о���ɼ�ֹ�ʴ��ֻ����
           03  ORCHC04-KIKAN-AREA.
               05  ORCHC04-KIKAN-STYMD     PIC X(08).
               05  ORCHC04-KIKAN-EDYMD     PIC X(08).
               05  ORCHC04-KIKAN-DENPNUM-G.
                   07  ORCHC04-KIKAN-DENPNUM   PIC 9(07)
                                            OCCURS   100.
      *    ȯ�ԥڡ�����
           03  ORCHC04-PAGE            PIC  9(05).
      *
      *    ���ڡ������ʰ���ե�����̾�Խ��ѡ�
           03  ORCHC04-PAGE-CNT        PIC  9(06).
      *
      *    �����ģ½��϶�ʬ(1:�����ģ¤˽��ϡ�
           03  ORCHC04-PRTKBN          PIC X(01).
      *    ���ѥ�᥿
           03  ORCHC04-PRTAREA         PIC X(25000).
