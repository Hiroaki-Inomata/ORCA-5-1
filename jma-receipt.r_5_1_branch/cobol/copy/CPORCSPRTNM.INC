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
      *    ����饤��Ģɼ̾��������ץ��̾�����ѥ�᥿        *
      *                         COPY  (CPORCSPRTNM.INC)         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSPRTNMAREA.
      *������
      *    �����ʣ��� ����饤��Ģɼ̾����
      *          ����������ץ��̾������
           03  ORCSPRTNM-KBN           PIC X(01).
      *    Ģɼ�ʽ�����ʬ���� ɸ��Ģɼ��ʬ̾��
           03  ORCSPRTNM-KBNCD         PIC X(08).
      *    ������ʶ�����������ƥ����ա�
           03  ORCSPRTNM-SRYYMD        PIC X(08).
      *    ������ʬ�ʶ�������ӣУ���
           03  ORCSPRTNM-NYUGAIKBN     PIC X(01).
      *
      *������
      *    Ģɼ̾�ʽ�����ʬ�����λ��Τߡ�
           03  ORCSPRTNM-PRTPG         PIC X(20).
      *    ����������
           03  ORCSPRTNM-RC            PIC 9(02).
      *
      *