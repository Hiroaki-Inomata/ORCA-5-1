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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɴ����ݸ��������ʬ̾���Խ����֥ѥ�᥿
      *                                         (CPORAPI12SUB1.INC)
      ******************************************************************
       01  ORAPI012SUB1AREA.
      *    �ݸ������ֹ�
           03  API012SUB1-HKNNUM        PIC X(03).
      *    �����ʬ
           03  API012SUB1-HOJOKBN       PIC X(01).
      *    �������ա�̤���ѡ�
           03  API012SUB1-SYSYMD        PIC X(08).
      *
      *�ֵ�
           03  API012SUB1-OUT-AREA.
      *        �����ʬ̾��
               05  API012SUB1-HOJOKBN-NAME  PIC X(20).
      *        ��ô���ʹ���ԡ�
               05  API012SUB1-RATE          PIC 9(03).
      *        ���顼��ʬ�ʣ������顼��
               05  API012SUB1-ERRCD         PIC 9(01).
      *
      *
